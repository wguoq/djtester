import asyncio
from concurrent.futures import ThreadPoolExecutor

from flow.domain.enums import NodeStatus, FlowType, FlowStatus
from flow.domain.node_mgr import NodeMgr
from flow.models import Flow_Instance, Flow_Result_Rule, Flow_Status_Rule
from flow.repositories import FlowStatusRuleDBHelper, FlowResultRuleDBHelper, NodeInstanceDBHelper


class FlowInstanceRunner:
    def __init__(self):
        self.flow_instance: Flow_Instance = Flow_Instance()

    def run(self, flow_instance: Flow_Instance):
        self.flow_instance = flow_instance
        result = self._run()
        self._update_result_and_status(result)
        return self

    def _run(self):
        flow_type = self.flow_instance.flow_design.flow_type
        if flow_type == FlowType.Serial.value:
            return self._run_serial()
        elif flow_type == FlowType.Parallel.value:
            return self._run_parallel()
        else:
            raise Exception(f'无法识别的 flow_type = {flow_type},serial=串行;parallel=并行')

    def _run_serial(self):
        self.flow_instance.flow_status = FlowStatus.Running.value
        # 查询node_instance,并排序
        node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': self.flow_instance.id}).order_by(
            'node_order')
        # 按顺序执行node
        for node_instance in node_instance_list:
            node_mgr = NodeMgr().run_node_instance(node_instance=node_instance, flow_data=self.flow_instance.flow_data)
            data = self.flow_instance.flow_data
            data.update(node_mgr.return_data)
            self.flow_instance.flow_data = data
            # 判断 node 运行状态,是 Stop 和 Cancelled 就退出循环
            if node_mgr.node_instance.node_status == NodeStatus.Cancelled.value:
                self.flow_instance.flow_status = FlowStatus.Cancelled.value
                self.flow_instance.flow_result = node_mgr.node_instance.node_result
                return -1
            elif node_mgr.node_instance.node_status == NodeStatus.Stop.value:
                self.flow_instance.flow_status = FlowStatus.Stop.value
                self.flow_instance.flow_result = node_mgr.node_instance.node_result
                return -1
            else:
                # 其他任何情况都继续执行
                continue
        return 1

    def _run_parallel(self):
        # todo
        """
        # orm不支持异步,sqlite3不支持多线程 gg
        # 确认哪些节点可以运行
        # 查询所有节点
        node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': self.flow_instance.id}).order_by(
            'node_order')
        nodes = []
        for node_instance_ in node_instance_list:
            if NodeMgr().check_node_start_rule(node_instance_, self.flow_instance.flow_data):
                print(f'节点可以运行 node_instance_id = {node_instance_.id}')
                nodes.append(node_instance_)
        if len(nodes) == 0:
            print(f'没有节点可以运行 flow_instance = {self.flow_instance.id}')
            return None
        else:
            # 事件循环
            loop = asyncio.get_event_loop()
            # 线程池
            pool = ThreadPoolExecutor()
            tasks = []
            for node in nodes:
                tasks.append(loop.run_in_executor(pool, NodeMgr().only_run_node_instance, node, self.flow_instance.flow_data))
            # 等待所有任务结束并返回
            ttt = asyncio.gather(*tasks)
            loop.run_until_complete(ttt)
        return tasks
        """
        return self._run_serial()

    def _update_result_and_status(self, result):
        # -1就表示流程没运行完
        if result == -1:
            pass
        # 1表示串行流程的节点都运行完了,需要根据规则来确定结果
        elif result == 1:
            self.flow_instance.flow_result = self._check_result_rule()
            self.flow_instance.flow_status = self._check_status_rule()

    def _check_result_rule(self):
        result = None
        # 查出对应的 flow_result_rules 可以是多条,约定为or关系
        result_rule_list = FlowResultRuleDBHelper().filter_by({'flow_design_id': self.flow_instance.flow_design_id})
        for result_rule in result_rule_list:
            result = self._get_flow_result_by_rule(result_rule)
            if result:
                return result
            else:
                continue
        if result is None:
            return None

    def _get_flow_result_by_rule(self, result_rule):
        rule: Flow_Result_Rule = result_rule
        if rule.result_rule_type == 'last_node_result':
            # 使用最后一个有运行状态的节点的结果
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': self.flow_instance.id}).order_by('-node_order')
            for node_instance in node_instance_list:
                if node_instance.node_status != NodeStatus.Pending.value:
                    return node_instance.node_result
                else:
                    continue
            return None
        elif rule.result_rule_type == 'custom':
            # todo 自定义的先放着
            return None
        else:
            raise Exception(f'无法识别的result_rule_type = {rule.result_rule_type}')

    def _check_status_rule(self):
        status = None
        # 查出对应的 flow_status_rules 可以是多条,约定为or关系
        status_rules = FlowStatusRuleDBHelper().filter_by({'flow_design_id': self.flow_instance.flow_design_id})
        for status_rule in status_rules:
            status = self._get_flow_status_by_rule(status_rule)
            if status:
                return status
            else:
                continue
        if status is None:
            return FlowStatus.Running.value

    def _get_flow_status_by_rule(self, status_rule):
        rule: Flow_Status_Rule = status_rule
        if rule.status_rule_type == 'last_node_status':
            #  使用最后一个节点的状态,如果最后一个节点没有运行,则不使用
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': self.flow_instance.id}).order_by('-node_order')
            for node_instance in node_instance_list:
                if node_instance.node_status != NodeStatus.Pending.value:
                    return node_instance.node_status
                else:
                    return None
            return None
        elif rule.status_rule_type == 'custom':
            # todo 自定义的先放着
            return None
