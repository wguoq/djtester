from flow.domain.enums import NodeStatus, FlowType, FlowStatus
from flow.domain.node_mgr import NodeMgr
from flow.models import Flow_Instance, Flow_Result_Rule, Node_Instance, Flow_Status_Rule
from flow.repositories import FlowStatusRuleDBHelper, FlowResultRuleDBHelper, NodeInstanceDBHelper


class FlowInstanceRunner:
    def __init__(self):
        self.flow_instance = None

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
        flow_data = self.flow_instance.flow_data
        if flow_data is None:
            flow_data = {}
        # 查询node_instance,并排序
        node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': self.flow_instance.id}).order_by(
            'node_order')
        # 按顺序执行node
        for node_instance in node_instance_list:
            nodeMgr_result: NodeMgr = NodeMgr().run_node_instance(node_instance=node_instance, flow_data=flow_data)
            # node的状态是finish或者skip时继续执行后面的
            if nodeMgr_result.node_instance.node_status in [NodeStatus.Finish.value, NodeStatus.Skip.value]:
                # 把return_data添加到flow_data里
                flow_data.update(nodeMgr_result.return_data)
            # node的状态是stop时停止执行
            elif nodeMgr_result.node_instance.node_status == NodeStatus.Stop.value:
                self.flow_instance.flow_status = FlowStatus.Stop.value
                self.flow_instance.flow_result = nodeMgr_result.node_instance.node_result
                return -1
            else:
                return 0
        return 1

    @staticmethod
    def _run_parallel():
        # todo
        pass

    def _update_result_and_status(self, result):
        # -1就表示终止
        if result == -1:
            pass
        # 0就表示串行的节点没有运行完,使用默认结果
        elif result == 0:
            self.flow_instance.flow_status = FlowStatus.Running.value
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
            # last_node_result为默认使用最后一个节点的结果
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': self.flow_instance.id}).order_by('-node_order')
            last: Node_Instance = node_instance_list[0]
            return last.node_result
        elif rule.result_rule_type == 'custom':
            # todo 自定义的先放着
            return None

    def _check_status_rule(self):
        status = None
        status_rules = FlowStatusRuleDBHelper().filter_by({'flow_design_id': self.flow_instance.flow_design_id})
        for status_rule in status_rules:
            status = self._get_flow_status_by_rule(status_rule)
            if status:
                return status
            else:
                continue
        if status is None:
            return FlowStatus.Unknown.value

    def _get_flow_status_by_rule(self, status_rule):
        rule: Flow_Status_Rule = status_rule
        if rule.status_rule_type == 'last_node_status':
            # last_node_status 为默认使用最后一个节点的状态
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': self.flow_instance.id}).order_by('-node_order')
            last: Node_Instance = node_instance_list[0]
            return last.node_status
        elif rule.status_rule_type == 'custom':
            # todo 自定义的先放着
            return None
