import abc
import asyncio
from concurrent.futures import ThreadPoolExecutor
from flow.domain.enums import NodeStatus, FlowRuleType, FlowStatus
from flow.domain.node_mgr import NodeMgr
from flow.models import Flow_Instance
from flow.repositories import FlowStatusRuleDBHelper, FlowResultRuleDBHelper, NodeInstanceDBHelper


def run_node_list(flow_instance: Flow_Instance) -> Flow_Instance:
    # 查询node_instance,并排序
    node_inst_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance.pk})
    node_inst_list = sorted(node_inst_list, key=lambda item: item.node_order, reverse=False)
    # 按顺序执行node
    for node_inst in node_inst_list:
        node_mgr = NodeMgr().run_node_instance(node_instance=node_inst, flow_data=flow_instance.flow_data)
        # 运行完node以后把返回的return_data放进flow_data
        data = flow_instance.flow_data
        data.update(node_mgr.return_data)
        flow_instance.flow_data = data
        # 判断 node 运行状态,是 Stop 和 Cancelled 就退出循环
        if node_mgr.node_instance.node_status in [NodeStatus.Cancelled.value, NodeStatus.Stop.value]:
            return flow_instance
        else:
            # 其他任何情况都继续执行
            continue
    return flow_instance


def update_result_and_status(flow_instance: Flow_Instance) -> Flow_Instance:
    flow_instance.flow_result = check_flow_result(flow_instance)
    flow_status = check_flow_status(flow_instance)
    if flow_status:
        flow_instance.flow_status = flow_status
    else:
        pass
    return flow_instance


def get_last_node_inst_attr(flow_instance: Flow_Instance, attr_name):
    # 使用最后一个运行过的节点结果
    node_inst_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance.pk})
    node_inst_list = sorted(node_inst_list, key=lambda item: item.node_order, reverse=True)
    for node_inst in node_inst_list:
        # 运行过的 node_instance 才检查
        if node_inst.node_status != NodeStatus.Pending.value:
            return node_inst.__getattribute__(attr_name)
        else:
            continue
    return None


def check_flow_result(flow_instance: Flow_Instance):
    # 查出对应的 flow_result_rules
    result_rule = FlowResultRuleDBHelper().get_by({'pk': flow_instance.flow_design.fw_result_rule})
    if result_rule.rule_type == FlowRuleType.Default.value:
        return get_last_node_inst_attr(flow_instance, 'node_result')
    elif result_rule.rule_type == FlowRuleType.Script.value:
        # todo
        return None
    else:
        raise Exception(f'无法识别的result_rule_type = {result_rule.rule_type}')


def check_flow_status(flow_instance: Flow_Instance):
    status_rule = FlowStatusRuleDBHelper().get_by({'pk': flow_instance.flow_design.fw_status_rule})
    if status_rule.rule_type == FlowRuleType.Default.value:
        return get_last_node_inst_attr(flow_instance, 'node_status')
    elif status_rule.rule_type == FlowRuleType.Script.value:
        # todo
        return None
    else:
        raise Exception(f'无法识别的 status_rule_type = {status_rule.rule_type}')


class SerialFlowRunnerResult:
    def __init__(self, flow_instance):
        self.flow_instance = flow_instance


class SerialFlowRunner:

    @staticmethod
    def run(flow_instance: Flow_Instance) -> SerialFlowRunnerResult:
        flow_instance = run_node_list(flow_instance)
        if flow_instance.flow_status == FlowStatus.Pending.value:
            flow_instance.flow_status = FlowStatus.Running.value
        flow_instance = update_result_and_status(flow_instance)
        return SerialFlowRunnerResult(flow_instance)

# class FlowParallelRunner(FlowRunner):
#
#     def run(self, flow_instance: Flow_Instance):
#         self.flow_instance = flow_instance
#         while self._run():
#             self._run()
#         self._update_result_and_status()
#         return self
#
#     def _run(self):
#         # todo
#         # orm不支持异步,sqlite3不支持多线程
#         # 确认哪些节点可以运行
#         node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': self.flow_instance.id}).order_by(
#             'node_order')
#         nodes = []
#         for node_instance_ in node_instance_list:
#             if NodeMgr().check_node_start_rule(node_instance_, self.flow_instance.flow_data):
#                 print(f'节点可以运行 node_instance_id = {node_instance_.id}')
#                 nodes.append(node_instance_)
#             else:
#                 continue
#         if len(nodes) == 0:
#             print(f'没有节点可以运行 flow_instance = {self.flow_instance.id}')
#             return False
#         else:
#             # 事件循环
#             loop = asyncio.get_event_loop()
#             # 线程池
#             pool = ThreadPoolExecutor()
#             tasks = []
#             for node in nodes:
#                 tasks.append(
#                     loop.run_in_executor(pool, NodeMgr().run_node_instance, node, self.flow_instance.flow_data))
#             # 等待所有任务结束并返回
#             ttt = asyncio.gather(*tasks)
#             loop.run_until_complete(ttt)
#             for task in tasks:
#                 print(task)
#             self._update_result_and_status()
#             return True
#
#     def _update_result_and_status(self):
#         self.flow_instance.flow_result = self._check_result_rule()
#         self.flow_instance.flow_status = self._check_status_rule()
