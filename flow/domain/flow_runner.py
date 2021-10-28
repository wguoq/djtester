from flow.domain.enums import NodeStatus, FlowType, FlowStatus
from flow.domain.node_mgr import NodeMgr
from flow.models import Flow_Instance, Flow_Result_Rule, Node_Instance, Flow_Status_Rule
from flow.repositories import FlowStatusRuleDBHelper, FlowResultRuleDBHelper, NodeInstanceDBHelper


class FlowInstanceRunner:
    def __init__(self):
        self.new_flow_instance = None

    def run(self, flow_instance: Flow_Instance):
        result = self._run(flow_instance)
        self.new_flow_instance = self._update_result_status(result, flow_instance)
        return self

    def _run(self, flow_instance: Flow_Instance):
        flow_type = flow_instance.flow_design.flow_type
        if flow_type == FlowType.Serial.value:
            return self._run_serial(flow_instance)
        elif flow_type == FlowType.Parallel.value:
            return self._run_parallel(flow_instance)
        else:
            raise Exception(f'无法识别的 flow_type = {flow_type},serial=串行;parallel=并行')

    @staticmethod
    def _run_serial(flow_instance: Flow_Instance):
        flow_data = flow_instance.flow_data
        if flow_data is None:
            flow_data = {}
        # 查询node_instance,并排序
        node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance.id}).order_by(
            'node_order')
        # 按顺序执行node
        for node_instance in node_instance_list:
            node_result: NodeMgr = NodeMgr().run_node_instance(node_instance=node_instance, flow_data=flow_data)
            # 只有node的状态是finish或者skip时才继续执行后面的
            if node_result.new_node_status in [NodeStatus.Finish.value, NodeStatus.Skip.value]:
                # 把return_data添加到flow_data里
                flow_data.update(node_result.return_data)
            else:
                return 0
        return 1

    @staticmethod
    def _run_parallel(flow_instance):
        # todo
        return False

    def _update_result_status(self, result, flow_instance):
        # 0就表示串行的节点没有运行完,使用默认结果
        if result == 0:
            flow_instance.flow_status = FlowStatus.Running.value
            return flow_instance
        # 1表示串行流程的节点都运行完了,需要根据规则来确定结果
        elif result == 1:
            flow_instance.flow_result = self._check_result(flow_instance)
            flow_instance.flow_status = self._check_status(flow_instance)
            return flow_instance

    def _check_result(self, flow_instance):
        result = None
        # 查出对应的 flow_result_rules 可以是多条,约定为or关系
        result_rules = FlowResultRuleDBHelper().filter_by({'flow_design_id': flow_instance.flow_design_id})
        for result_rule in result_rules:
            result = self._get_flow_result_by_rule(result_rule, flow_instance)
            if result:
                return result
            else:
                continue
        if result is None:
            return None

    @staticmethod
    def _get_flow_result_by_rule(result_rule, flow_instance):
        rule: Flow_Result_Rule = result_rule
        if rule.result_rule_type == 'last_node_result':
            # last_node_result为默认使用最后一个节点的结果
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': flow_instance.id}).order_by('-node_order')
            last: Node_Instance = node_instance_list[0]
            return last.node_result
        elif rule.result_rule_type == 'custom':
            # todo 自定义的先放着
            return None

    def _check_status(self, flow_instance):
        status = None
        status_rules = FlowStatusRuleDBHelper().filter_by({'flow_design_id': flow_instance.flow_design_id})
        for status_rule in status_rules:
            status = self._get_flow_status_by_rule(status_rule, flow_instance)
            if status:
                return status
            else:
                continue
        if status is None:
            return FlowStatus.Unknown.value

    @staticmethod
    def _get_flow_status_by_rule(status_rule, flow_instance):
        rule: Flow_Status_Rule = status_rule
        if rule.status_rule_type == 'last_node_status':
            # last_node_status 为默认使用最后一个节点的状态
            node_instance_list = NodeInstanceDBHelper().filter_by(
                {'flow_instance_id': flow_instance.id}).order_by('-node_order')
            last: Node_Instance = node_instance_list[0]
            return last.node_status
        elif rule.status_rule_type == 'custom':
            # todo 自定义的先放着
            return None
