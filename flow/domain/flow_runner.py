from flow.domain.enums import NodeStatus, FlowRuleType, FlowStatus
from flow.domain.node_mgr import NodeMgr
from flow.models import FlowInstance, NodeInstance
from flow.repositories import FlowStatusRuleRepository, FlowResultRuleRepository, NodeInstanceRepository


def run_node_list(flow_instance: FlowInstance) -> FlowInstance:
    if NodeInstanceRepository().count_by({'flow_instance_id': flow_instance.pk}) == 0:
        raise Exception(f'没有查询到 flow_instance_pk = {flow_instance.pk} 对应的 node_inst ')
    # 查询node_instance,并排序
    node_inst_list = NodeInstanceRepository().filter_by({'flow_instance': flow_instance.pk})
    node_inst_list = sorted(node_inst_list, key=lambda item: item.node_order, reverse=False)
    # 按顺序执行node
    for node_inst in node_inst_list:
        node_res = NodeMgr().run_node_instance(node_instance=node_inst, flow_data=flow_instance.flow_data)
        # 运行完node以后把返回的return_data放进flow_data
        data = flow_instance.flow_data
        data.update(node_res.return_data)
        flow_instance.flow_data = data
        # 判断 node 运行状态,是 Stop 和 Cancelled 就退出循环
        if node_res.node_instance.node_status in [NodeStatus.Cancelled.value, NodeStatus.Stop.value]:
            return flow_instance
        else:
            # 其他任何情况都继续执行
            continue
    return flow_instance


def update_result_and_status(flow_instance: FlowInstance) -> FlowInstance:
    flow_instance.flow_result = check_flow_result(flow_instance)
    flow_status = check_flow_status(flow_instance)
    if flow_status:
        flow_instance.flow_status = flow_status
    else:
        pass
    return flow_instance


def get_last_node_inst_attr(flow_instance: FlowInstance, attr_name):
    # 倒叙遍历Node_Instance，如果最后一个节点运行过有状态了，就返回这个
    # 如果最后一个节点没有运行过，就继续看上一个节点是不是stop和cancelled，是就返回，否则返回None
    node_inst_list = NodeInstanceRepository().filter_by({'flow_instance_id': flow_instance.pk})
    node_inst_list = sorted(node_inst_list, key=lambda item: item.node_order, reverse=True)
    last: NodeInstance = node_inst_list[0]
    if last.node_status != NodeStatus.Pending.value:
        return last.__getattribute__(attr_name)
    else:
        node_inst: NodeInstance
        for node_inst in node_inst_list:
            if node_inst.node_status in [NodeStatus.Stop.value, NodeStatus.Cancelled.value]:
                return node_inst.__getattribute__(attr_name)
            else:
                continue
        return None


def check_flow_result(flow_instance: FlowInstance):
    result_rule = flow_instance.flow_design.fw_result_rule
    if result_rule is None \
            or len(result_rule) == 0 \
            or FlowResultRuleRepository().count_by({'pk': flow_instance.flow_design.fw_result_rule}) == 0:
        return None
    # 查出对应的 flow_result_rules
    result_rule = FlowResultRuleRepository().filter_by_pk(flow_instance.flow_design.fw_result_rule)[0]
    if result_rule.rule_type == FlowRuleType.Default.value:
        return get_last_node_inst_attr(flow_instance, 'node_result')
    elif result_rule.rule_type == FlowRuleType.Script.value:
        # todo
        return None
    else:
        raise Exception(f'无法识别的result_rule_type = {result_rule.rule_type}')


def check_flow_status(flow_instance: FlowInstance):
    status_rule = flow_instance.flow_design.fw_status_rule
    if status_rule is None \
            or len(status_rule) == 0 \
            or FlowStatusRuleRepository().count_by({"pk": status_rule}) == 0:
        return None
    status_rule = FlowStatusRuleRepository().filter_by_pk(flow_instance.flow_design.fw_status_rule)[0]
    if status_rule.rule_type == FlowRuleType.Default.value:
        last_node_status = get_last_node_inst_attr(flow_instance, 'node_status')
        if last_node_status:
            return last_node_status
        else:
            pass
    elif status_rule.rule_type == FlowRuleType.Script.value:
        # todo
        return None
    else:
        raise Exception(f'无法识别的 status_rule_type = {status_rule.rule_type}')


class SingleFlowResult:
    def __init__(self, flow_instance):
        self.flow_instance = flow_instance or None


class SingleFlowRunner:

    @staticmethod
    def run(flow_instance: FlowInstance) -> SingleFlowResult:
        flow_instance = run_node_list(flow_instance)
        if flow_instance.flow_status == FlowStatus.Pending.value:
            flow_instance.flow_status = FlowStatus.Running.value
        flow_instance = update_result_and_status(flow_instance)
        return SingleFlowResult(flow_instance)
