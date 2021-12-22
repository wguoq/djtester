from flow.domain.enums import NodeStatus, FlowResultRuleType, FlowStatusRuleType
from flow.models import Flow_Instance, Flow_Result_Rule, Flow_Status_Rule
from flow.repositories import FlowStatusRuleDBHelper, FlowResultRuleDBHelper, NodeInstanceDBHelper


def get_last_node_inst_attr(flow_instance: Flow_Instance, attr_name):
    # 使用最后一个运行过的节点结果
    node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance.id}).order_by('-node_order')
    for node_instance in node_instance_list:
        # 运行过的 node_instance 才检查
        if node_instance.node_status != NodeStatus.Pending.value:
            return node_instance.__getattribute__(attr_name)
        else:
            continue
    return None


def check_flow_result(flow_instance: Flow_Instance):
    # 查出对应的 flow_result_rules
    result_rule: Flow_Result_Rule = FlowResultRuleDBHelper().get_by({'flow_design_id': flow_instance.flow_design_id})
    if result_rule.result_rule_type == FlowResultRuleType.Default_1.value[0]:
        return get_last_node_inst_attr(flow_instance, 'node_result')
    elif result_rule.result_rule_type == FlowResultRuleType.Custom.value[0]:
        # todo 没想好
        return None
    else:
        raise Exception(f'无法识别的result_rule_type = {result_rule.result_rule_type}')


def check_flow_status(flow_instance: Flow_Instance):
    status_rule: Flow_Status_Rule = FlowStatusRuleDBHelper().get_by({'flow_design_id': flow_instance.flow_design_id})
    if status_rule.status_rule_type == FlowStatusRuleType.Default_1.value[0]:
        return get_last_node_inst_attr(flow_instance, 'node_status')
    elif status_rule.status_rule_type == FlowStatusRuleType.Custom.value[0]:
        # todo 没想好
        return None
    else:
        raise Exception(f'无法识别的 status_rule_type = {status_rule.status_rule_type}')
