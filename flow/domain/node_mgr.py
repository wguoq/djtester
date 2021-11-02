import operator

from django.forms import model_to_dict
from djtester.tools_man import verify_str
from flow.domain.enums import NodeStatus
from flow.domain.node_runner import NodeInstanceRunner
from flow.models import Node_Instance, Node_Start_Rule
from flow.repositories import NodeInstanceDBHelper, NodeStartRuleDesignDBHelper, NodeStartRuleDBHelper


class NodeMgr:
    def __init__(self):
        self.node_instance = None
        self.return_data = {}

    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        self.node_instance = node_instance
        # 判断是否满足节点运行条件
        a = self._check_node_start_rule(node_instance, flow_data)
        if a:
            # run 并且返回新的 Node_Instance
            run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
            self.node_instance = run_node_result.node_instance
            self.return_data = run_node_result.return_data
            # 保存新的 Node_Instance
            NodeInstanceDBHelper().save_this(model_to_dict(self.node_instance))
            return self
        else:
            return self

    @staticmethod
    def re_check_node_status(new_result: str, node_instance_id):
        node_instance = NodeInstanceDBHelper().get_by({'pk': node_instance_id})
        new_node_instance = NodeInstanceRunner().re_check_status(new_result, node_instance)
        NodeInstanceDBHelper().save_this(model_to_dict(new_node_instance))
        return {'node_status': new_node_instance.node_status,
                'flow_instance_id': new_node_instance.flow_instance.id}

    def _check_node_start_rule(self, node_instance, flow_data):
        # 1.node 状态不为finish,stop,skip才执行
        if node_instance.node_status in [NodeStatus.Finish.value, NodeStatus.Stop.value, NodeStatus.Skip.value]:
            return False
        else:
            # 查询出 start_rule_design_list
            node_design_id = node_instance.node_design.id
            start_rule_design_list = NodeStartRuleDesignDBHelper().filter_by(
                {'node_design_id': node_design_id}).order_by('rule_order')
            # start_rule_design_list 里面所有都 True 才算通过
            for start_rule_design in start_rule_design_list:
                if self._check_start_rule_design(start_rule_design, flow_data, node_instance):
                    continue
                else:
                    return False
            return True

    def _check_start_rule_design(self, start_rule_design, flow_data, node_instance):
        start_rule_list = NodeStartRuleDBHelper().filter_by({'rule_design': start_rule_design.id})
        if start_rule_design.rule_type == 'and':
            if self._check_rule_type_and(start_rule_list, flow_data, node_instance):
                return True
            else:
                return False
        elif start_rule_design.rule_type == 'or':
            if self._check_rule_type_or(start_rule_list, flow_data, node_instance):
                return True
            else:
                return False
        else:
            raise Exception(f'无法识别的 rule_type={start_rule_design.rule_type} 应该是 and 或者 or ')

    def _check_rule_type_and(self, start_rule_list, flow_data, node_instance):
        for start_rule in start_rule_list:
            if self._check_start_rule(start_rule, flow_data, node_instance):
                continue
            else:
                return False
        return True

    def _check_rule_type_or(self, start_rule_list, flow_data, node_instance):
        for start_rule in start_rule_list:
            if self._check_start_rule(start_rule, flow_data, node_instance):
                return True
            else:
                continue
        return False

    @staticmethod
    def _check_start_rule(start_rule: Node_Start_Rule, flow_data: dict, node_instance: Node_Instance):
        rule_target = start_rule.rule_target
        rule_where = start_rule.rule_where
        rule_operator = start_rule.rule_operator
        rule_value = start_rule.rule_value
        if rule_target == 'flow_data':
            data = flow_data.get(str(rule_where))
            return verify_str(data, rule_operator, rule_value)
        elif rule_target == 'node_result':
            # 先查询出对应 flow_instance 里所有 node_instance
            flow_instance_id = node_instance.flow_instance.id
            node_instance_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance_id})
            # 找到对应 rule_where 里面 node_design_id 的那一条取node_result
            for node_instance_ in node_instance_list:
                if str(node_instance_.node_design.id) == str(rule_where):
                    return verify_str(node_instance_.node_result, rule_operator, rule_value)
                else:
                    continue
            return False
        else:
            raise Exception(f' 无法识别的 rule_target = {rule_target} 只能是 flow_data | node_result')
