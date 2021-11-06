from django.forms import model_to_dict
from djtester.tools_man import verify_str
from flow.domain.enums import NodeStatus, NodeStartRuleTarget
from flow.domain.node_runner import NodeInstanceRunner
from flow.models import Node_Instance, Node_Start_Rule
from flow.repositories import NodeInstanceDBHelper, NodeStartRuleDesignDBHelper, NodeStartRuleDBHelper


class NodeMgr:
    def __init__(self):
        self.node_instance = None
        self.return_data = {}

    # def only_run_node_instance(self, node_instance: Node_Instance, flow_data: dict = None):
    #     print(f'开始 run node_instance = {node_instance.id}')
    #     self.node_instance = node_instance
    #     run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
    #     self.node_instance = run_node_result.node_instance
    #     self.return_data = run_node_result.return_data
    #     print(f'结束 run node_instance = {node_instance.id}')
    #     return self
    #

    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        self.node_instance = node_instance
        # 判断是否满足节点运行条件
        a = self.check_node_start_rule(node_instance, flow_data)
        if a:
            # run 并且返回新的 Node_Instance
            run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
            self.node_instance = run_node_result.node_instance
            self.return_data = run_node_result.return_data
            # 保存新的 Node_Instance
            NodeInstanceDBHelper().save_this(model_to_dict(self.node_instance))
            return self
        else:
            print(f'节点不满足运行状态 node_instance_id = {node_instance.id}')
            return self

    def re_check_node_status(self, new_result: str, node_instance_id):
        node_instance = NodeInstanceDBHelper().get_by({'pk': node_instance_id})
        new_node_instance = NodeInstanceRunner().re_check_status(new_result, node_instance)
        NodeInstanceDBHelper().save_this(model_to_dict(new_node_instance))
        self.node_instance = new_node_instance
        return self

    def check_node_start_rule(self, node_instance, flow_data):
        # 1.只运行 node 状态是 Pending 和 Unknown 的,不运行 Running 状态的避免重复提交
        if node_instance.node_status in [NodeStatus.Pending.value, NodeStatus.Unknown.value]:
            # 查询出 start_rule_design_list
            node_design_id = node_instance.node_design.id
            start_rule_design_list = NodeStartRuleDesignDBHelper().filter_by(
                {'node_design_id': node_design_id}).order_by('rule_order')
            # start_rule_design_list 里面所有都 True 才算通过
            for start_rule_design in start_rule_design_list:
                if self._check_start_rule_design(start_rule_design, flow_data, node_instance):
                    continue
                else:
                    print(f'节点启动条件没有通过 start_rule_design = {start_rule_design.id}')
                    return False
            return True
        else:
            print(f'节点状态为 {node_instance.node_status} 不运行 node_instance_id = {node_instance.id}')
            return False

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
        if rule_target == NodeStartRuleTarget.FlowData.value:
            # 约定 rule_where 填的是 flow_data 里面的一个 key
            data = flow_data.get(str(rule_where))
            return verify_str(data, rule_operator, rule_value)
        elif rule_target == NodeStartRuleTarget.NodeResult.value:
            # 约定 rule_where 填的是 node_design_id
            # 因为 flow_instance_id 和 node_design_id 联合查询应该只会有一条数据,所有用get
            node_instance_target = NodeInstanceDBHelper().get_by({'flow_instance_id': node_instance.flow_instance.id,
                                                                  'node_design_id': rule_where})
            return verify_str(node_instance_target.node_result, rule_operator, rule_value)
        elif rule_target == NodeStartRuleTarget.NodeStatus.value:
            # 约定 rule_where 填的是 node_design_id
            # 因为 flow_instance_id 和 node_design_id 联合查询应该只会有一条数据,所有用get
            node_instance_target = NodeInstanceDBHelper().get_by({'flow_instance_id': node_instance.flow_instance.id,
                                                                  'node_design_id': rule_where})
            return verify_str(node_instance_target.node_status, rule_operator, rule_value)
        else:
            raise Exception(f' 无法识别的 rule_target = {rule_target} 只能是 flow_data | node_result | node_status')
