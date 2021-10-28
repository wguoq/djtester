from django.forms import model_to_dict
from flow.domain.enums import NodeStatus
from flow.domain.node_runner import NodeInstanceRunner
from flow.models import Node_Instance, Node_Start_Rule_Design
from flow.repositories import NodeInstanceDBHelper, NodeStartRuleDesignDBHelper, NodeStartRuleDBHelper


class NodeMgr:
    def __init__(self):
        self.new_node_status = None
        self.new_node_result = None
        self.return_data = {}

    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        # 判断是否满足节点运行条件
        if self._check_node_start_rule(node_instance, flow_data):
            # run 并且返回新的 Node_Instance
            run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
            # 保存新的 Node_Instance
            new_node_instance = NodeInstanceDBHelper().save_this(model_to_dict(run_node_result.new_node_instance))
            # 返回 node_status 和 node_result
            self.new_node_status = new_node_instance.node_status
            self.new_node_result = new_node_instance.node_result
            self.return_data = run_node_result.return_data
            return self
        else:
            return self

    @staticmethod
    def re_check_node_status(result: str, node_instance_id):
        node_instance = NodeInstanceDBHelper().get_by({'pk': node_instance_id})
        new_node_instance = NodeInstanceRunner().update_result_status(result, node_instance)
        new_node_instance = NodeInstanceDBHelper().save_this(model_to_dict(new_node_instance))
        node_status = new_node_instance.node_status
        flow_instance_id = new_node_instance.flow_instance.id
        return {'node_status': node_status,
                'flow_instance_id': flow_instance_id}

    @staticmethod
    def _check_node_start_rule(node_instance, flow_data):
        # 1.node 状态不为finish,stop,skip才执行
        if node_instance.node_status in [NodeStatus.Finish.value, NodeStatus.Stop.value, NodeStatus.Skip.value]:
            return False
        else:
            # todo
            # 查询出 start_rule_design_list
            # node_design_id = node_instance.node_design.id
            # start_rule_design_list = NodeStartRuleDesignDBHelper().filter_by({'node_design_id': node_design_id}).order_by('rule_order')
            # for start_rule_design in start_rule_design_list:
            #     # 查询出 start_rule_list
            #     start_rule_design: Node_Start_Rule_Design
            #     if start_rule_design.rule_type == 'and':
            #
            #         start_rule_list = NodeStartRuleDBHelper().filter_by({'rule_design_id': start_rule_design.id})
            #     elif start_rule_design.rule_type == 'or':
            #         pass
            #     else:
            #         raise Exception(f'')

            return True






