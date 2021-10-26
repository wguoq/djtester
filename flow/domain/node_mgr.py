import abc

from django.forms import model_to_dict

from flow.domain.node_runner import NodeInstanceRunner
from flow.models import Node_Instance
from flow.repositories import NodeInstanceDBHelper


class NodeMgr:
    def __init__(self):
        self.new_node_status = None
        self.new_node_result = None
        self.return_data = {}

    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        # 判断是否满足节点运行条件
        if self._check_node_start_rule(node_instance.node_design.node_start_rule):
            run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
            # 保存新的Node_Instance
            new_node_instance = NodeInstanceDBHelper().save_this(model_to_dict(run_node_result.new_node_instance))
            # 返回 node_status 和 node_result
            self.new_node_status = new_node_instance.node_status
            self.new_node_result = new_node_instance.node_result
            self.return_data = run_node_result.return_data
            return self
        else:
            return self

    def _check_node_start_rule(self, node_start_rule):
        # todo
        # 1.node 状态不为finish和stop才执行
        # 2.满足start_rule
        return True


