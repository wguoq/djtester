from django.db import transaction
from django.forms import model_to_dict
from flow.domain.enums import FlowStatus
from flow.domain.flow_runner import FlowInstanceRunner
from flow.models import Flow_Instance, Flow_Design
from flow.repositories import FlowInstanceDBHelper, NodeInstanceDBHelper, FlowNodeDesignOderDBHelper


class FlowMgr:
    def __init__(self):
        self.flow_instance = None

    @transaction.atomic
    def instance_flow_design(self, flow_design: Flow_Design, flow_data: dict = None):
        if flow_data is None:
            flow_data = {}
        # 保存 flow_instance
        fi = {'flow_design': flow_design,
              'flow_data': flow_data}
        self.flow_instance: Flow_Instance = FlowInstanceDBHelper().save_this(fi)
        # 查询出 node_list 保存 node_instance
        node_list = FlowNodeDesignOderDBHelper().filter_by({'flow_design_id': flow_design.id})
        for node in node_list:
            node_design = node.node_design
            node_func_name = node_design.node_func_name
            node_func_data = node_design.node_func_data
            node_order = node.node_order
            ni = {'node_design': node_design,
                  'node_func_name': node_func_name,
                  'node_func_data': node_func_data,
                  'node_order': node_order,
                  'flow_instance': self.flow_instance}
            NodeInstanceDBHelper().save_this(ni)
        return self

    def run_flow_instance(self, flow_instance: Flow_Instance):
        self.flow_instance = flow_instance
        # 先判断流程状态是不是已完成或者终止
        if flow_instance.flow_status in [FlowStatus.Finish.value, FlowStatus.Stop.value]:
            self.flow_instance = flow_instance
            return self
        else:
            self.flow_instance = FlowInstanceRunner().run(flow_instance).flow_instance
            # 保存
            FlowInstanceDBHelper().save_this(model_to_dict(self.flow_instance))
            return self


