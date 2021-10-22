from django.db import transaction
from django.forms import model_to_dict

from flow.domain.flow_runner import FlowInstanceRunner
from flow.models import Flow_Instance, Flow_Design, Node_Instance
from flow.repositories import FlowInstanceDBHelper, NodeInstanceDBHelper, FlowNodeDesignOderDBHelper


class FlowMgr:
    def __init__(self):
        self.new_flow_status = None
        self.new_flow_result = None

    def run_flow(self, flow_instance: Flow_Instance):
        # 先判断流程状态是不是已完成或者终止
        if flow_instance.flow_status in ['finish', 'stop']:
            self.new_flow_status = flow_instance.flow_status
            self.new_flow_result = flow_instance.flow_result
            return self
        else:
            new = FlowInstanceRunner().run(flow_instance)
            # 保存
            new_flow_instance = FlowInstanceDBHelper().save_this(model_to_dict(new))
            return new_flow_instance

    @transaction.atomic
    def instance_flow_design(self, flow_design: Flow_Design, flow_data: dict):
        # 保存 flow_instance
        fi = {'flow_design': flow_design,
              'flow_data': flow_data}
        flow_instance: Flow_Instance = FlowInstanceDBHelper().save_this(fi)
        # 查询出 node_list 保存 node_instance
        node_list = FlowNodeDesignOderDBHelper().filter_by({'flow_design_id': flow_design.id})
        for node in node_list:
            node_design = node.node_design
            node_data = node_design.node_data
            node_order = node.node_order
            ni = {'node_design': node_design,
                  'node_data': node_data,
                  'node_order': node_order,
                  'flow_instance': flow_instance}
            NodeInstanceDBHelper().save_this(ni)

