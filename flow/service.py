from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseServicer
from flow.domain.flow_mgr import FlowMgr
from flow.domain.node_func import NodeFuncBase
from flow.repositories import *


class FLowServicer(BaseServicer):
    @show_class_name('service')
    def __init__(self, db_helper):
        super().__init__(db_helper)


class NodeFuncRunFLow(NodeFuncBase):
    @reg_node_func(node_type='flow_runner', class_path='flow.service')
    def __init__(self):
        super().__init__()

    def do_func(self, node_func_data: dict, flow_data: dict):
        flow_design_id = node_func_data.get('flow_design_id')
        flow_design = FlowDesignDBHelper().get_by({'pk': flow_design_id})
        flow_instance = FlowMgr().instance_flow_design(flow_design, flow_data)
        a = FlowMgr().run_flow(flow_instance)
        new_flow_instance: Flow_Instance = a.new_flow_instance
        self.result = new_flow_instance.flow_result
        return self

    def node_func_data_model(self):
        return {'flow_design_id': ''}
