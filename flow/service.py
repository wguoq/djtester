from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseServicer
from flow.domain.enums import FlowStatus
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

    def node_func_param(self) -> dict:
        return {'flow_design_id': ''}

    def node_func_result_list(self) -> list[str]:
        return [FlowStatus.Running.value,
                FlowStatus.Finish.value,
                FlowStatus.Cancelled.value]

    def do_func(self, node_func_param: dict, flow_data: dict):
        flow_design_id = node_func_param.get('flow_design_id')
        flow_design = FlowDesignDBHelper().get_by({'pk': flow_design_id})
        flow_instance = FlowMgr().instance_flow_design(flow_design, flow_data)
        new_flow_instance = FlowMgr().run_flow_instance(flow_instance)
        self.result = new_flow_instance.flow_result
        return self
