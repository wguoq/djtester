import random
import time

from django.forms import model_to_dict
from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseService
from flow.domain.flow_mgr import *
from flow.domain.node_func import NodeFuncBase
from flow.repositories import *


class FlowDesignService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(FlowDesignDBHelper())

    @staticmethod
    def get_design_temp():
        a = model_to_dict(Flow_Design())
        a.pop("id")
        a.pop("code")
        return a

    @staticmethod
    def add_flow_design(data: dict):
        code = 'fw' + str(round(time.time()) + random.randint(0, 99))
        data.update({"code": code})
        a = FlowDesignDBHelper().save_this(data)
        return a.id

    @staticmethod
    def edit_flow_design(data: dict):
        a = FlowDesignDBHelper().save_this(data)
        return a.id

    @staticmethod
    def get_node_list(flow_design_id):
        node_list = FlowNodeDesignOderDBHelper().filter_by({'flow_design_id': flow_design_id})
        a = []
        for node in node_list:
            node_dict = dict(id=node.id, flow_design_id=node.flow_design.id, node_order=node.node_order,
                             node_design_id=node.node_design.id)
            node_dict.update(model_to_dict(node.node_design))
            a.append(node_dict)
        return a


class FlowInstService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(FlowInstanceDBHelper())

    @staticmethod
    def run_inst(pk):
        flow_instance = FlowInstanceDBHelper().get_by(dict(pk=pk))
        a = FlowMgr.run_flow_instance(flow_instance)
        return model_to_dict(a)


class NodeDesignService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(NodeDesignDBHelper())


class NodeInstService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(NodeInstanceDBHelper())


class FlowService:
    @staticmethod
    def instance_flow(flow_design_id, flow_data):
        return FlowMgr.instance_flow_design(flow_design_id, flow_data)


class NodeFuncRunFLow(NodeFuncBase):

    @reg_node_func(node_type='flow_runner', class_path='flow.service')
    def __init__(self):
        pass

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
        return self.NodeFuncResult(new_flow_instance.flow_result)
