"""
这里和view对接接口，表的增删改查相关业务逻辑写在这里
"""
import json
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
    def get_temp():
        a = model_to_dict(Flow_Design())
        a.pop("id")
        a.pop("code")
        return a

    def add(self, data: dict):
        # 新增时生成code
        code = 'fw' + str(round(time.time()) + random.randint(0, 99))
        data.update({"code": code})
        return super().add(data)


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

    @staticmethod
    def get_temp():
        a = model_to_dict(Node_Design())
        a.pop("id")
        a.pop("code")
        return a

    def add(self, data):
        code = 'nd' + str(round(time.time()) + random.randint(0, 99))
        data.update({"code": code})
        return super().add(data)


class NodeInstService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(NodeInstanceDBHelper())


class FlowNodeService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(FlowNodeOderDBHelper())

    @staticmethod
    def get_temp():
        a = model_to_dict(Flow_Node_Oder())
        a.pop("id")
        return a

    def filter_by(self, kwargs: dict):
        fn_list = super().filter_by(kwargs)
        # fn_list = FlowNodeOderDBHelper().filter_by(kwargs)
        a = []
        for fn in fn_list:
            # node_dict = dict(id=fn.id, flow_design_id=fn.flow_design.id, node_order=fn.node_order,
            #                  node_design_id=fn.node_design.id)
            # node_dict.update(model_to_dict(fn.node_design))
            # a.append(node_dict)
            node_design = NodeDesignService().get_by_pk(fn.get('node_design'))
            node_design.pop('id')
            # 就把fn的时间字段给覆盖进去
            node_design.update(fn)
            a.append(node_design)
        return a

    # 新增和编辑要检查
    # 1.node_order不能为空
    # 2.如果是子流程，不能出现死循环
    @staticmethod
    def _check(data: dict):
        node_order = data.get('node_order')
        flow_design_id = data.get('flow_design')
        node_func_data = data.get('node_func_data')
        if node_func_data:
            node_func_data = json.loads(node_func_data)
        flow_id = node_func_data.get('flow_design_id')
        if node_order is None or len(str(node_order)) == 0:
            raise Exception(' node_order 不能为空并且要为数字')
        elif flow_design_id == flow_id:
            raise Exception(f'子流程的flow_design_id == 父流程的flow_design_id，会死循环')

    # 需要同时保存到关系表和node表
    @transaction.atomic
    def add(self, data: dict):
        self._check(data)
        flow_design = data.pop('flow_design')
        node_order = data.pop('node_order')
        node_design = data.pop('node_design')
        node_func_data = data.pop('node_func_data')
        if node_func_data:
            data.update(dict(node_func_data=json.loads(node_func_data)))
        # 存node_design
        if node_design is None:
            node_design = NodeDesignService().add(data).get("id")
        else:
            pass
        flow_node_dict = dict(flow_design=flow_design,
                              node_order=node_order,
                              node_design=node_design)
        return super().add(flow_node_dict)

    # 需要同时修改关系表和node表
    def edit(self, data):
        self._check(data)




        return super().edit(data)


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
