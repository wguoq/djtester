"""
负责把业务实现方法的组装成各种接口
返回值统一为dict或者list
"""
import json
import random
import time
from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseService
from flow.domain.flow_mgr import *
from flow.domain.node_func import NodeFuncBase
from flow.repositories import *


class FlowDesignService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(FlowDesignDBHelper())

    def add(self, data: dict) -> dict:
        # 新增时生成code
        code = 'fw' + str(round(time.time()) + random.randint(0, 99))
        data.update({"code": code})
        return super().add(data)


class FlowInstService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(FlowInstanceDBHelper())

    @staticmethod
    def run_inst(flow_instance_pk) -> dict:
        flow_instance = FlowInstanceDBHelper().get_by(dict(pk=flow_instance_pk))
        return model_to_dict(FlowMgr.run_flow_instance(flow_instance))


class NodeDesignService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(NodeDesignDBHelper())

    def add(self, data) -> dict:
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

    def filter_by(self, kwargs: dict, offset: int = 0, limit: int = 1000) -> list:
        res = super().filter_by(kwargs=kwargs, offset=offset, limit=limit)
        ll = []
        for r in res:
            node_design_id = r.get('node_design')
            r.update(NodeDesignService().get_by_pk(node_design_id))
            ll.append(r)
        return ll

    # 新增和编辑要检查
    # 1.node_order不能为空
    # 2.如果是子流程，不能出现死循环
    @staticmethod
    def _check(data: dict):
        node_order = data.get('node_order')
        if node_order is None or len(str(node_order)) == 0:
            raise Exception(' node_order 不能为空并且要为数字')
        flow_design = data.get('flow_design')
        node_func_data = data.get('node_func_data')
        if node_func_data:
            node_func_data = json.loads(node_func_data)
            flow_id = node_func_data.get('flow_design_id')
            if flow_design == flow_id:
                raise Exception(f'子流程的flow_design_id == 父流程的flow_design_id，会死循环')

    # 需要同时保存到关系表和node表
    @transaction.atomic
    def add(self, data: dict) -> dict:
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
    def edit(self, data) -> dict:
        self._check(data)
        flow_design = data.pop('flow_design')
        node_order = data.pop('node_order')
        node_design = data.pop('node_design')
        node_func_data = data.pop('node_func_data')
        if node_func_data:
            data.update(dict(node_func_data=json.loads(node_func_data)))
        # 存node_design
        if node_design is None:
            node_design = NodeDesignService().edit(data).get("id")
        else:
            pass
        flow_node_dict = dict(flow_design=flow_design,
                              node_order=node_order,
                              node_design=node_design)
        return super().edit(flow_node_dict)


class FlowService:
    @staticmethod
    def instance_flow(flow_design_id, flow_data) -> dict:
        return model_to_dict(FlowMgr.instance_flow(flow_design_id, flow_data))


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
        flow_instance = FlowMgr().instance_flow(flow_design, flow_data)
        new_flow_instance = FlowMgr().run_flow_instance(flow_instance)
        return self.NodeFuncResult(new_flow_instance.flow_result)


class NodeStartRuleService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(NodeStartRuleDBHelper())
