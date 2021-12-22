import importlib

from django.db import transaction

from djtester.repositories import BaseDBHelper, save_foreignkey
from flow.models import *

MODELS_PATH = 'flow.models'
REPOSITORIES_PATH = 'flow.repositories'


class FlowDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Design.__name__)

    def _save_m2m(self, new_model):
        return new_model


class FlowStatusRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Status_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model

    # @transaction.atomic
    # def save_this(self, data: dict):
    #     data['flow_design'] = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
    #     return super().save_this(data)


class FlowResultRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Result_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model

    # @transaction.atomic
    # def save_this(self, data: dict):
    #     data['flow_design'] = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
    #     return super().save_this(data)


class NodeDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Design.__name__)

    def _save_m2m(self, new_model):
        return new_model


class NodeStatusRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Status_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)


class FlowInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Instance.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['flow_design'] = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        return super().save_this(data)


class NodeInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Instance.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        data['flow_instance'] = save_foreignkey(REPOSITORIES_PATH, FlowInstanceDBHelper.__name__,
                                                data.get('flow_instance'))
        return super().save_this(data)


class FlowNodeDesignOderDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Node_Design_Oder.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        node_order = data.get('node_order')
        if node_order is None or len(str(node_order)) == 0:
            raise Exception(' node_order 为空')
        node_design = save_foreignkey(REPOSITORIES_PATH,
                                      NodeDesignDBHelper.__name__, data.get('node_design'))
        flow_design = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        # 检查一下 node_data 里面的flow_design_id 不能和自己关联的 flow_design_id 一样,避免死循环
        if node_design.node_func_name == 'flow_runner' and node_design.node_func_data.get('flow_design_id') == flow_design.id:
            raise Exception(f' node_design_id = {node_design.id} node_design_name = {node_design.node_name} 嵌套了自己的 flow_design,会死循环')
        else:
            data['node_design'] = node_design
            data['flow_design'] = flow_design
            return super().save_this(data)


class NodeStartRuleDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Start_Rule_Design.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH,
                                              NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)


class NodeStartRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Start_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['rule_design'] = save_foreignkey(REPOSITORIES_PATH,
                                              NodeStartRuleDesignDBHelper.__name__, data.get('rule_design'))
        return super().save_this(data)
