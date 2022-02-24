"""
这里只处理表外键相关的逻辑，不涉及业务逻辑
"""

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


class FlowResultRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Result_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model


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
        super().__init__(MODELS_PATH, Flow_Node_Oder.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        node_design = save_foreignkey(REPOSITORIES_PATH,
                                      NodeDesignDBHelper.__name__, data.get('node_design'))
        flow_design = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        data['node_design'] = node_design
        data['flow_design'] = flow_design
        return super().save_this(data)


class NodeStartRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Start_Rule.__name__)

    def _save_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH,
                                              NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)
