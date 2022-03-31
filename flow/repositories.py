import random
import time
from django.db import transaction
from djtester.repositories import BaseDBHelper, save_foreignkey
from flow.models import *

MODELS_PATH = 'flow.models'
REPOSITORIES_PATH = 'flow.repositories'


class FlowDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Design.__name__)

    def save_this(self, data: dict):
        code = data.get('code')
        if code is None or len(code) == 0:
            code = 'fw' + str(round(time.time()) + random.randint(0, 99))
            data.update({"code": code})
        else:
            pass
        return super().save_this(data)


class FlowStatusRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Status_Rule.__name__)


class FlowResultRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Result_Rule.__name__)


class NodeDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Design.__name__)


class NodeStatusRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Status_Rule.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)


class FlowInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Instance.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['flow_design'] = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        return super().save_this(data)


class NodeInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Instance.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        data['flow_instance'] = save_foreignkey(REPOSITORIES_PATH, FlowInstanceDBHelper.__name__,
                                                data.get('flow_instance'))
        return super().save_this(data)


class FlowNodeOderDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Flow_Node_Oder.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        node_order = data.get('node_order')
        if node_order is None or len(str(node_order)) == 0:
            raise Exception(' node_order 不能为空并且要为数字')
        node_design = save_foreignkey(REPOSITORIES_PATH,
                                      NodeDesignDBHelper.__name__, data.get('node_design'))
        flow_design = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        data['node_design'] = node_design
        data['flow_design'] = flow_design
        return super().save_this(data)


class NodeStartRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Node_Start_Rule.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH,
                                              NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)
