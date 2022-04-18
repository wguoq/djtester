import random
import time
from django.db import transaction
from djtester.repositories import BaseDBHelper, save_foreignkey
from flow.models import *

MODELS_PATH = 'flow.models'
REPOSITORIES_PATH = 'flow.repositories'


class FlowDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, FlowDesign.__name__)

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
        super().__init__(MODELS_PATH, FlowStatusRule.__name__)


class FlowResultRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, FlowResultRule.__name__)


class NodeDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, NodeDesign.__name__)

    def save_this(self, data: dict):
        code = data.get('code')
        if code is None or len(code) == 0:
            code = 'nd' + str(round(time.time()) + random.randint(0, 99))
            data.update({"code": code})
        else:
            pass
        return super().save_this(data)


class NodeStatusRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, NodeStatusRule.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)


class FlowInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, FlowInstance.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['flow_design'] = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, data.get('flow_design'))
        return super().save_this(data)


class NodeInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, NodeInstance.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH, NodeDesignDBHelper.__name__, data.get('node_design'))
        data['flow_instance'] = save_foreignkey(REPOSITORIES_PATH, FlowInstanceDBHelper.__name__,
                                                data.get('flow_instance'))
        return super().save_this(data)


class FlowNodeOderDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, FlowNodeOder.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        flow_design = data.get('flow_design')
        node_design = data.get('node_design')
        node_order = data.get('node_order')
        if node_order is None or len(str(node_order)) == 0:
            raise Exception(' node_order 不能为空并且要为正整数')
        # elif int(node_order) <= 0:
        #     raise Exception(' node_order 需要是大于0的正整数')
        # if flow_design is None or len(str(flow_design)) == 0:
        #     raise Exception(' flow_design 不能为空')
        # if node_design is None or len(str(node_design)) == 0:
        #     raise Exception(' node_design 不能为空')
        # orders = super().filter_by(dict(flow_design=flow_design))
        # for o in orders:
        #     if str(node_order) == str(o.node_order):
        #         raise Exception('node_order 不能重复')
        if node_design:
            node_design = save_foreignkey(REPOSITORIES_PATH,
                                          NodeDesignDBHelper.__name__, node_design)
        if flow_design:
            flow_design = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, flow_design)
        data['node_design'] = node_design
        data['flow_design'] = flow_design
        return super().save_this(data)


class NodeStartRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, NodeStartRule.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['node_design'] = save_foreignkey(REPOSITORIES_PATH,
                                              NodeDesignDBHelper.__name__, data.get('node_design'))
        return super().save_this(data)
