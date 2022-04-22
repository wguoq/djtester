import random
import time
from djtester.repositories import BaseDBHelper
from flow.models import *

APP_NAME = 'flow'


class FlowDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, FlowDesign.__name__)

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
        super().__init__(APP_NAME, FlowStatusRule.__name__)


class FlowResultRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, FlowResultRule.__name__)


class NodeDesignDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, NodeDesign.__name__)

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
        super().__init__(APP_NAME, NodeStatusRule.__name__)


class FlowInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, FlowInstance.__name__)


class NodeInstanceDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, NodeInstance.__name__)


class FlowNodeOderDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, FlowNodeOder.__name__)

    # @transaction.atomic
    # def save_this(self, data: dict):
    #     flow_design = data.get('flow_design')
    #     node_design = data.get('node_design')
    #     node_order = data.get('node_order')
    #     if node_order is None or len(str(node_order)) == 0:
    #         raise Exception(' node_order 不能为空并且要为正整数')
    #     # elif int(node_order) <= 0:
    #     #     raise Exception(' node_order 需要是大于0的正整数')
    #     # if flow_design is None or len(str(flow_design)) == 0:
    #     #     raise Exception(' flow_design 不能为空')
    #     # if node_design is None or len(str(node_design)) == 0:
    #     #     raise Exception(' node_design 不能为空')
    #     # orders = super().filter_by(dict(flow_design=flow_design))
    #     # for o in orders:
    #     #     if str(node_order) == str(o.node_order):
    #     #         raise Exception('node_order 不能重复')
    #     if node_design:
    #         node_design = save_foreignkey(REPOSITORIES_PATH,
    #                                       NodeDesignDBHelper.__name__, node_design)
    #     if flow_design:
    #         flow_design = save_foreignkey(REPOSITORIES_PATH, FlowDesignDBHelper.__name__, flow_design)
    #     data['node_design'] = node_design
    #     data['flow_design'] = flow_design
    #     return super().save_this(data)


class NodeStartRuleDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, NodeStartRule.__name__)
