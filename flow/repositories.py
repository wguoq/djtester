import random
import time
from django.db import transaction
from djtester.repositories import BaseRepository, save_foreignkey
from flow.models import *

APP_NAME = 'flow'


class FlowDesignRepository(BaseRepository):
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


class FlowStatusRuleRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, FlowStatusRule.__name__)


class FlowResultRuleRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, FlowResultRule.__name__)


class NodeDesignRepository(BaseRepository):
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


class NodeStatusRuleRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, NodeStatusRule.__name__)


class FlowInstanceRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, FlowInstance.__name__)


class NodeInstanceRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, NodeInstance.__name__)


class FlowNodeOderRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, FlowNodeOder.__name__)

    @transaction.atomic
    def _replace_fk_data(self, data: dict):
        flow_design = data.get('flow_design')
        node_design = data.get('node_design')
        if node_design:
            node_design = save_foreignkey(APP_NAME, NodeDesign.__name__, node_design)
        else:
            node_design = None
        if flow_design:
            flow_design = save_foreignkey(APP_NAME, FlowDesign.__name__, flow_design)
        else:
            flow_design = None
        data['node_design'] = node_design
        data['flow_design'] = flow_design
        return data


class NodeStartRuleRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, NodeStartRule.__name__)
