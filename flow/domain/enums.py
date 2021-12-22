from enum import Enum


class FlowType(Enum):
    Serial = 'serial'
    Parallel = 'parallel'


class FlowStatus(Enum):
    Pending = 'pending'
    Running = 'running'
    Finish = 'finish'
    Cancelled = 'cancelled'
    Stop = 'stop'
    Unknown = 'unknown'


class NodeStatus(Enum):
    Pending = 'pending'
    Running = 'running'
    Finish = 'finish'
    Cancelled = 'cancelled'
    Stop = 'stop'
    Skip = 'skip'
    Unknown = 'unknown'


class NodeStartRuleTarget(Enum):
    FlowData = 'flow_data'
    NodeResult = 'node_result'
    NodeStatus = 'node_status'


class FlowResultRuleType(Enum):
    Default_1 = ['last_node_result', '使用最后一个运行过的节点结果']
    Custom = ['custom', '使用自定义方法']


class FlowStatusRuleType(Enum):
    Default_1 = ['last_node_status', '使用最后一个运行过的节点状态']
    Custom = ['custom', '使用自定义方法']
