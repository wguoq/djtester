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
    LastNodeResult = 'last_node_result'
    Custom = 'custom'


class FlowStatusRuleType(Enum):
    LastNodeStatus = 'last_node_status'
    Custom = 'custom'
