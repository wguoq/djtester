from enum import Enum


class FlowType(Enum):
    Serial = 'serial'
    Parallel = 'parallel'


class FlowStatus(Enum):
    Pending = 'pending'
    Running = 'running'
    Finish = 'finish'
    Cancelled = 'cancelled'
    Unknown = 'unknown'


class NodeStatus(Enum):
    Pending = 'pending'
    Running = 'running'
    Finish = 'finish'
    Cancelled = 'cancelled'
    Skip = 'skip'
    Unknown = 'unknown'


class NodeStartRuleTarget(Enum):
    FlowData = 'flow_data'
    NodeResult = 'node_result'
    NodeStatus = 'node_status'
