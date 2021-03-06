from enum import Enum


class FlowType(Enum):
    Single = 'single'
    Multiple = 'multiple'


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
    Unknown = 'unknown'


class NodeStartRuleTarget(Enum):
    FlowData = 'flow_data'
    NodeResult = 'node_result'
    NodeStatus = 'node_status'


class FlowRuleType(Enum):
    Default = 'default'
    Script = 'script'


class NodeStartRuleType(Enum):
    AND = 'and'
    OR = 'or'
    Custom = 'custom'
