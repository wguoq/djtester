from enum import Enum


class FlowType(Enum):
    Serial = 'serial'
    Parallel = 'parallel'


class FlowStatus(Enum):
    Ready = 'ready'
    Running = 'running'
    Finish = 'finish'
    Stop = 'stop'
    Unknown = 'unknown'


class NodeStatus(Enum):
    Ready = 'ready'
    Running = 'running'
    Finish = 'finish'
    Stop = 'stop'
    Skip = 'skip'
    Unknown = 'unknown'
