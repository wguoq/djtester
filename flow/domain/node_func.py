import abc


class NodeFuncBase:
    def __init__(self):
        self.result = None
        self.return_data = None

    @abc.abstractmethod
    def do_func(self, node_data, flow_data):
        return self
