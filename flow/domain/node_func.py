import abc


class NodeFuncBase:
    def __init__(self):
        self.result = None
        self.return_data = {}

    @abc.abstractmethod
    def node_func_data_model(self):
        return {}

    @abc.abstractmethod
    def do_func(self, node_func_data, flow_data):
        return self
