import abc


class NodeFuncBase:
    def __init__(self):
        self.result = None
        self.return_data = {}

    @abc.abstractmethod
    def node_func_data_model(self) -> dict:
        """
        node_func_data 的结构,用于编写传入参数
        """
        return {}

    @abc.abstractmethod
    def node_func_result_list(self) -> list[str]:
        """
        node_func 返回结果列表,用于编写 node_status 状态规则
        """
        return []

    @abc.abstractmethod
    def do_func(self, node_func_data, flow_data):
        """
        node_func 执行方法,完成后要把结果写入self.result和self.return_data
        """
        return self
