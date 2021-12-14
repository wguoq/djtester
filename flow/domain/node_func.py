import abc


class NodeFuncBase:
    def __init__(self):
        self.result = ""
        self.return_data = {}

    @abc.abstractmethod
    def node_func_param(self) -> dict:
        """
        node_func 需要的参数结构
        """
        return {}

    @abc.abstractmethod
    def node_func_result_list(self) -> list[str]:
        """
        node_func 返回结果列表,用于编写对应 node_status 的状态规则
        """
        return []

    @abc.abstractmethod
    def do_func(self, node_func_param: dict, flow_data: dict):
        """
        node_func 执行方法,完成后要把结果写入self.result和self.return_data
        """
        return self
