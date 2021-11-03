from django.db import transaction
from django.forms import model_to_dict
from flow.domain.enums import FlowStatus, NodeStatus
from flow.domain.flow_runner import FlowInstanceRunner
from flow.models import Flow_Instance, Flow_Design, Node_Instance
from flow.repositories import FlowInstanceDBHelper, NodeInstanceDBHelper, FlowNodeDesignOderDBHelper


class FlowMgr:
    def __init__(self):
        self.flow_instance = None

    @transaction.atomic
    def instance_flow_design(self, flow_design: Flow_Design, flow_data: dict = None):
        if flow_data is None:
            flow_data = {}
        # 保存 flow_instance
        fi = {'flow_design': flow_design,
              'flow_data': flow_data}
        self.flow_instance: Flow_Instance = FlowInstanceDBHelper().save_this(fi)
        # 查询出 node_list 保存 node_instance
        node_list = FlowNodeDesignOderDBHelper().filter_by({'flow_design_id': flow_design.id})
        for node in node_list:
            node_design = node.node_design
            node_func_name = node_design.node_func_name
            node_func_data = node_design.node_func_data
            node_order = node.node_order
            ni = {'node_design': node_design,
                  'node_func_name': node_func_name,
                  'node_func_data': node_func_data,
                  'node_order': node_order,
                  'flow_instance': self.flow_instance}
            NodeInstanceDBHelper().save_this(ni)
        return self

    def run_flow_instance(self, flow_instance: Flow_Instance):
        self.flow_instance = flow_instance
        # 先判断流程状态是不是已完成或者终止
        if flow_instance.flow_status in [FlowStatus.Finish.value, FlowStatus.Stop.value]:
            self.flow_instance = flow_instance
            print(f'流程状态是Finish|Stop 不运行; flow_instance_id = {flow_instance.id}')
            return self
        else:
            self.flow_instance = FlowInstanceRunner().run(flow_instance).flow_instance
            # 保存
            FlowInstanceDBHelper().save_this(model_to_dict(self.flow_instance))
            return self

    @transaction.atomic
    def rollback_to_node(self, node_instance: Node_Instance):
        """
        只能回滚流程和节点的执行结果和状态,不能回滚flow_data和node_data
        也无法回滚子流程,对于子流程会重新跑一条新的flow_inst出来
        """
        # 把这条node的状态和结果都重置并保存
        node_instance.node_result = None
        node_instance.node_status = NodeStatus.Ready.value
        NodeInstanceDBHelper().save_this(model_to_dict(node_instance))
        # 查询出流程里所有节点,把当前节点之后的全部重置
        flow_instance_id = node_instance.flow_instance.id
        node_order_ = node_instance.node_order
        node_ins_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance_id}).order_by('-node_order')
        for node_ins in node_ins_list:
            if node_ins.node_order > node_order_:
                node_ins.node_result = None
                node_ins.node_status = NodeStatus.Ready.value
                NodeInstanceDBHelper().save_this(model_to_dict(node_ins))
            else:
                continue
        # 把对应的flow_instance的状态也要重置并保存
        flow_instance_ = node_instance.flow_instance
        flow_instance_.flow_result = None
        flow_instance_.flow_status = FlowStatus.Ready.value
        FlowInstanceDBHelper().save_this(model_to_dict(flow_instance_))
        self.flow_instance = flow_instance_
        return self



