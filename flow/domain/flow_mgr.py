from django.db import transaction
from django.forms import model_to_dict
from flow.domain.enums import FlowStatus, NodeStatus, FlowType
from flow.domain.flow_runner import *
from flow.models import Flow_Instance, Flow_Design, Node_Instance
from flow.repositories import FlowInstanceDBHelper, NodeInstanceDBHelper, FlowNodeDesignOderDBHelper


class FlowMgr:

    @staticmethod
    @transaction.atomic
    def instance_flow_design(flow_design_id, flow_data: dict = None) -> Flow_Instance:
        node_list = FlowNodeDesignOderDBHelper().filter_by({'flow_design_id': flow_design_id})
        if node_list is None or node_list.count() == 0:
            raise Exception("node_list 为空，不能实例化")
        # 保存 flow_instance
        if flow_data is None:
            flow_data = {}
        fi = {'flow_design': flow_design_id,
              'flow_data': flow_data}
        flow_instance = FlowInstanceDBHelper().save_this(fi)
        # 查询出 node_list 保存 node_instance
        for node in node_list:
            node_design = node.node_design
            node_func_name = node_design.node_func_code
            node_func_data = node_design.node_func_data
            node_order = node.node_order
            ni = {'node_design': node_design,
                  'node_func_name': node_func_name,
                  'node_func_data': node_func_data,
                  'node_order': node_order,
                  'flow_instance': flow_instance}
            NodeInstanceDBHelper().save_this(ni)
        return flow_instance

    @staticmethod
    def run_flow_instance(flow_instance: Flow_Instance) -> Flow_Instance:
        # 先判断流程状态能不能运行
        if flow_instance.flow_status in [FlowStatus.Finish.value, FlowStatus.Stop.value, FlowStatus.Cancelled.value]:
            print(f'流程状态是 {flow_instance.flow_status} 不运行; flow_instance_id = {flow_instance.id}')
            return flow_instance
        else:
            flow_type = flow_instance.flow_design.fw_type
            if flow_type == FlowType.Serial.value:
                flow_instance = SerialFlowRunner.run(flow_instance).flow_instance
            elif flow_type == FlowType.Parallel.value:
                raise Exception(f'并行的没写')
            else:
                raise Exception(f'无法识别的 flow_type = {flow_type},serial=串行;parallel=并行')
            # 保存
            FlowInstanceDBHelper().save_this(model_to_dict(flow_instance))
            return flow_instance

    @staticmethod
    @transaction.atomic
    def rollback_to_node(node_instance: Node_Instance) -> Flow_Instance:
        """
        只能回滚流程和节点的执行结果和状态,不能回滚flow_data和node_data
        也无法回滚子流程,对于子流程会重新跑一条新的flow_inst出来
        """
        # 把这条node的状态和结果都重置并保存
        node_instance.node_result = None
        node_instance.node_status = NodeStatus.Pending.value
        NodeInstanceDBHelper().save_this(model_to_dict(node_instance))
        # 查询出流程里所有节点,把当前节点之后的全部重置
        flow_instance_id = node_instance.flow_instance.id
        node_order_ = node_instance.node_order
        node_ins_list = NodeInstanceDBHelper().filter_by({'flow_instance_id': flow_instance_id}).order_by('-node_order')
        for node_ins in node_ins_list:
            if node_ins.node_order > node_order_:
                node_ins.node_result = None
                node_ins.node_status = NodeStatus.Pending.value
                NodeInstanceDBHelper().save_this(model_to_dict(node_ins))
            else:
                continue
        # 把对应的flow_instance的状态也要重置并保存
        flow_instance_ = node_instance.flow_instance
        flow_instance_.flow_result = None
        flow_instance_.flow_status = FlowStatus.Running.value
        FlowInstanceDBHelper().save_this(model_to_dict(flow_instance_))
        return flow_instance_
