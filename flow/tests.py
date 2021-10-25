from django.forms import model_to_dict

# Create your tests here.

from django.test import TestCase

from .domain.flow_mgr import FlowMgr
from .domain.node_mgr import NodeMgr
from .domain.node_runner import NodeInstanceRunner
from .repositories import *


class Test_Flow(TestCase):
    def test_flow_design(self):
        node_design_1 = {
            'id': None,
            'node_code': 'nd07675770',
            'node_name': '节点设计1',
            'node_type': 'test123',
            'node_data': {},
            'node_start_rule': {},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_1 = {
            'id': None,
            'status_rule_type': 'def',
            'status_operator': 'eq',
            'status_target': 'wait',
            'node_status': 'wait',
            'node_design': 1,
        }
        node_status_rule_2 = {
            'id': None,
            'status_rule_type': 'def',
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 1,
        }
        node_status_rule_3 = {
            'id': None,
            'status_rule_type': 'def',
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'stop',
            'node_design': 1,
        }

        flow_design_1 = {
            'id': None,
            'flow_code': 'fw432453212',
            'flow_name': 'flow_design_1',
            'flow_type': 'serial',
        }
        flow_node_design_oder_1 = {
            'id': None,
            'flow_design': 1,
            'node_order': 1,
            'node_design': 1,
        }
        flow_node_design_oder_2 = {
            'id': None,
            'flow_design': 1,
            'node_order': 2,
            'node_design': 1,
        }
        flow_node_design_oder_3 = {
            'id': None,
            'flow_design': 1,
            'node_order': 3,
            'node_design': 1,
        }
        flow_result_rule_1 = {
            'id': None,
            'result_rule_type': 'last_node_result',
            'result_rule_name': 'result_rule_name 1',
            'flow_result': 'ok',
            'result_rule_script': {},
            'flow_design': 1,
        }
        flow_status_rule_1 = {
            'id': None,
            'status_rule_type': 'last_node_status',
            'status_rule_name': 'status_rule_name 1',
            'flow_status': 'finish',
            'status_rule_script': {},
            'flow_design': 1
        }

        print(f'==== add flow_design_1 ==== ')
        FlowDesignDBHelper().save_this(flow_design_1)
        fd1 = FlowDesignDBHelper().get_by({'pk': 1})
        print(model_to_dict(fd1))

        # print(f'==== add flow_instance_1 ==== ')
        # FlowInstanceDBHelper().save_this(flow_instance_1)
        # fi1 = FlowInstanceDBHelper().get_by({'pk': 1})
        # print(model_to_dict(fi1))

        print(f'==== add node_design_1 ==== ')
        nd1 = NodeDesignDBHelper().save_this(node_design_1)
        print(model_to_dict(nd1))
        print(f'==== add node_status_rule_1 ==== ')
        nsr1 = NodeStatusRuleDBHelper().save_this(node_status_rule_1)
        print(model_to_dict(nsr1))
        print(f'==== add node_status_rule_2 ==== ')
        nsr2 = NodeStatusRuleDBHelper().save_this(node_status_rule_2)
        print(model_to_dict(nsr2))
        print(f'==== add node_status_rule_3 ==== ')
        nsr3 = NodeStatusRuleDBHelper().save_this(node_status_rule_3)
        print(model_to_dict(nsr3))
        print(f'==== query nd1 by pk ==== ')
        query_nd1 = NodeDesignDBHelper().get_by({'pk': 1})
        print(model_to_dict(query_nd1))
        print(f'==== query nd1 node_status_rule_set.all ==== ')
        nd1_node_status_rule_set_all = query_nd1.node_status_rule_set.all().order_by('-id')
        for a in nd1_node_status_rule_set_all:
            print(model_to_dict(a))

        # print(f'==== add node_instance_1 ====')
        # ni1 = NodeInstanceDBHelper().save_this(node_instance_1)
        # print(model_to_dict(ni1))

        # print(f'==== run ni1 ====')
        # aa = NodeInstanceRunner().run(ni1, {})
        # print('==== new node_instance ====')
        # print(model_to_dict(aa.new_node_instance))
        # print('==== new flow_data ====')
        # print(aa.return_data)
        # print(f'==== NodeMgr run_node_instance ====')
        # bb = NodeMgr().run_node_instance(ni1, {})
        # print(bb.__dict__)
        # print(f'==== query new_ni1 ====')
        # new_ni1 = NodeInstanceDBHelper().get_by({'pk': 1})
        # print(model_to_dict(new_ni1))

        print(f'==== add flow_node_design_oder_1 ====')
        flow_node_design_oder1 = FlowNodeDesignOderDBHelper().save_this(flow_node_design_oder_1)
        print(model_to_dict(flow_node_design_oder1))

        print(f'==== add flow_node_design_oder_2 ====')
        flow_node_design_oder2 = FlowNodeDesignOderDBHelper().save_this(flow_node_design_oder_2)
        print(model_to_dict(flow_node_design_oder2))

        print(f'==== add flow_node_design_oder_3 ====')
        flow_node_design_oder3 = FlowNodeDesignOderDBHelper().save_this(flow_node_design_oder_3)
        print(model_to_dict(flow_node_design_oder3))

        print(f'==== add flow_result_rule_1 ====')
        flow_result_rule1 = FlowResultRuleDBHelper().save_this(flow_result_rule_1)
        print(model_to_dict(flow_result_rule1))

        print(f'==== add flow_status_rule_1 ====')
        flow_status_rule1 = FlowStatusRuleDBHelper().save_this(flow_status_rule_1)
        print(model_to_dict(flow_status_rule1))

        print(f'==== instance_flow_design fd1 ====')
        fd1 = FlowDesignDBHelper().get_by({'pk': 1})
        FlowMgr().instance_flow_design(fd1)

        print(f'==== get flow_instance all ====')
        fi_all = FlowInstanceDBHelper().get_all()
        for fi in fi_all:
            print(model_to_dict(fi))
            print(f'==== run flow_instance {fi.flow_design.flow_name} ====')
            run_flow = FlowMgr().run_flow(fi)
            print(model_to_dict(run_flow.new_flow_instance))

        print(f'==== get node_instance all ====')
        ni_all = NodeInstanceDBHelper().get_all()
        for ni in ni_all:
            print(model_to_dict(ni))

