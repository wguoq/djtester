from django.forms import model_to_dict

# Create your tests here.

from django.test import TestCase

from .domain.node_runner import NodeRunner, NodeMgr
from .repositories import *


class Test_Flow(TestCase):
    def test_flow_design(self):
        node_design_1 = {
            'id': None,
            'code': 'nd07675770',
            'node_name': '节点设计1',
            'node_type': 'test123',
            'node_data_type': 'test_config',
            'node_start_rule': {},
            'version': 1,
            'version_status': 0,
            'created_time': None,
            'modified_time': None,
            'del_flag': None,
        }

        node_status_rule_1 = {
            'id': None,
            'node_status': 'wait',
            'operator': 'eq',
            'return_result': 'wait',
            'node_design': 1,
        }

        node_status_rule_2 = {
            'id': None,
            'node_status': 'finish',
            'operator': 'eq',
            'return_result': 'pass',
            'node_design': 1,
        }
        node_status_rule_3 = {
            'id': None,
            'node_status': 'stop',
            'operator': 'eq',
            'return_result': 'fail',
            'node_design': 1,
        }

        node_instance_1 = {
            'id': None,
            'code': '31242535',
            'node_design': 1,
            'node_data': {},
            'node_status': None,
            'node_result': None,
        }

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
        nd1_node_status_rule_set_all = query_nd1.node_status_rule_set.all()
        for a in nd1_node_status_rule_set_all:
            print(model_to_dict(a))
            print(a.node_status)
        print(f'==== add node_instance_1 ====')
        ni1 = NodeInstanceDBHelper().save_this(node_instance_1)
        print(model_to_dict(ni1))
        print(f'==== run ni1 ====')
        aa = NodeRunner().run(ni1, {})
        print('==== new node_instance ====')
        print(model_to_dict(aa.node_instance))
        print('==== new flow_data ====')
        print(aa.flow_data)
        print(f'==== NodeMgr run_node_instance ====')
        bb = NodeMgr().run_node_instance(ni1, {})
        print(bb.__dict__)
