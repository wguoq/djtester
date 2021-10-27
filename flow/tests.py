from django.forms import model_to_dict
from django.test import TestCase

from djtester.all_app_service import TestCaseService
from .domain.flow_mgr import FlowMgr
from .repositories import *
from .service import NodeFuncRunFLow


class Test_Flow(TestCase):
    def test_flow_design(self):

        case1 = {
            "test_case_type": "api",
            "tc_identity": {
                "test_case_id": "tc1622690409",
                "test_case_name": "test_case_name001"
            },
            "tc_action": {
                "action_type": "ApiAction",
                "action_name": "get index",
                "action": {
                    "method": "get",
                    "protocol": "http",
                    "host": "127.0.0.1",
                    "port": "8000",
                    "path": ""
                }
            },
            "tc_data": {
                "data_type": "ApiParams",
                "data_name": "defApiParams",
                "data": {
                    "timeout": 120,
                    "allow_redirects": True,
                    "verify": False,
                    "headers": {

                    },
                    "cookies": {

                    },
                    "data": {

                    },
                    "json_data": {

                    },
                    "files": {

                    }
                }
            },
            "tc_check_list": [
                {
                    "check_point_type": "ApiCheckPoint",
                    "check_point_name": "status_code == 200",
                    "check_point": {
                        "response_property": "status_code",
                        "rule": "",
                        "operator": "eq",
                        "expect": "200"
                    }
                },
            ],
        }
        case2 = {
            "test_case_type": "api",
            "tc_identity": {
                "test_case_id": "tc18765690409",
                "test_case_name": "test_case_name002"
            },
            "tc_action": {
                "action_type": "ApiAction",
                "action_name": "get index",
                "action": {
                    "method": "get",
                    "protocol": "http",
                    "host": "127.0.0.1",
                    "port": "8000",
                    "path": ""
                }
            },
            "tc_data": {
                "data_type": "ApiParams",
                "data_name": "defApiParams",
                "data": {
                    "timeout": 120,
                    "allow_redirects": True,
                    "verify": False,
                    "headers": {

                    },
                    "cookies": {

                    },
                    "data": {

                    },
                    "json_data": {

                    },
                    "files": {

                    }
                }
            },
            "tc_check_list": [
                {
                    "check_point_type": "ApiCheckPoint",
                    "check_point_name": "status_code == 200",
                    "check_point": {
                        "response_property": "status_code",
                        "rule": "",
                        "operator": "nq",
                        "expect": "200"
                    }
                },
            ],
        }

        node_design_1 = {
            'id': None,
            'node_code': 'nd07675770',
            'node_name': '节点设计1',
            'node_func_name': 'api_tester',
            'node_func_data': {'test_case_id': 1},
            'node_start_rule': {},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_1 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'wait',
            'node_status': 'wait',
            'node_design': 1,
        }
        node_status_rule_2 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 1,
        }
        node_status_rule_3 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'stop',
            'node_design': 1,
        }

        node_design_2 = {
            'id': None,
            'node_code': 'nd076545770',
            'node_name': '节点设计2',
            'node_func_name': 'api_tester',
            'node_func_data': {'test_case_id': 2},
            'node_start_rule': {},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_4 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'wait',
            'node_status': 'wait',
            'node_design': 2,
        }
        node_status_rule_5 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 2,
        }
        node_status_rule_6 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'stop',
            'node_design': 2,
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
            'node_design': 2,
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

        print(f'==== add node_design_1,2 ==== ')
        nd1 = NodeDesignDBHelper().save_this(node_design_1)
        nd2 = NodeDesignDBHelper().save_this(node_design_2)
        print(model_to_dict(nd1))
        print(model_to_dict(nd2))
        print(f'==== add node_status_rule_1 ==== ')
        nsr1 = NodeStatusRuleDBHelper().save_this(node_status_rule_1)
        print(model_to_dict(nsr1))
        print(f'==== add node_status_rule_2 ==== ')
        nsr2 = NodeStatusRuleDBHelper().save_this(node_status_rule_2)
        print(model_to_dict(nsr2))
        print(f'==== add node_status_rule_3 ==== ')
        nsr3 = NodeStatusRuleDBHelper().save_this(node_status_rule_3)
        print(model_to_dict(nsr3))
        print(f'==== add node_status_rule_4 ==== ')
        nsr4 = NodeStatusRuleDBHelper().save_this(node_status_rule_4)
        print(model_to_dict(nsr4))
        print(f'==== add node_status_rule_5 ==== ')
        nsr5 = NodeStatusRuleDBHelper().save_this(node_status_rule_5)
        print(model_to_dict(nsr5))
        print(f'==== add node_status_rule_6 ==== ')
        nsr6 = NodeStatusRuleDBHelper().save_this(node_status_rule_6)
        print(model_to_dict(nsr6))
        print(f'==== query nd1 by pk ==== ')
        query_nd1 = NodeDesignDBHelper().get_by({'pk': 1})
        print(model_to_dict(query_nd1))
        print(f'==== query nd1 node_status_rule_set.all ==== ')
        nd1_node_status_rule_set_all = query_nd1.node_status_rule_set.all().order_by('-id')
        for a in nd1_node_status_rule_set_all:
            print(model_to_dict(a))

        test_case_servicer = TestCaseService().test_case_servicer()
        test_case_servicer().add(case1)
        test_case_servicer().add(case2)

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

        print(f'==== get node_instance all not run ====')
        ni_all = NodeInstanceDBHelper().get_all()
        for ni in ni_all:
            print(model_to_dict(ni))

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

        a = NodeFuncRunFLow().node_func_data_model()
        print(a)