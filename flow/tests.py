import time
from django.forms import model_to_dict
from django.test import TestCase
from djtester.all_app_service import TestCaseService
from .domain.flow_mgr import FlowMgr
from .domain.flow_tools_man import check_flow_result, check_flow_status
from .domain.node_mgr import NodeMgr
from .repositories import *


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
                    "check_point_name": "status_code == 10086",
                    "check_point": {
                        "response_property": "status_code",
                        "rule": "",
                        "operator": "eq",
                        "expect": "10086"
                    }
                },
            ],
        }

        node_design_1 = {
            'id': None,
            'node_code': 'nd07675770',
            'node_name': '节点设计1 api_tester',
            'node_func_name': 'api_tester',
            'node_func_data': {'test_case_id': 1},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_1 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 1,
        }
        node_status_rule_2 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'skip',
            'node_design': 1,
        }

        node_design_2 = {
            'id': None,
            'node_code': 'nd076545770',
            'node_name': '节点设计2 api_tester',
            'node_func_name': 'api_tester',
            'node_func_data': {'test_case_id': 2},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_3 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 2,
        }
        node_status_rule_4 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'skip',
            'node_design': 2,
        }

        node_design_3 = {
            'id': None,
            'node_code': 'nd076545770',
            'node_name': '节点设计3 flow_runner',
            'node_func_name': 'flow_runner',
            'node_func_data': {'flow_design_id': 2},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_5 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 3,
        }
        node_status_rule_6 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'skip',
            'node_design': 3,
        }

        node_design_4 = {
            'id': None,
            'node_code': 'nd076545770',
            'node_name': '节点设计4 api_tester',
            'node_func_name': 'api_tester',
            'node_func_data': {'test_case_id': 1},
            'version': 1,
            'version_status': 0,
        }
        node_status_rule_7 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'pass',
            'node_status': 'finish',
            'node_design': 4,
        }
        node_status_rule_8 = {
            'id': None,
            'status_operator': 'eq',
            'status_target': 'fail',
            'node_status': 'skip',
            'node_design': 4,
        }

        node_start_rule_design_1 = {
            'rule_type': 'or',
            'rule_order': '1',
            'node_design': 4
        }
        node_start_rule_design_2 = {
            'rule_type': 'and',
            'rule_order': '1',
            'node_design': 4
        }

        node_start_rule_1 = {
            'rule_target': 'flow_data',
            'rule_where': 'flag',
            'rule_operator': 'eq',
            'rule_value': 'ok',
            'rule_design': 1
        }
        node_start_rule_2 = {
            'rule_target': 'flow_data',
            'rule_where': 'flag',
            'rule_operator': 'eq',
            'rule_value': 'pass',
            'rule_design': 1
        }
        node_start_rule_3 = {
            'rule_target': 'node_result',
            'rule_where': 1,
            'rule_operator': 'eq',
            'rule_value': 'pass',
            'rule_design': 2
        }
        node_start_rule_4 = {
            'rule_target': 'node_status',
            'rule_where': 1,
            'rule_operator': 'eq',
            'rule_value': "finish",
            'rule_design': 2
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
            'node_design': 2,
        }
        flow_node_design_oder_3 = {
            'id': None,
            'flow_design': 1,
            'node_order': 3,
            'node_design': 3,
        }
        flow_node_design_oder_4 = {
            'id': None,
            'flow_design': 1,
            'node_order': 4,
            'node_design': 4,
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

        flow_design_2 = {
            'id': None,
            'flow_code': 'fw4324535512',
            'flow_name': 'flow_design_2',
            'flow_type': 'serial',
        }
        flow_node_design_oder_5 = {
            'id': None,
            'flow_design': 2,
            'node_order': 1,
            'node_design': 1,
        }
        flow_result_rule_2 = {
            'id': None,
            'result_rule_type': 'last_node_result',
            'result_rule_name': 'result_rule_name 2',
            'flow_result': 'ok',
            'result_rule_script': {},
            'flow_design': 2
        }
        flow_status_rule_2 = {
            'id': None,
            'status_rule_type': 'last_node_status',
            'status_rule_name': 'status_rule_name 2',
            'flow_status': 'finish',
            'status_rule_script': {},
            'flow_design': 2
        }

        print(f'==== 新增 flow_design ==== ')
        flow_design_list = [flow_design_1, flow_design_2]
        for flow_design in flow_design_list:
            FlowDesignDBHelper().save_this(flow_design)
        fd1 = FlowDesignDBHelper().get_by({'pk': 1})
        print(model_to_dict(fd1))

        print(f'==== 新增 node_design ==== ')
        node_design_list = [node_design_1, node_design_2, node_design_3, node_design_4, ]
        for node_design in node_design_list:
            a = NodeDesignDBHelper().save_this(node_design)
            print(model_to_dict(a))

        print(f'==== 新增 node_status_rule ==== ')
        node_status_rule_list = [node_status_rule_1, node_status_rule_2, node_status_rule_3, node_status_rule_4,
                                 node_status_rule_5, node_status_rule_6, node_status_rule_7, node_status_rule_8,
                                 ]

        for node_status_rule in node_status_rule_list:
            a = NodeStatusRuleDBHelper().save_this(node_status_rule)
            print(model_to_dict(a))

        print(f'==== 新增 node_start_rule_design ==== ')
        node_start_rule_design_list = [node_start_rule_design_1, node_start_rule_design_2, ]

        for node_start_rule_design in node_start_rule_design_list:
            a = NodeStartRuleDesignDBHelper().save_this(node_start_rule_design)
            print(model_to_dict(a))

        print(f'==== 新增 node_start_rule ==== ')
        node_start_rule_list = [node_start_rule_1, node_start_rule_2, node_start_rule_3, node_start_rule_4, ]

        for node_start_rule in node_start_rule_list:
            a = NodeStartRuleDBHelper().save_this(node_start_rule)
            print(model_to_dict(a))

        print(f'==== 查询 nd1 by pk ==== ')
        query_nd1 = NodeDesignDBHelper().get_by({'pk': 1})
        print(model_to_dict(query_nd1))

        print(f'==== 查询 nd1 node_status_rule_set.all ==== ')
        nd1_node_status_rule_set_all = query_nd1.node_status_rule_set.all().order_by('-id')
        for a in nd1_node_status_rule_set_all:
            print(model_to_dict(a))

        test_case_servicer = TestCaseService().test_case_servicer()
        test_case_servicer().add(case1)
        test_case_servicer().add(case2)

        print(f'==== 新增 flow_node_design_oder ====')
        flow_node_design_oder_list = [flow_node_design_oder_1, flow_node_design_oder_2, flow_node_design_oder_3,
                                      flow_node_design_oder_4, flow_node_design_oder_5, ]
        for flow_node_design_oder in flow_node_design_oder_list:
            a = FlowNodeDesignOderDBHelper().save_this(flow_node_design_oder)
            print(model_to_dict(a))

        print(f'==== 新增 flow_result_rule ====')
        flow_result_rule_list = [flow_result_rule_1, flow_result_rule_2, ]
        for flow_result_rule in flow_result_rule_list:
            a = FlowResultRuleDBHelper().save_this(flow_result_rule)
            print(model_to_dict(a))

        print(f'==== 新增 flow_status_rule ====')
        flow_status_rule_list = [flow_status_rule_1, flow_status_rule_2]
        for flow_status_rule in flow_status_rule_list:
            a = FlowStatusRuleDBHelper().save_this(flow_status_rule)
            print(model_to_dict(a))

        print(f'==== instance_flow_design fd1 ====')
        fd1 = FlowDesignDBHelper().get_by({'pk': 1})
        flow_data = {'flag': 'ok'}
        FlowMgr().instance_flow_design(fd1, flow_data)

        print(f'==== 查询初始化的 node_instance all  ====')
        ni_all = NodeInstanceDBHelper().get_all()
        for ni in ni_all:
            print(model_to_dict(ni))

        print(f'==== 查询初始化的 flow_instance all ====')
        fi_all = FlowInstanceDBHelper().get_all()
        for fi in fi_all:
            print(model_to_dict(fi))
            print(f'==== 运行 flow_instance {fi.flow_design.flow_name} ====')
            s = time.time()
            print(s)
            run_flow = FlowMgr().run_flow_instance(fi)
            e = time.time()
            time_ = e-s
            print(e)
            print(f'==== time ==== {time_}')
            print(model_to_dict(run_flow.flow_instance))

        print(f'==== 查询运行的 node_instance all ====')
        ni_all = NodeInstanceDBHelper().get_all()
        for ni in ni_all:
            print(model_to_dict(ni))

        print(f'==== 查询运行的 flow_instance all ====')
        aaa = FlowInstanceDBHelper().get_all()
        for a in aaa:
            print(model_to_dict(a))

        print(f'==== 重新设置节点3的结果和状态 re_check_node_status to fail ====')
        a = NodeMgr().re_check_node_status('fail', 3)
        print(model_to_dict(a.node_instance))

        print(f'==== 回滚到3号节点 rollback_to_node  ====')
        node_ins_2 = NodeInstanceDBHelper().get_by({'pk': 3})
        a = FlowMgr().rollback_to_node(node_ins_2)
        print(model_to_dict(a.flow_instance))
        aaa = NodeInstanceDBHelper().get_all()
        for a in aaa:
            print(model_to_dict(a))
        flow_inst_1 = FlowInstanceDBHelper().get_by({'pk': 1})
        print(f'==== 重新运行flow_inst_1 ====')
        FlowMgr().run_flow_instance(flow_inst_1, {'name': '重新运行'})
        flow_inst_1 = FlowInstanceDBHelper().get_by({'pk': 1})
        print(model_to_dict(flow_inst_1))
        aaa = NodeInstanceDBHelper().get_all()
        result = 'node_result'
        for a in aaa:
            print(model_to_dict(a))
            print(a.__getattribute__(result))
        print('===============')
        print(check_flow_result(flow_inst_1))
        print(check_flow_status(flow_inst_1))
