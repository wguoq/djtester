import json
import datetime
# Create your tests here.


from django.test import TestCase

from .service import *

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
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "200"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheck",
            "check_point_name": "json_schema",
            "check_point": {
                "json_schema": ""
            }
        }
    ],
}

case2 = {
    "test_case_type": "api",
    "tc_identity": {
        "test_case_id": "tc1628342459",
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
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "200"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheck",
            "check_point_name": "json_schema",
            "check_point": {
                "json_schema": ""
            }
        }
    ],
}


class TestTC(TestCase):

    def test_TcCaseService(self):
        print(f'1111111111111\n获取一个Enums:TcEnums.case_type()==========\n{TestCaseEnums.case_type()}')

        api_case = TestCaseService.new.api_test_case()
        print(f'222222222222\n初始化一个api_case======\n{json.dumps(api_case)}')

        # 写入
        new_case12 = TestCaseService.save([case1, case2])
        print(f'33333333333333\n写入2条所有字段都是新的且完整的 new_case12=======\n{new_case12}')
        # 查询
        all_case = TestCaseService.query.get_all()
        print(f'44444444444444\nget all查询全部case ======\n{all_case}')

        case_pk1 = TestCaseService.query.get_by_pk(1)
        print(f'555555555555\nget pk查询case =======\n{json.dumps(case_pk1)}')

        case_id12 = TestCaseService.query.filter_by_case_id(['tc1622690409', 'tc1628342459'])
        print(f'666666666666\nfilter case_id查询case12 =======\n{case_id12}')

        case_name1 = TestCaseService.query.filter_by_case_name(['test_case_name002'])
        print(f'7777777777\nfilter case_name查询case1 =======\n{case_name1}')

        case_filter_identity = TestCaseService.query.filter_by({'tc_identity': 1})
        print(f'8888888888\nfilter tc_identity字段的值查询case =======\n{case_filter_identity}')

        case_filter_action = TestCaseService.query.filter_by({'tc_action': 1})
        print(f'9999999999\nfilter tc_action字段的值查询case =======\n{case_filter_action}')

        case_filter_data = TestCaseService.query.filter_by({'tc_data': 1})
        print(f'10.10.10.10.10\nfilter tc_data字段的值查询case =======\n{case_filter_data}')

        case_filter_check = TestCaseService.query.filter_by({'tc_check_list': 1})
        print(f'11.11.11.11\nfilter tc_check字段的值查询case =======\n{case_filter_check}')

        identity1 = TestCaseIdentityService.get_by_pk(1)
        print(f'12.12.12.12.12\nget identity 1 =======\n{identity1}')

        action1 = TestCaseActionService.get_by_pk(1)
        print(f'13.13.13.13.13\nget action 1 =======\n{action1}')

        data1 = TestCaseDataService.get_by_pk(1)
        print(f'14.14.14.14.14\nget data 1 =======\n{data1}')

        checkpoint1 = TestCaseCheckPointService.get_by_pk(1)
        print(f'15.15.15.15.15\nget checkpoint 1 =======\n{checkpoint1}')

        # 写入
        case3 = {
            "test_case_type": "api",
            "tc_identity": {
                "test_case_id": "tc1627690409",
                "test_case_name": "test_case_name03"
            },
            "tc_action": {
                "id": 2
            },
            "tc_data": {
                "id": 2

            },
            "tc_check_list": [
                {
                    "id": 1
                },
                {
                    "id": 2
                }

            ],
        }
        new_case3 = TestCaseService.save([case3])
        print(f'16.16.16.16.\n写入新case使用已存在的外键 =======\n{new_case3}')
        case4 = {
            "test_case_type": "api",
            "tc_identity": {
                "test_case_id": "tc1557690409",
                "test_case_name": "test_case_name04"
            }
        }
        new_case4 = TestCaseService.save([case4])
        print(f'17.17.17.17\n写入只写了名字的新case =======\n{new_case4}')
        # 修改
        case4_1 = {
            "id": 4,
            "tc_identity": {
                "id": 4,
                "test_case_name": "test_case_name444444444"
            },
        }
        new_case4_1 = TestCaseService.save([case4_1])
        new_name = TestCaseIdentityService.get_by_pk(4)
        print(f'18.18.18.18.18\n只修改test_case_name =======\n{new_case4_1}\n{new_name}')
        case4_2 = {
            "id": 4,
            "tc_action": {
                "id": 1
            },
        }
        new_case4_2 = TestCaseService.save([case4_2])
        print(f'19.19.19.19\n只修改tc_action字段 =======\n{new_case4_2}')
        case4_3 = {
            "id": 4,
            "tc_data": {
                "id": 1
            },
        }
        new_case4_3 = TestCaseService.save([case4_3])
        print(f'20.20.20.20.20\n只修改tc_data字段 =======\n{new_case4_3}')
        case4_4 = {
            "id": 4,
            "tc_check_list": [
                {
                    "id": 1
                }
            ],
        }
        new_case4_4 = TestCaseService.save([case4_4])
        print(f'21.21.21.21\n只修改tc_check字段 =======\n{new_case4_4}')

