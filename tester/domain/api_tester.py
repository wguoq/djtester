from typing import Union
import requests

from djtester.enums import ResponseProperty, TestResult
from tester.domain.base_tester import BaseTester, ApiRequestSender
from tester.domain.tt_models import *
from tester.utils import *


class ApiTester(BaseTester):

    def __init__(self, api_test_case: ApiTestCase, api_test_config: ApiCaseConfig = None):
        super().__init__(api_test_case)
        self.test_config = api_test_config if api_test_config else None
        self.api_response: requests.models.Response = None
        if self.test_config:
            self._config()

    def do_action(self):
        if self.test_case is None:
            raise Exception(f' api_test_case is None ')
        else:
            self.api_response = self._send_request()
        return self

    def verify_check_point(self):
        self.check_point_result_list = self._verify_check_point_list()
        return self

    def set_test_case_result(self):
        # 如果check_point_result_list是None就默认pass
        if self.check_point_result_list is None:
            self.test_case_result.case_result = TestResult.PASS.value
        else:
            # 默认所有check point 都要pass才行
            if self.check_point_is_all_pass():
                self.test_case_result.case_result = TestResult.PASS.value
            else:
                self.test_case_result.case_result = TestResult.FAIL.value
        self.test_case_result.test_case_id = self.test_case.test_case_id
        self.test_case_result.test_case_name = self.test_case.test_case_name
        self.test_case_result.case_message = self.check_point_result_list
        return self

    # 如果有test_config,把里面的字段替换到testcase里
    def _config(self):
        if self.test_config is None:
            pass

        if self.test_config.test_host:
            self.test_case.host = self.test_config.test_host

        if self.test_config.test_port:
            self.test_case.port = self.test_config.test_port

        if self.test_config.test_headers:
            self.test_case.headers = self.test_config.test_headers

        if self.test_config.test_cookies:
            self.test_case.cookies = self.test_config.test_cookies

    # 用requests发包
    def _send_request(self):
        if self.test_case.method in ['GET', 'get', 'Get']:
            return ApiRequestSender(self.test_case).send_get()
        elif self.test_case.method in ['POST', 'post', 'Post']:
            return ApiRequestSender(self.test_case).send_post()
        else:
            return None

    def _verify_check_point_list(self):
        check_list = self.test_case.check_list
        result_list = []
        # 如果没有check_list就返回None用来判断
        if check_list is None or len(check_list) == 0:
            return None
        for check in check_list:
            check_point_type = check.get('check_point_type')
            check_point_name = check.get('check_point_name')
            a = False
            if check_point_type == 'ApiCheckPoint':
                a = self._verify_check_point(check.get('check_point'))
                print(a)
            elif check_point_type == 'ApiJsonSchemaCheckPoint':
                a = self._verify_json_schema_check_point(check.get('check_point'))
            if a:
                result = TestCheckPointResult(check_point_name=check_point_name,
                                              check_point_result=TestResult.PASS.value)
            else:
                result = TestCheckPointResult(check_point_name=check_point_name,
                                              check_point_result=TestResult.FAIL.value)
            result_list.append(result)
        return result_list

    def _verify_check_point(self, check_point):
        print(check_point)
        cp = ApiCheckPoint(**check_point)
        response_property = cp.response_property
        rule = cp.rule
        operator_ = cp.operator
        expect = cp.expect
        check_target = self._get_check_target(response_property, rule)
        print(check_target)
        if check_target:
            if operator_ == 'eq':
                return operator.eq(str(check_target), str(expect))
            elif operator_ == 'ne':
                return operator.ne(str(check_target), str(expect))
        else:
            return False


    def _verify_json_schema_check_point(self, check_point):
        # todo
        return True

    def _get_check_target(self, response_property, rule):
        # 从response里面取出需要验证的属性
        if response_property is None or len(response_property) == 0:
            return None
        else:
            check_target = self._get_response_property(response_property)

        # 从response的属性里面取出需要的值
        if rule is None or len(rule) == 0:
            return check_target
        else:
            # 如果有__就判断是需要使用规则匹配json的值
            if '__' in rule:
                return get_json_value(check_target, rule)
            else:
                return check_target.get(rule)

    def _get_response_property(self, response_property: str):
        if response_property == ResponseProperty.STATUS_CODE.value:
            return self.api_response.status_code
        elif response_property == ResponseProperty.HEADERS.value:
            return self.api_response.headers
        elif response_property == ResponseProperty.COOKIES.value:
            return self.api_response.cookies
        elif response_property == ResponseProperty.JSON.value:
            try:
                return self.api_response.json()
            except Exception as e:
                print(f'使用response.json() error \n{e}')
                return {}
        else:
            raise Exception(f'只支持取出status_code,headers,cookies,json')
