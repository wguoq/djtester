from typing import Union
import requests

from djtester.enums import ResponseProperty, TestResult
from tester.domain.base_tester import BaseTester, ApiRequestSender
from tester.domain.tt_models import *
from tester.utils import *


class ApiTester(BaseTester):

    def __init__(self):
        super().__init__()
        self._api_response: requests.models.Response = None

    def run(self, test_case: ApiTestCase, test_case_config: ApiCaseConfig):
        if test_case is None:
            raise Exception(f' test_case is None ')
        test_case = self._config(test_case, test_case_config)
        self._api_response = self._send_request(test_case)
        check_point_result_list = self._verify_check_point_list(test_case)
        self._set_test_result(test_case, check_point_result_list)
        return self

    def _set_test_result(self, test_case, check_point_result_list):
        """
        决定测试结果,默认所有check point都要pass才行
        """
        # 如果check_point_result_list是None就默认pass
        if check_point_result_list is None:
            self.test_case_result.case_result = TestResult.PASS.value
        else:
            # 默认所有check point 都要pass才行
            if self._check_point_is_all_pass(check_point_result_list):
                self.test_case_result.case_result = TestResult.PASS.value
            else:
                self.test_case_result.case_result = TestResult.FAIL.value
        self.test_case_result.test_case_id = test_case.test_case_id
        self.test_case_result.test_case_name = test_case.test_case_name
        self.test_case_result.case_message = check_point_result_list
        return self

    # 如果有 test_case_config,把里面的字段替换到 testcase 里
    @staticmethod
    def _config(test_case, test_case_config):
        if test_case_config is None:
            return test_case
        if test_case_config.test_host:
            test_case.test_case.host = test_case_config.test_host

        if test_case_config.test_port:
            test_case.test_case.port = test_case_config.test_port

        if test_case_config.test_headers:
            test_case.test_case.headers = test_case_config.test_headers

        if test_case_config.test_cookies:
            test_case.test_case.cookies = test_case_config.test_cookies
        return test_case

    # 用requests发包
    @staticmethod
    def _send_request(test_case):
        if test_case.method in ['GET', 'get', 'Get']:
            return ApiRequestSender(test_case).send_get()
        elif test_case.method in ['POST', 'post', 'Post']:
            return ApiRequestSender(test_case).send_post()
        else:
            return None

    # 验证check_point_list
    def _verify_check_point_list(self, test_case):
        check_list = test_case.check_list
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

    # 验证check_point
    def _verify_check_point(self, check_point):
        cp = ApiCheckPoint(**check_point)
        response_property = cp.response_property
        rule = cp.rule
        operator_ = cp.operator
        expect = cp.expect
        check_target = self._get_check_target(response_property, rule)
        if check_target:
            if operator_ == 'eq':
                return operator.eq(str(check_target), str(expect))
            elif operator_ == 'ne':
                return operator.ne(str(check_target), str(expect))
        else:
            return False

    # 验证check_point_json_schema
    def _verify_json_schema_check_point(self, check_point):
        # todo
        return True

    # 根据配置的rule来获取josn里面的check_target
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
            # 如果rule里面有__就判断是需要使用规则获取
            if '__' in rule:
                return get_json_value(check_target, rule)
            else:
                return check_target.get(rule)

    # 获取response里面的返回值
    def _get_response_property(self, response_property: str):
        if response_property == ResponseProperty.STATUS_CODE.value:
            return self._api_response.status_code
        elif response_property == ResponseProperty.HEADERS.value:
            return self._api_response.headers
        elif response_property == ResponseProperty.COOKIES.value:
            return self._api_response.cookies
        elif response_property == ResponseProperty.JSON.value:
            try:
                return self._api_response.json()
            except Exception as e:
                print(f'使用response.json() error \n{e}')
                return {}
        else:
            raise Exception(f'只支持取出status_code,headers,cookies,json')
