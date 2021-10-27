from djtester.enums import ResponseProperty, Operators
from tester.domain.base_tester import BaseTester, ApiRequestSender
from tester.domain.tt_models import *
from tester.utils import *


def get_response_property(api_response, response_property: str):
    if response_property == ResponseProperty.STATUS_CODE.value:
        return api_response.status_code
    elif response_property == ResponseProperty.HEADERS.value:
        return api_response.headers
    elif response_property == ResponseProperty.COOKIES.value:
        return api_response.cookies
    elif response_property == ResponseProperty.JSON.value:
        try:
            return api_response.json()
        except Exception as e:
            print(f'使用response.json() error \n{e}')
            return {}
    else:
        raise Exception(f'只支持取出status_code,headers,cookies,json')


def get_target_by_rule(data, rule):
    if rule is None or len(rule) == 0:
        return data
    else:
        # 如果rule里面有 __ 就判断是需要使用规则获取
        if '__' in rule:
            return get_json_value(data, rule)
        else:
            return data.get(rule)


class ApiTestVerifier:
    def __init__(self, check_data):
        self.check_point_result = None
        self.check_point_type = check_data.get('check_point_type')
        self.check_point_name = check_data.get('check_point_name')
        self.check_point: dict = check_data.get('check_point')

    def verify(self, api_response):
        if self.check_point_type == 'ApiCheckPoint':
            r = self._verify_str(api_response, self.check_point)
            self.check_point_result = r
            return self
        elif self.check_point_type == 'ApiJsonSchemaCheckPoint':
            r = self._verify_schema(api_response, self.check_point)
            self.check_point_result = r
            return self
        else:
            raise Exception(f'无法识别的 check_point_type = {self.check_point_type}')

    @staticmethod
    def _verify_str(api_response, check_point):
        response_property = check_point.get('response_property')
        rule = check_point.get('rule')
        operator_ = check_point.get('operator')
        expect = check_point.get('expect')
        data = get_response_property(api_response, response_property)
        target = get_target_by_rule(data, rule)
        if operator_ == Operators.EQ.value:
            return operator.eq(str(target), str(expect))
        elif operator_ == Operators.NE.value:
            return operator.ne(str(target), str(expect))
        else:
            raise Exception(f'无法识别的 check_point.operator = {operator_}')

    @staticmethod
    def _verify_schema(api_response, check_point):
        # todo
        return True


class ApiTester(BaseTester):
    def __init__(self):
        super().__init__()

    def run(self, test_case: ApiTestCase, test_case_config: ApiCaseConfig):
        self.test_case_id = test_case.test_case_id
        self.test_case_name = test_case.test_case_name
        # 配置 test_case_config
        test_case = self._config(test_case, test_case_config)
        # 调用requests
        api_response = self._send_request(test_case)
        # 验证
        check_list = test_case.check_list
        if check_list is None or len(check_list) == 0:
            # 如果没有check_list就默认不需要验证直接通过
            self.test_case_result = TestResult.PASS.value
            self.message = '没有 check_list,默认为 pass'
            return self
        else:
            check_point_result_list = []
            for check_data in check_list:
                r = ApiTestVerifier(check_data).verify(api_response)
                check_point_result_list.append(r)
            # 判断测试结果
            if self._check_point_is_all_pass(check_point_result_list):
                self.test_case_result = TestResult.PASS.value
            else:
                self.test_case_result = TestResult.FAIL.value
            for res in check_point_result_list:
                self.check_point_result_list.append(res.__dict__)
            self.message = '测试完成'
            return self

    @staticmethod
    def _config(test_case: ApiTestCase, test_case_config: ApiCaseConfig):
        # 如果有 test_case_config,把里面的字段替换到 testcase 里

        if test_case_config.test_host:
            test_case.host = test_case_config.test_host

        if test_case_config.test_port:
            test_case.port = test_case_config.test_port

        if test_case_config.test_headers:
            test_case.headers = test_case_config.test_headers

        if test_case_config.test_cookies:
            test_case.cookies = test_case_config.test_cookies
        return test_case

    @staticmethod
    def _send_request(test_case):
        if test_case.method in ['GET', 'get', 'Get']:
            return ApiRequestSender(test_case).send_get()
        elif test_case.method in ['POST', 'post', 'Post']:
            return ApiRequestSender(test_case).send_post()
        else:
            return None

    @staticmethod
    def _check_point_is_all_pass(check_point_result_list):
        for res in check_point_result_list:
            res: ApiTestVerifier = res
            if res.check_point_result:
                continue
            else:
                return TestResult.FAIL.value
        return TestResult.PASS.value

