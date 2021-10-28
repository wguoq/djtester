import requests
from djtester.enums import ResponseProperty, Operators, TestResult
from tester.domain.base_tester import BaseTester
from tester.utils import *
from pydantic import BaseModel


class ApiTestCase(object):
    def __init__(self, api_test_case: dict):
        self.id = api_test_case.get('id')
        self.test_case_type = api_test_case.get('test_case_type')
        # identity
        self.tc_identity = api_test_case.get('tc_identity')
        self.test_case_id = self.tc_identity.get('test_case_id')
        self.test_case_name = self.tc_identity.get('test_case_name')
        # action
        self.tc_action = api_test_case.get('tc_action')
        self.action_type = self.tc_action.get('action_type')
        self.action_name = self.tc_action.get('action_name')
        self.action = self.tc_action.get('action')
        self.method = self.action.get('method')
        self.protocol = self.action.get('protocol')
        self.host = self.action.get('host')
        self.port = self.action.get('port')
        self.path = self.action.get('path')
        # data
        self.tc_data = api_test_case.get('tc_data')
        self.data_type = self.tc_data.get('data_type')
        self.data_name = self.tc_data.get('data_name')
        self.data = self.tc_data.get('data')
        self.timeout = self.data.get('timeout')
        self.allow_redirects = self.data.get('allow_redirects')
        self.verify = self.data.get('verify')
        self.headers = self.data.get('headers')
        self.cookies = self.data.get('cookies')
        self.data = self.data.get('data')
        self.json_data = self.data.get('json_data')
        self.files = self.data.get('files')
        # check list
        self.check_list = api_test_case.get('tc_check_list')

    @property
    def url(self) -> str:
        url = self.protocol + '://' + self.host
        if self.port and len(self.port) >= 1:
            url = url + ':' + self.port
        else:
            pass
        if self.path and len(self.path) >= 1:
            url = url + self.path
        else:
            pass
        return url


class ApiTestConfig(BaseModel):
    config_name: str = None
    test_host: str = None
    test_port: str = None
    test_headers: dict = None
    test_cookies: dict = None


class ApiRequestSender(object):
    def __init__(self, data: ApiTestCase):
        self.data = data if data else None
        self.payload = {}
        if self.data:
            self.payload = dict(url=data.url,
                                timeout=data.timeout,
                                allow_redirects=data.allow_redirects,
                                verify=data.verify,
                                headers=data.headers,
                                cookies=data.cookies,
                                data=data.data,
                                json=data.json_data,
                                files=data.files)

    def send_get(self):
        try:
            a = requests.get(**self.payload)
            return a
        except Exception as e:
            raise Exception(f'requests.get error:\n {e}')

    def send_post(self):
        try:
            a = requests.post(**self.payload)
            return a
        except Exception as e:
            raise Exception(f'requests.post error:\n {e}')


class ApiTester(BaseTester):
    def __init__(self):
        self.check_point_result_list = []
        super().__init__()

    def run(self, test_case: ApiTestCase, test_case_config: ApiTestConfig):
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
            result_list = []
            for check_data in check_list:
                r = ApiCheckPointVerifier().verify(check_data, api_response)
                result_list.append(r)
            # 判断测试结果,填写返回值
            self.test_case_result = self._verify_test_case_result(result_list)
            for result in result_list:
                self.check_point_result_list.append(result.__dict__)
            self.message = '测试完成'
            return self

    @staticmethod
    def _config(test_case: ApiTestCase, test_case_config: ApiTestConfig):
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

    def _verify_test_case_result(self, result_list):
        if self._all_check_point_is_true(result_list):
            return TestResult.PASS.value
        else:
            return TestResult.FAIL.value

    @staticmethod
    def _all_check_point_is_true(check_point_result_list):
        for result in check_point_result_list:
            if result.check_point_result:
                continue
            else:
                return False
        return True


class ApiCheckPointVerifier:
    def __init__(self):
        self.check_point_result = None
        self.check_point_type = None
        self.check_point_name = None

    def verify(self, check_data, api_response):
        self.check_point_type = check_data.get('check_point_type')
        self.check_point_name = check_data.get('check_point_name')
        check_point: dict = check_data.get('check_point')
        response_property = check_point.get('response_property')
        rule = check_point.get('rule')
        operator_ = check_point.get('operator')
        expect = check_point.get('expect')
        json_schema = check_point.get('json_schema')
        data = self._get_response_data(api_response, response_property)
        target = self._get_target_by_rule(data, rule)

        if self.check_point_type == 'ApiCheckPoint':
            r = self._verify_str(operator_, target, expect)
            self.check_point_result = r
            return self
        elif self.check_point_type == 'ApiJsonSchemaCheckPoint':
            r = self._verify_schema(target, json_schema)
            self.check_point_result = r
            return self
        else:
            raise Exception(f'无法识别的 check_point_type = {self.check_point_type}')

    @staticmethod
    def _verify_str(operator_, target, expect):
        if operator_ == Operators.EQ.value:
            return operator.eq(str(target), str(expect))
        elif operator_ == Operators.NE.value:
            return operator.ne(str(target), str(expect))
        else:
            raise Exception(f'无法识别的 check_point.operator = {operator_}')

    @staticmethod
    def _verify_schema(data, json_schema):
        # todo
        return check_json_schema(data, json_schema)

    @staticmethod
    def _get_response_data(api_response, response_property: str):
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

    @staticmethod
    def _get_target_by_rule(data, rule):
        if rule is None or len(rule) == 0:
            return data
        else:
            # 如果rule里面有 __ 就判断是需要使用规则获取
            if '__' in rule:
                return get_json_value(data, rule)
            else:
                return data.get(rule)
