from typing import Union
import requests

from tester.domain.tt_enums import *
from tester.domain.tt_models import *
from tester.utils import str_check, json_schema_check


class ApiTester:

    def __init__(self, api_test_case: dict, api_test_config: dict = None):
        self.test_config = ApiCaseConfig(**api_test_config) if api_test_config else None
        self.test_case = ApiTestCase(api_test_case) if api_test_case else None
        self._response: requests.models.Response
        self._test_result: Union[ApiCaseResult, None] = None

    def run_test_case(self):
        if self.test_case is None:
            raise Exception(f' test_case is None')
        else:
            self._test_result = self._config()._send_request()._verify()
        return self

    @property
    def get_response(self):
        return self._response

    @property
    def get_test_result(self):
        return self._test_result

    # 如果有配置,把里面的字段替换到testcase里
    def _config(self):
        if self.test_config is None:
            return self

        if self.test_config.test_host is None:
            pass
        else:
            self.test_case.host = self.test_config.test_host

        if self.test_config.test_port is None:
            pass
        else:
            self.test_case.port = self.test_config.test_port

        if self.test_config.test_headers is None:
            pass
        else:
            self.test_case.headers = self.test_config.test_headers

        if self.test_config.test_cookies is None:
            pass
        else:
            self.test_case.cookies = self.test_config.test_cookies

        return self

    # 用requests发包
    def _send_request(self):
        payload = self.test_case.payload
        if self.test_case.method in ['GET', 'get', 'Get']:
            try:
                self._response = requests.get(url=self.test_case.url,
                                              timeout=payload['timeout'],
                                              allow_redirects=payload['allow_redirects'],
                                              verify=payload['verify'],
                                              headers=payload['headers'],
                                              cookies=payload['cookies'],
                                              data=payload['data'],
                                              json=payload['json_data'],
                                              files=payload['files'])
                return self
            except Exception as e:
                raise Exception(f'requests.get error:\n {e}')

        elif self.test_case.method in ['POST', 'post', 'Post']:
            try:
                self._response = requests.post(url=self.test_case.url,
                                               timeout=payload['timeout'],
                                               allow_redirects=payload['allow_redirects'],
                                               verify=payload['verify'],
                                               headers=payload['headers'],
                                               cookies=payload['cookies'],
                                               data=payload['data'],
                                               json=payload['json_data'],
                                               files=payload['files'])
                return self
            except Exception as e:
                raise Exception(f'requests.post error:\n {e}')
        else:
            raise Exception(f'只支持GET和POST')

    def _verify(self):
        result_list: list[dict] = []
        test_result = ""
        test_message = []
        check_list = self.test_case.check_list

        # 遍历check_list逐一验证
        for check in check_list:
            # 把check_point内容拆解出来
            check_point_type = check.get('check_point_type')
            name = check.get('check_point_name')
            check_point = check.get('check_point')
            response_property = check_point.get('response_property')
            property_key = check_point.get('property_key')
            operator = check_point.get('operator')
            expect = check_point.get('expect')
            json_schema = check_point.get('json_schema')

            # 判断check类型
            if check_point_type == 'ApiStrCheck':
                check_target = self._get_check_target(response_property, property_key)
                a = self._do_str_check(name, check_target, operator, expect)
                result_list.append(a)
            elif check_point_type == 'ApiJsonSchemaCheck':
                # todo 还没测试不知道行不行
                check_target = self._get_check_target(ResponseProperty.JSON.value, None)
                b = self._do_json_schema_check(name, check_target, json_schema)
                result_list.append(b)
            else:
                raise Exception(f'不支持的check_point_type {check_point_type}')

        # 只有所有check都是PASS才算测试通过
        for result in result_list:
            if result.get('result') == 'fail':
                test_result = 'fail'
                break
            elif result.get('result') == 'pass':
                test_result = 'pass'
            else:
                test_result = 'fail'

        # 把每个check的msg合并到一起
        for result in result_list:
            test_message.append(result['msg'])

        return dict(test_case_id=self.test_case.test_case_id,
                    test_case_name=self.test_case.test_case_name,
                    test_result=test_result,
                    test_message=test_message)

    @staticmethod
    def _do_str_check(name, check_target, check_operator, check_expect) -> dict:
        try:
            if str_check(check_target, check_operator, check_expect):
                return {'result': 'pass', 'msg': f'{name} pass'}
            else:
                return {'result': 'fail', 'msg': f'{name} fail'}
        except Exception as e:
            return {'result': 'fail', 'msg': f'{name} fail {e}'}

    @staticmethod
    def _do_json_schema_check(name, check_target, json_schema) -> dict:
        # todo 没测,不知道对不对
        try:
            if json_schema_check(check_target, json_schema):
                return {'result': 'pass', 'msg': f'{name} pass'}
            else:
                return {'result': 'fail', 'msg': f'{name} fail'}
        except Exception as e:
            return {'result': 'fail', 'msg': f'{name} fail {e}'}

    def _get_check_target(self, response_property, property_key):
        # 从response里面取出需要验证的属性
        if response_property is None or len(response_property) == 0:
            return None
        else:
            check_target = self._get_response_property(response_property)
        # 从response的属性里面取出需要的值
        # todo 还没测试
        if property_key is None or len(property_key) == 0:
            return check_target
        else:
            return check_target.get(property_key)

    def _get_response_property(self, response_property: str):
        if response_property == ResponseProperty.STATUS_CODE.value:
            return self._response.status_code
        elif response_property == ResponseProperty.HEADERS.value:
            return self._response.headers
        elif response_property == ResponseProperty.COOKIES.value:
            return self._response.cookies
        elif response_property == ResponseProperty.JSON.value:
            try:
                return self._response.json()
            except Exception as e:
                # raise Exception(f'取出response里的json出错 {e}')
                return {}
        else:
            raise Exception(f'只支持取出status_code,headers,cookies,json')
