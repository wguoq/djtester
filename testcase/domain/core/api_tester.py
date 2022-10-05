import json
from testcase.domain.core.utils import *
from testcase.domain.sub.http_request import *
from testcase.repositories import *


class ApiTester:
    @staticmethod
    def _set_data_config(data: ApiTestData, data_config: dict):
        data.host = data_config.get('host') or data.host
        data.port = data_config.get('port') if data_config.get('port') is not None else data.port
        data.timeout = data_config.get('timeout') if data_config.get('timeout') is not None else data.timeout
        data.headers = data_config.get('headers') or data.headers
        data.cookies = data_config.get('cookies') or data.cookies
        data.data = data_config.get('data') or data.data
        return data

    @staticmethod
    def _http_request(api: TestApi, data: ApiTestData):
        protocol = api.protocol or 'http'
        if data.host is None:
            raise Exception(f'host 不能为空')
        else:
            url = protocol + '://' + str(data.host)
        if data.port:
            url = url + ':' + str(data.port)
        if api.path and len(api.path) > 0:
            url = url + api.path
        timeout = data.timeout or 3000
        headers = data.headers or {}
        cookies = data.cookies or {}
        data_ = data.data or {}
        payload = dict(url=url,
                       timeout=timeout,
                       headers=headers,
                       cookies=cookies)
        if api.method == 'get':
            # 由于get发送的是string，data如果是多层结构的json就需要把第二层以后的的dict全部转成string
            for (k, v) in data_.items():
                if isinstance(v, dict):
                    data_[k] = json.dumps(v)
                else:
                    continue
            payload.update(dict(params=data_))
            return http_get(payload)
        elif api.method == 'post':
            # post发送的是对象，所以保持完整的json结构不用管
            payload.update(dict(json=data_))
            return http_post(payload)
        else:
            raise Exception(f'不支持的 api.method {api.method}')

    @staticmethod
    def _get_target(res, check: ApiTestCheckPoint):
        if check.target == 'status_code':
            data = res.status_code
        elif check.target == 'json':
            data = res.json()
        else:
            raise Exception(f'不支持的 check.target {check.target}')
        if check.rule:
            return get_json_value(data, check.rule)
        else:
            return data

    def _verify_check_point(self, res, check):
        check: ApiTestCheckPoint
        target = self._get_target(res, check)
        if check.operator == 'eq':
            return operator.eq(str(target), str(check.expect))
        elif check.operator == 'ne':
            return operator.ne(str(target), str(check.expect))
        else:
            raise Exception(f'不支持的 check.operator {check.operator}')

    def run(self, api_test_case_pk, data_config: dict = None) -> dict:
        if ApiTestCaseRepository().count_by({'pk':api_test_case_pk}) == 0:
            raise Exception(f'没有查询到ApiTestCase数据')
        else:
            api_test_case: ApiTestCase = ApiTestCaseRepository().filter_by_pk(api_test_case_pk)[0]
        if TestApiRepository().count_by({'pk': api_test_case.FK_TestApi_pk}) == 0:
            raise Exception(f'没有查询到应该关联的TestApi数据')
        else:
            test_api: TestApi = TestApiRepository().filter_by_pk(api_test_case.FK_TestApi_pk)[0]
        if ApiTestDataRepository().count_by({'FK_ApiTestCase_pk': api_test_case.pk}) == 0:
            raise Exception(f'没有查询到应该关联的ApiTestData数据')
        else:
            # 一条用例关联多条测试数据
            test_data_list = ApiTestDataRepository().filter_by({'FK_ApiTestCase_pk': api_test_case.pk})
        test_verify_list = []
        for data in test_data_list:
            if data_config:
                data = self._set_data_config(data, data_config)
            else:
                pass
            # 发包
            res = self._http_request(test_api, data)
            if ApiTestCheckPointRepository().count_by({'FK_ApiTestData_pk': data.pk}) == 0:
                # 如果没有检查点，默认通过
                test_verify_list.append(dict(data_name=data.name, check_name=None, result=True))
            else:
                # 一条数据关联多条验证点
                check_list = ApiTestCheckPointRepository().filter_by({'FK_ApiTestData_pk': data.pk})
                for check in check_list:
                    test_verify_list.append(dict(data_name=data.name,
                                                 check_name=check.name,
                                                 result=self._verify_check_point(res, check)))
        # 所有检查点必须都是True才算pass
        for check_res in test_verify_list:
            if check_res.get('result'):
                continue
            else:
                return dict(result='fail', log=test_verify_list)
        return dict(result='pass', log=test_verify_list)
