import json

import requests
from django.forms import model_to_dict
from django.http import response

from tester.utils import *
from tester.repositories import *


class ApiTester:
    @staticmethod
    def _set_data_config(data: TcApiData, data_config: dict):
        data.host = data_config.get('host') or data.host
        data.port = data_config.get('port') if data_config.get('port') is not None else data.port
        data.timeout = data_config.get('timeout') if data_config.get('timeout') is not None else data.timeout
        data.headers = data_config.get('headers') or data.headers
        data.cookies = data_config.get('cookies') or data.cookies
        data.data = data_config.get('data') or data.data
        return data

    @staticmethod
    def _request_send(api: TcApi, data: TcApiData):
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
            return requests.get(**payload)
        elif api.method == 'post':
            # post发送的是对象，所以保持完整的json结构不用管
            payload.update(dict(json=data_))
            return requests.post(**payload)
        else:
            raise Exception(f'不支持的 api.method {api.method}')

    @staticmethod
    def _get_target(res, check: TcCheckPoint):
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
        check: TcCheckPoint
        target = self._get_target(res, check)
        if check.operator == 'eq':
            return operator.eq(str(target), str(check.expect))
        elif check.operator == 'ne':
            return operator.ne(str(target), str(check.expect))
        else:
            raise Exception(f'不支持的 check.operator {check.operator}')

    def run(self, test_case_pk, data_config: dict = None) -> dict:
        test_case: TestCase = TestCaseDBHelper().get_by_pk(test_case_pk)[0]
        # 一条用例对应一个接口
        if TcApiDBHelper().count_by({'pk': test_case.tc_action_id}) == 0:
            raise Exception(f'没有查询到对应的tc_api')
        else:
            tc_api: TcApi = TcApiDBHelper().get_by_pk(test_case.tc_action_id)[0]
        # 一条用例可以有多条数据
        if TcApiDataDBHelper().count_by({'test_case': test_case.pk}) == 0:
            raise Exception(f'没有查询到对应的tc_data')
        else:
            tc_data_list = TcApiDataDBHelper().filter_by({'test_case': test_case.pk})
        test_verify_list = []
        for data in tc_data_list:
            if data_config:
                data = self._set_data_config(data, data_config)
            else:
                pass
            # 发包
            res = self._request_send(tc_api, data)
            # 一条数据可以对应多条验证点
            if TcCheckPointDBHelper().count_by({'tc_data_id': data.pk}) == 0:
                # 如果没有检查点，默认通过
                test_verify_list.append(dict(data_name=data.data_name, check_name=None, result=True))
            else:
                check_list = TcCheckPointDBHelper().filter_by({'tc_data_id': data.pk})
                for check in check_list:
                    test_verify_list.append(dict(data_name=data.data_name,
                                                 check_name=check.check_name,
                                                 result=self._verify_check_point(res, check)))
        # 所有检查点必须都是True才算pass
        for check_res in test_verify_list:
            if check_res.get('result'):
                continue
            else:
                return dict(result='fail', log=test_verify_list)
        return dict(result='pass', log=test_verify_list)
