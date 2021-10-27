from pydantic import BaseModel

from djtester.enums import TestResult


class ApiCaseConfig(BaseModel):
    config_name: str = None
    test_host: str = None
    test_port: str = None
    test_headers: dict = None
    test_cookies: dict = None


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


# class ApiCheckPoint(BaseModel):
#     response_property: str = None
#     rule: str = None
#     operator: str = None
#     expect: str = None
#
#
# class ApiJsonSchemaCheckPoint(BaseModel):
#     response_property: str = 'json'
#     rule: str = None
#     json_schema: str = None


# class TestCheckPointResult(BaseModel):
#     check_point_name: str = None
#     check_point_result: str = None


# class TestCaseResult(BaseModel):
#     test_case_id: str = None
#     test_case_name: str = None
#     case_result: str = None
#     case_message: list[TestCheckPointResult] = None
#
#     def to_dict(self):
#         a = self.__dict__
#         case_message = []
#         for msg in self.case_message:
#             case_message.append(msg.__dict__)
#         a['case_message'] = case_message
#         return a

