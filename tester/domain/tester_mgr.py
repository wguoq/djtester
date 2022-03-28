from pydantic import BaseModel
from tester.domain.api_tester import ApiTester
from tester.repositories import *


class TestResult(BaseModel):
    id: int = ""
    code: str = ""
    name: str = ""
    result: str = ""
    log: str = ""


class TesterMgr:

    @staticmethod
    def run_case(test_case_pk):
        test_case: Test_Case = TestCaseDBHelper().get_by({'pk': test_case_pk})
        if test_case.tc_type == 'api':
            r = ApiTester().run(test_case_pk)
            return dict(id=test_case.pk,
                        code=test_case.code,
                        name=test_case.tc_name,
                        result=r.get('result'),
                        log=r.get('log'))
        else:
            raise Exception(f'无法识别的 tc_type {test_case.tc_type}')
