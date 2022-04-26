from djtester.views import BaseViews
from testcase.apps import TestcaseConfig
from testcase.domain.tester_mgr import TesterMgr


class TestCaseViews(BaseViews):
    def __init__(self):
        super().__init__(TestcaseConfig.name)

    def _do_commit(self, repo: str, action: str, data: dict, condition: list = None) -> dict or list:
        if action == "run":
            test_case_pk = data.get('pk')
            test_config = data.get('config') or {}
            return TesterMgr().run_case(test_case_pk, test_config)
        else:
            return super()._do_commit(repo, action, data, condition)
