from djtester.views import BaseViews
from tester.domain.tester_mgr import TesterMgr


class TesterViews(BaseViews):
    def __init__(self):
        super().__init__('tester.repositories')

    def _do_commit(self, repo_name, action, data):
        if action == "run":
            test_case_pk = data.get('pk')
            test_config = data.get('config') or {}
            return TesterMgr().run_case(test_case_pk, test_config)
        else:
            return super()._do_commit(repo_name, action, data)
