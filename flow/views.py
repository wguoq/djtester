from djtester.views import BaseViews
from flow.domain.flow_mgr import FlowMgr


class FlowViews(BaseViews):
    def __init__(self):
        super().__init__('flow.repositories')

    def _do_commit(self, repo_name, action, data):
        if action == "instance":
            FlowMgr().instance_flow(data.get('pk'), data.get('flow_data'))
            return {}
        elif action == "run":
            FlowMgr().run_flow_instance(data.get('pk'))
            return {}
        else:
            return super()._do_commit(repo_name, action, data)
