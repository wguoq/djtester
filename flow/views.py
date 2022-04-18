from djtester.views import BaseViews
from flow.domain.flow_mgr import FlowMgr


class FlowViews(BaseViews):
    def __init__(self):
        super().__init__('flow.repositories')

    def _do_commit(self, repo: str, action: str, data: dict, condition: list = None) -> dict or list:
        if action == "instance":
            a = FlowMgr().instance_flow(data.get('pk'), data.get('flow_data'))
            return dict(pk=a.pk)
        elif action == "run":
            a = FlowMgr().run_flow_instance(data.get('pk'))
            return dict(pk=a.pk)
        else:
            return super()._do_commit(repo, action, data, condition)

