from djtester.views import BaseViews


class FlowViews(BaseViews):
    def __init__(self):
        super().__init__('flow.service')

    def _do_commit(self, action, data):
        if action == "instance":
            return self.service().instance_flow(data.get('id'), data.get('flow_data'))
        elif action == "run":
            return self.service().run_inst(data.get('id'))
        else:
            return super()._do_commit(action, data)

