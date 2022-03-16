from djtester.views import BaseViews


class FlowViews(BaseViews):
    def __init__(self):
        super().__init__('flow.service')

    def _do_commit(self, service_name, action, data):
        service = getattr(self.module, service_name)
        if action == "instance":
            return service().instance_flow(data.get('id'), data.get('flow_data'))
        elif action == "run":
            return service().run_inst(data.get('id'))
        else:
            return super()._do_commit(service_name, action, data)
