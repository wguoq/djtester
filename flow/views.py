import json

from django.forms import model_to_dict
from django.http import JsonResponse

from djtester.views import BaseViews
from flow.apps import FlowConfig
from flow.domain.flow_mgr import FlowMgr


class FlowViews(BaseViews):
    def __init__(self):
        super().__init__(FlowConfig.name)

    @staticmethod
    def commit(request):
        payload = json.loads(request.body) or {}
        print(f" commit payload = {payload}")
        action = payload.get('action')
        data = payload.get('data')
        if action is None or data is None:
            return JsonResponse(dict(message='参数错误'), status=500, safe=False)
        else:
            if action == 'inst':
                try:
                    a = FlowMgr().instance_flow(data.get('pk'), data.get('flow_data'))
                    return JsonResponse(model_to_dict(a), status=200, safe=False)
                except Exception as e:
                    context = dict(message=str(e))
                    return JsonResponse(context, status=500, safe=False)
            elif action == 'run':
                try:
                    a = FlowMgr().run_flow_instance(data.get('pk'))
                    return JsonResponse(model_to_dict(a), status=200, safe=False)
                except Exception as e:
                    context = dict(message=str(e))
                    return JsonResponse(context, status=500, safe=False)
            else:
                return JsonResponse(dict(message='参数错误'), status=500, safe=False)
