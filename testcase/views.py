import json
from django.http import JsonResponse
from djtester.views import BaseViews
from testcase.apps import TestcaseConfig
from testcase.domain.tester_mgr import TesterMgr


class TestCaseViews(BaseViews):
    def __init__(self):
        super().__init__(TestcaseConfig.name)

    @staticmethod
    def commit(request):
        payload = json.loads(request.body) or {}
        print(f" commit payload = {payload}")
        action = payload.get('action')
        data = payload.get('data')
        if action is None or data is None:
            return JsonResponse(dict(message='参数错误'), status=500, safe=False)
        else:
            if action == "run":
                test_case_pk = data.get('pk')
                test_config = data.get('config') or {}
                try:
                    context = TesterMgr().run_api_case(test_case_pk, test_config)
                    return JsonResponse(context, status=200, safe=False)
                except Exception as e:
                    context = dict(message=str(e))
                    return JsonResponse(context, status=500, safe=False)
            else:
                return JsonResponse(dict(message='参数错误'), status=500, safe=False)

