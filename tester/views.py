import importlib
import json

from django.http import HttpResponse, JsonResponse, HttpResponseNotFound
# Create your views here.
from django.views.decorators.csrf import csrf_exempt

from tester.service import TesterService


def run_test_case(request):
    test_case_id = request.POST.get('id')
    result = TesterService.run_testcase(test_case_id)
    context = dict(data=result.__dict__)
    return JsonResponse(status=200, data=context)


def run(request):
    print(request)
    print(request.body)
    j = json.loads(request.body)
    print(j)
    test_case_id = j.get('id')
    result = TesterService.run_testcase(test_case_id)
    context = dict(data=result.__dict__)
    return JsonResponse(status=200, data=context)


def run_test_case_list(request):
    test_case_list = request.POST.get('test_case_list')
    data_list = []
    # 多线程 todo
    for test_case in test_case_list:
        test_case_id = test_case.get('id')
        result = TesterService.run_testcase(test_case_id)
        data_list.append(result.__dict__)
    context = dict(data=data_list)
    return JsonResponse(status=200, data=context)


tester_service = importlib.import_module('tester.service')


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        context = {}
        payload = json.loads(request.body)
        print(request)
        print(payload)
        service_name = payload.get('service')
        service = getattr(tester_service, service_name)
        print(f"service= {service}")
        action = payload.get('action')
        data = payload.get('data')
        if action == "run":
            test_case_id = data.get('id')
            result = TesterService.run_testcase(test_case_id)
            context = dict(data=result.__dict__)
            return JsonResponse(status=200, data=context)
        else:
            return JsonResponse(context, status=200)
