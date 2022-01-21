import json

from django.http import HttpResponse, JsonResponse
# Create your views here.
from django.views.decorators.csrf import csrf_exempt

from tester.service import TesterService


@csrf_exempt
def run_test_case(request):
    test_case_id = request.POST.get('id')
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
