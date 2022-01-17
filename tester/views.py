import json

from django.http import HttpResponse, JsonResponse
# Create your views here.
from tester.service import TesterServicer


def run_test_case(request):
    test_case_id = request.POST.get('test_case_id')
    result = TesterServicer.run_testcase(test_case_id).test_case_result
    data = {"test_case_id": test_case_id,
            "result": result}
    return JsonResponse(data, status=200)


def run_test_case_list(request):
    test_case_list = request.POST.get('test_case_list')
    data_list = []
    # 多线程 todo
    for test_case in test_case_list:
        test_case_id = test_case.get('id')
        result = TesterServicer.run_testcase(test_case_id).test_case_result
        data = {"test_case_id": test_case_id,
                "result": result}
        data_list.append(data)
    return JsonResponse(data_list, status=200)
