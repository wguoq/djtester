import json

from django.http import HttpResponse, JsonResponse
# Create your views here.
from tester.service import TesterServicer


def run_test_case(request):
    test_case_pk = request.POST.get('test_case_pk')
    result = TesterServicer(test_case_pk).run_testcase().test_case_result.to_dict()
    data = {"test_case_pk": test_case_pk,
            "result": result}
    return JsonResponse(data, status=200)


def run_test_case_list(request):
    test_case_list = request.POST.get('test_case_list')
    data_list = []
    # 多线程 todo
    for test_case_pk in test_case_list:
        result = TesterServicer(test_case_pk).run_testcase().test_case_result.to_dict()
        data = {"test_case_pk": test_case_pk,
                "result": result}
        data_list.append(data)
    return JsonResponse(data_list, status=200)