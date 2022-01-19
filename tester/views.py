import json

from django.http import HttpResponse, JsonResponse
# Create your views here.
from django.views.decorators.csrf import csrf_exempt

from tester.service import TesterServicer


@csrf_exempt
def run_test_case(request):
    test_case_id = request.POST.get('id')
    result = TesterServicer.run_testcase(test_case_id)
    return JsonResponse(status=200, data=result.__dict__)


def run_test_case_list(request):
    test_case_list = request.POST.get('test_case_list')
    data_list = []
    # 多线程 todo
    for test_case in test_case_list:
        test_case_id = test_case.get('id')
        result = TesterServicer.run_testcase(test_case_id).test_case_result
        data = dict(test_case_id=test_case_id, result=result)
        data_list.append(data)
    return JsonResponse(status=200, data=data_list)
