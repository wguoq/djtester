import json

from django.http import HttpResponse, JsonResponse
# Create your views here.
from tester.service import TesterServicer


def run_test_case(request):
    test_case_pk = request.POST.get('test_case_pk')
    result = TesterServicer(test_case_pk).run_testcase().get_test_result
    data = {"test_case_pk": test_case_pk,
            "result": result}
    return JsonResponse(data, status=200)
