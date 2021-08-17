import json

from django.http import HttpResponse, JsonResponse
from django.shortcuts import render
from djtester.testcase_service import TestCaseServicer
# Create your views here.
from tester.service import TesterService


def run_test_case(request):
    test_case_pk = request.POST.get('test_case_pk')
    test_case = TestCaseServicer().get_by_pk(test_case_pk)
    result = TesterService.run_testcase(test_case).get_test_result
    data = {"test_case_pk": test_case_pk,
            "result": result}
    return JsonResponse(data, status=200)
