import json

from django.http import HttpResponse
from django.shortcuts import render
from djtester.testcase_service import TestCaseService
# Create your views here.
from tester.service import TesterService


def run_test_case(request):
    test_case_pk = request.POST.get('test_case_pk')
    test_case = TestCaseService.get_by_pk(test_case_pk)
    result = TesterService.run_testcase(test_case).get_test_result
    data = {"test_case_pk": test_case_pk,
            "result": result}
    return HttpResponse(json.dumps(data))
