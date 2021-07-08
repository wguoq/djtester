import json

from django.http import HttpResponse, JsonResponse, HttpResponseNotFound
from django.shortcuts import render

# Create your views here.
from testcase.service import TestCaseService


def index(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        return render(request, 'testcase/index.html')


def init_api_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        api_test = TestCaseService.init_testcase(case_type='api')
        return JsonResponse(api_test, status=200)


def init_ui_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        ui_test = TestCaseService.init_testcase(case_type='ui')
        return JsonResponse(ui_test, status=200)


def init_mobile_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        mobile_test = TestCaseService.init_testcase(case_type='mobile')
        return JsonResponse(mobile_test, status=200)


def get_all_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        all_case = TestCaseService.get_all()
        # return HttpResponse(json.dumps(all_case))
        context = {'data': all_case}
        return render(request, 'testcase/test_case.html', context)
