import json

from django.http import HttpResponse
from django.shortcuts import render

# Create your views here.
from testcase.service import TestCaseService


def index(request):
    if request.method != 'GET':
        raise Exception(' only GET ')
    else:
        return render(request, 'testcase/index.html')


def init_api_testcase(request):
    if request.method != 'GET':
        raise Exception(' only GET ')
    else:
        api_test = TestCaseService.init_testcase(case_type='api')
        return HttpResponse(json.dumps(api_test))


def init_ui_testcase(request):
    if request.method != 'GET':
        raise Exception(' only GET ')
    else:
        ui_test = TestCaseService.init_testcase(case_type='ui')
        return HttpResponse(json.dumps(ui_test))


def init_mobile_testcase(request):
    if request.method != 'GET':
        raise Exception(' only GET ')
    else:
        mobile_test = TestCaseService.init_testcase(case_type='mobile')
        return HttpResponse(json.dumps(mobile_test))
