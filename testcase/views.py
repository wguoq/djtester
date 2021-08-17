import json

from django.http import HttpResponse, JsonResponse, HttpResponseNotFound
from django.shortcuts import render

# Create your views here.
from testcase.service import TestCaseServicer


def index(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        return render(request, 'testcase/index.html')


def new_api_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        api_test = TestCaseServicer.new_api_testcase()
        return JsonResponse(api_test, status=200)


def get_all_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        all_case = TestCaseServicer().get_all()
        # return HttpResponse(json.dumps(all_case))
        context = {'data': all_case}
        return render(request, 'testcase/test_case.html', context)
