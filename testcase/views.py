import importlib
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


def new_api_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        api_test = TestCaseService.new_api_testcase()
        return JsonResponse(api_test, status=200)


def get_all_testcase(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        all_case = TestCaseService().get_all()
        # return HttpResponse(json.dumps(all_case))
        context = {'data': all_case}
        return render(request, 'testcase/test_case.html', context)


def get_all(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        print(request.GET)
        all_case = TestCaseService().get_all()
        total = len(all_case)
        context = {'data': all_case, 'total': total}
        return JsonResponse(context, status=200)


tc_service = importlib.import_module('testcase.service')


def query(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        context = {}
        params = request.GET
        print(f"params= {params}")
        if params and len(params) >= 1:
            # 这些取出来以后是str,可以用json.loads转一下，但是必须保证里面都是双引号
            service_name = params.get('service')
            action = params.get('action')
            filters = json.loads(params.get('filters'))
            page = json.loads(params.get('page'))
            pageable = page.get('pageable')
            pageSize = page.get('pageSize')
            pageNumber = page.get('pageNumber')
            service = getattr(tc_service, service_name)
            if action == 'all':
                result = service().get_all()
                context = dict(rows=result, total=len(result))
            elif action == 'get':
                pass
            elif action == 'filter':
                pass
            return JsonResponse(context, status=200)

