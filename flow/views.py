"""
复制处理前端传入的参数
路由到对应接口
返回json数据
"""
import importlib
import traceback
from django.http import HttpResponseNotFound, JsonResponse
from flow.service import *

FLOW_SERVICE = importlib.import_module('flow.service')


def query(request):
    time.sleep(0.5)
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        params = request.GET
        print(f"params= {params}")
        if params and len(params) >= 1:
            try:
                # 这些取出来以后是str,可以用json.loads转一下，但是必须保证里面都是双引号
                service_name = params.get('service')
                action = params.get('action')
                filters = params.get('filters')
                if filters:
                    filters = json.loads(filters)
                else:
                    filters = {}
                page_size = params.get('pageSize')
                page_number = params.get('pageNumber')
                service = getattr(FLOW_SERVICE, service_name)
                context = do_query(service, action, filters, page_size, page_number)
                return JsonResponse(context, status=200, safe=False)
            except Exception as e:
                traceback.print_exc()
                context = dict(message=str(e)),
                return JsonResponse(context, status=500)
        else:
            return JsonResponse({}, status=200)


def do_query(service, action, filters, page_size, page_number):
    if action == 'filter':
        offset = int(page_size) * (int(page_number) - 1)
        limit = int(page_size) * int(page_number)
        total = service().count_by(filters)
        rows = service().filter_by(kwargs=filters, offset=offset, limit=limit)
        return dict(rows=rows, total=total)
    elif action == 'get':
        result = service().get_by_pk(filters.get('pk'))
        return dict(data=result, total=1)
    elif action == 'getFieldInfo':
        result = service().get_field_info()
        return dict(fields=result)
    else:
        return {}


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        payload = json.loads(request.body)
        service_name = payload.get('service')
        service = getattr(FLOW_SERVICE, service_name)
        print(f"payload = {payload}")
        try:
            # 因为头信息表示传输的格式是json，所以data取出来就是dict格式
            action = payload.get('action')
            data = payload.get('data')
            context = do_commit(service, action, data)
            return JsonResponse(context, status=200, safe=False)
        except Exception as e:
            traceback.print_exc()
            context = dict(message=str(e))
            return JsonResponse(context, status=500)


def do_commit(service, action, data):
    if action == "instance":
        return service().instance_flow(data.get('id'), data.get('flow_data'))
    elif action == "run":
        return service().run_inst(data.get('id'))
    elif action == 'add':
        return service().add(data)
    elif action == 'edit':
        return service().edit(data)
    else:
        return {}
