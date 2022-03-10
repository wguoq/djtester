import importlib
import traceback
from django.http import HttpResponseNotFound, JsonResponse
from flow.service import *

flow_service = importlib.import_module('flow.service')


def query(request):
    time.sleep(0.5)
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        context = {}
        status = 200
        params = request.GET
        print(f"params= {params}")
        if params and len(params) >= 1:
            # 这些取出来以后是str,可以用json.loads转一下，但是必须保证里面都是双引号
            service_name = params.get('service')
            action = params.get('action')
            filters = params.get('filters')
            filters = json.loads(filters)
            page = params.get('page')
            page = json.loads(page)
            pageSize = page.get('pageSize')
            pageNumber = page.get('pageNumber')
            service = getattr(flow_service, service_name)
            if action == 'all':
                result = service().get_all()
                context = dict(rows=result, total=len(result))
            elif action == 'filter':
                result = service().filter_by(filters)
                context = dict(rows=result, total=len(result))
            elif action == 'get':
                result = service().get_by_pk(filters.get('pk'))
                context = dict(data=result, total=len(result))
            elif action == 'getTemp':
                result = service().get_temp()
                context = dict(rows=result)
            elif action == 'getFieldInfo':
                result = service().get_field_info()
                context = dict(fields=result)
            return JsonResponse(context, status=status, safe=False)


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        payload = json.loads(request.body)
        service_name = payload.get('service')
        service = getattr(flow_service, service_name)
        print(f"payload = {payload}")
        action = payload.get('action')
        data = payload.get('data')
        if isinstance(data, dict):
            result = do_commit(service, action, data)
            return JsonResponse(result.get('context'), status=result.get('status'))
        elif isinstance(data, list):
            result_list = []
            for d in data:
                result = do_commit(service, action, d)
                result_list.append(result)
            return JsonResponse(result_list, status=200)


def do_commit(service, action, data):
    if action == "instance":
        try:
            inst = service().instance_flow(data.get('id'), data.get('flow_data'))
            context = dict(instance_id=inst.id)
            return dict(context=context, status=200)
        except Exception as e:
            # 直接打印异常
            traceback.print_exc()
            # 返回字符串
            # traceback.format_exc()
            # 还可以将信息写入到文件
            # traceback.print_exc(file=open(‘error.txt’, ’a +’))
            return dict(context=dict(message=str(e)), status=500)
    elif action == "run":
        try:
            result = service().run_inst(data.get('id'))
            context = dict(instance_id=result)
            return dict(context=context, status=200)
        except Exception as e:
            traceback.print_exc()
            return dict(context=dict(message=str(e)), status=500)
    elif action == 'add':
        try:
            context = service().add(data)
            return dict(context=context, status=200)
        except Exception as e:
            traceback.print_exc()
            return dict(context=dict(message=str(e)), status=500)
    elif action == 'edit':
        try:
            context = service().edit(data)
            return dict(context=context, status=200)
        except Exception as e:
            traceback.print_exc()
            return dict(context=dict(message=str(e)), status=500)

