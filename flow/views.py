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
                service = getattr(flow_service, service_name)
                context = do_query(service, action, filters, page_size, page_number)
                return JsonResponse(context, status=200, safe=False)
            except Exception as e:
                traceback.print_exc()
                context = dict(message=str(e)),
                return JsonResponse(context, status=500)


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


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        payload = json.loads(request.body)
        service_name = payload.get('service')
        service = getattr(flow_service, service_name)
        print(f"payload = {payload}")
        try:
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
        inst = service().instance_flow(data.get('id'), data.get('flow_data'))
        context = dict(instance_id=inst.id)
        return dict(context=context, status=200)
    elif action == "run":
        result = service().run_inst(data.get('id'))
        context = dict(instance_id=result)
        return dict(context=context, status=200)
    elif action == 'add':
        context = service().add(data)
        return dict(context=context, status=200)

    elif action == 'edit':
        context = service().edit(data)
        return dict(context=context, status=200)
