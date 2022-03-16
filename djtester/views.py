"""
处理前端传入的参数
路由到对应的service接口
返回json数据
"""
import importlib
import json
import traceback
from django.http import HttpResponseNotFound, JsonResponse


class BaseViews:
    def __init__(self, module_path: str):
        self.module = importlib.import_module(module_path)
        self.service = None

    def _do_query(self, action, filters, page_size, page_number):
        if action == 'filter':
            offset = int(page_size) * (int(page_number) - 1)
            limit = int(page_size) * int(page_number)
            total = self.service().count_by(filters)
            rows = self.service().filter_by(kwargs=filters, offset=offset, limit=limit)
            return dict(rows=rows, total=total)
        elif action == 'get':
            result = self.service().get_by_pk(filters.get('pk'))
            return dict(data=result, total=1)
        elif action == 'getFieldInfo':
            result = self.service().get_field_info()
            return dict(fields=result)
        else:
            return {}

    def query(self, request):
        if request.method != 'GET':
            return HttpResponseNotFound
        else:
            params = request.GET
            print(f'params = {params}')
            if params and len(params) >= 1:
                try:
                    service_name = params.get('service')
                    self.service = getattr(self.module, service_name)
                    action = params.get('action')
                    filters = params.get('filters')
                    if filters:
                        # filters取出来是str，要转成dict
                        filters = json.loads(filters)
                    else:
                        filters = {}
                    page_size = params.get('pageSize')
                    page_number = params.get('pageNumber')
                    context = self._do_query(action, filters, page_size, page_number)
                    # safe=False 关闭safe模式才能序列化list数据
                    return JsonResponse(context, status=200, safe=False)
                except Exception as e:
                    # 把错误打印出来
                    traceback.print_exc()
                    context = dict(message=str(e)),
                    return JsonResponse(context, status=500)
            else:
                return JsonResponse({}, status=200)

    def _do_commit(self, action, data):
        if action == 'add':
            return self.service().add(data)
        elif action == 'edit':
            return self.service().edit(data)
        else:
            return {}

    def commit(self, request):
        if request.method != 'POST':
            return HttpResponseNotFound
        else:
            payload = json.loads(request.body)
            print(f"payload = {payload}")
            try:
                service_name = payload.get('service')
                self.service = getattr(self.module, service_name)
                action = payload.get('action')
                data = payload.get('data')
                context = self._do_commit(action, data)
                return JsonResponse(context, status=200, safe=False)
            except Exception as e:
                traceback.print_exc()
                context = dict(message=str(e))
                return JsonResponse(context, status=500)


