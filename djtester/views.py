"""
处理前端传入的参数
路由到对应的DBhelper方法
返回json数据
"""
import importlib
import json
import traceback

from django.forms import model_to_dict
from django.http import HttpResponseNotFound, JsonResponse
from django.core import serializers


class BaseViews:
    def __init__(self, module_path: str):
        self.module = importlib.import_module(module_path)

    def _do_query(self, repo_name, action, filters, page_size, page_number):
        helper = getattr(self.module, repo_name)
        pk_name = helper().get_pk_name()
        if action == 'filter':
            offset = int(page_size) * (int(page_number) - 1)
            limit = int(page_size) * int(page_number)
            total = helper().count_by(filters)
            res = helper().filter_by(kwargs=filters, offset=offset, limit=limit)
            # res是QuerySet，需要转成dict才能以json格式返回，并且主键名字被显示成pk，需要把model字段的名字替换进来
            ss = serializers.serialize('json', res)
            res_list = json.loads(ss)
            ll = []
            for r in res_list:
                pk = r.pop('pk')
                fields = r.get('fields')
                fields.update({pk_name: pk})
                ll.append(fields)
            return dict(rows=ll, total=total)
        elif action == 'get':
            res = helper().get_by_pk(filters.get('pk'))
            ss = serializers.serialize('json', res)
            r = json.loads(ss)[0]
            fields = r.get('fields')
            fields.update({pk_name: r.get('pk')})
            return dict(data=fields, total=1)
        elif action == 'getFieldInfo':
            result = helper().get_field_info()
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
                    repo = params.get('repo')
                    repo_name = str(repo) + 'DBHelper'
                    action = params.get('action')
                    filters = params.get('filters')
                    if filters:
                        # filters取出来是str，要转成dict
                        filters = json.loads(filters)
                    else:
                        filters = {}
                    page_size = params.get('pageSize')
                    page_number = params.get('pageNumber')
                    context = self._do_query(repo_name, action, filters, page_size, page_number)
                    # safe=False 关闭safe模式才能序列化list数据
                    return JsonResponse(context, status=200, safe=False)
                except Exception as e:
                    # 把错误打印出来
                    traceback.print_exc()
                    context = dict(message=str(e)),
                    return JsonResponse(context, status=500, safe=False)
            else:
                return JsonResponse({}, status=200)

    def _do_commit(self, repo_name, action, data):
        helper = getattr(self.module, repo_name)
        if action == 'save':
            helper().save_this(data)
            return {}
        elif action == 'del':
            helper().del_(data)
            return {}
        else:
            raise Exception(f'不支持的action {action}')

    def commit(self, request):
        if request.method != 'POST':
            return HttpResponseNotFound
        else:
            payload = json.loads(request.body)
            print(f"payload = {payload}")
            try:
                repo = payload.get('repo')
                repo_name = str(repo) + 'DBHelper'
                action = payload.get('action')
                data = payload.get('data')
                context = self._do_commit(repo_name, action, data)
                return JsonResponse(context, status=200, safe=False)
            except Exception as e:
                traceback.print_exc()
                context = dict(message=str(e))
                return JsonResponse(context, status=500, safe=False)


