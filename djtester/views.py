"""
处理前端传入的参数
路由到对应的DBhelper方法
返回json数据
"""
import importlib
import json
import traceback

from django.db import transaction
from django.forms import model_to_dict
from django.http import HttpResponseNotFound, JsonResponse
from django.core import serializers


class BaseViews:
    def __init__(self, module_path: str):
        self.module = importlib.import_module(module_path)

    @staticmethod
    def _do_filter(helper, filters, page_size, page_number):
        if page_size and page_number:
            offset = int(page_size) * (int(page_number) - 1)
            limit = int(page_size) * int(page_number)
        else:
            offset = None
            limit = None
        res = helper().filter_by(kwargs=filters, offset=offset, limit=limit)
        # res是QuerySet，需要转成dict才能以json格式返回，并且主键名字被显示成pk，需要把model字段的名字替换进来
        ss = serializers.serialize('json', res)
        res_list = json.loads(ss)
        ll = []
        for r in res_list:
            pk = r.pop('pk')
            fields = r.get('fields')
            fields.update({helper().get_pk_name(): pk})
            ll.append(fields)
            # 处理时间字段里面那个T
            for L in ll:
                L['created_time'] = L.get('created_time').replace('T', ' ')
                L['modified_time'] = L.get('modified_time').replace('T', ' ')
        return ll

    def _do_query(self, repo_name, action, filters, page_size, page_number):
        helper = getattr(self.module, repo_name)
        if action == 'filter':
            total = helper().count_by(filters)
            a = self._do_filter(helper, filters, page_size, page_number)
            return dict(rows=a, total=total)
        elif action == 'get':
            a = self._do_filter(helper, filters, page_size, page_number)
            return dict(data=a[0], total=1)
        elif action == 'getFieldInfo':
            result = helper().get_field_info()
            return dict(fields=result)
        elif action == 'getTableInfo':
            result = helper().get_table_info()
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
                    # filters取出来是str，要转成dict
                    filters = json.loads(params.get('filters')) if params.get('filters') else {}
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

    @staticmethod
    def _format_data(data: dict):
        # 把 {'t1@name': '1name','t2@name': '2name'}
        # 转成 {'t1': {'name': '1name'}, 't2': {'name': '2name'}}
        res = {}
        for k, v in data.items():
            x = k.split('@')
            y = res.get(x[0]) or {}
            y.update({x[1]: v})
            res.update({x[0]: y})
        return res

    @transaction.atomic
    def _group_save(self, data: dict, condition: list = None):
        # data = {'t1@name': '1name',
        #         't1@code': '1code',
        #         't2@name': '2name',
        #         't2@code': '2code',
        #         't3@name': '3name',
        #         't3@t1code': '1code',
        #         't3@t2code': '2code',
        #         }
        # constraint = ['t3@t1code=t1@code', 't3@t2code=t2@code']

        if data is None or len(data) == 0:
            return {}
        data = self._format_data(data)
        repos = data.keys()
        res = {}
        # 把所有表都保存一遍，把结果保存下来
        for repo in repos:
            repo__name = repo + 'DBHelper'
            helper = getattr(self.module, repo__name)
            r = helper().save_this(data.get(repo))
            res.update({repo: model_to_dict(r)})
        # 根据关联关系检查一遍结果，如果有值对不上的就要赋值
        if condition is None or len(condition) == 0:
            pass
        else:
            flag = []
            for c in condition:
                c = c.split('=')
                left = c[0].split('@')
                left_repo_name = left[0]
                left_key = left[1]
                left_value = res.get(left_repo_name).get(left_key)
                right = c[1].split('@')
                right_repo_name = right[0]
                right_key = right[1]
                right_value = res.get(right_repo_name).get(right_key)
                if left_value == right_value:
                    continue
                else:
                    res[left_repo_name][left_key] = right_value
                    flag.append(left_repo_name)
            # 用set方法去重
            flag = set(flag)
            for f in flag:
                repo_name = f + 'DBHelper'
                data = res.get(f)
                self._do_commit(repo_name, 'save', data, None)
        return {}

    def _do_commit(self, repo_name: str, action: str, data: dict, condition: list = None) -> dict or list:
        if action == 'save_group':
            return self._group_save(data, condition)
        helper = getattr(self.module, repo_name)
        if action == 'save':
            res = helper().save_this(data)
            return model_to_dict(res)
        elif action == 'saves':
            ll = []
            for d in data:
                res = helper().save_this(d)
                ll.append(model_to_dict(res))
            return ll
        elif action == 'del':
            res = helper().del_(data)
            return res
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
                print(f'data============ {data}')
                condition = payload.get('condition')
                print(f'condition============ {condition}')
                context = self._do_commit(repo_name=repo_name, action=action, data=data, condition=condition)
                return JsonResponse(context, status=200, safe=False)
            except Exception as e:
                traceback.print_exc()
                context = dict(message=str(e))
                return JsonResponse(context, status=500, safe=False)
