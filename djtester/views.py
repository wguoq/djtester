"""
载入repositories
根据参数调用对应的CRUD方法
把返回值转为json
"""
import importlib
import json
import traceback
from django.db import transaction
from django.forms import model_to_dict
from django.http import HttpResponseNotFound, JsonResponse
from django.core import serializers


class BaseViews:
    def __init__(self, app_name: str):
        self.repos = importlib.import_module(app_name + '.repositories')

    @staticmethod
    def _filter(repository, filters: dict, page_size: int, page_number: int):
        offset = page_size * (page_number - 1)
        limit = page_size * page_number
        res = repository().filter_by(kwargs=filters, offset=offset, limit=limit)
        # res是QuerySet，需要转成dict才能以json格式返回，并且主键名字被显示成pk，需要把model字段的名字替换进来
        ss = serializers.serialize('json', res)
        res_list = json.loads(ss)
        ll = []
        for r in res_list:
            pk = r.pop('pk')
            fields = r.get('fields')
            fields.update({repository().get_pk_name(): pk})
            ll.append(fields)
            # 处理时间字段里面那个T
            for L in ll:
                L['created_time'] = L.get('created_time').replace('T', ' ') if L.get('created_time') else None
                L['modified_time'] = L.get('modified_time').replace('T', ' ') if L.get('modified_time') else None
        return ll

    def _query(self, repo: str, action: str, filters: dict, page_size: int, page_number: int):
        repo_name = repo + 'Repository'
        repository = getattr(self.repos, repo_name)
        if action == 'filter':
            total = repository().count_by(filters)
            if total == 0:
                return dict(rows=[], total=0)
            else:
                a = self._filter(repository, filters, page_size, page_number)
                return dict(rows=a, total=total)
        elif action == 'get':
            if repository().count_by(filters) == 0:
                return dict(data={}, total=0)
            else:
                a = self._filter(repository, filters, page_size, page_number)[0]
                return dict(data=a, total=1)
        elif action == 'getFieldInfo':
            result = repository().get_field_info()
            return dict(fields=result)
        elif action == 'getTableInfo':
            result = repository().get_table_info()
            return dict(fields=result)
        elif action == 'table_filter':
            total = repository().count_by(filters)
            if total == 0:
                return dict(data={}, total=0)
            else:
                res = self._filter(repository, filters, page_size, page_number)
                # 把表名拼到字段名前面
                a = {}
                for r in res:
                    for k, v in r.items():
                        new_k = repo + '__' + k
                        a.update({new_k: v})
                return dict(rows=a, total=total)
        elif action == 'table_get':
            if repository().count_by(filters) == 0:
                return dict(data={}, total=0)
            else:
                res = self._filter(repository, filters, page_size, page_number)[0]
                a = {}
                for k, v in res.items():
                    new_k = repo + '__' + k
                    a.update({new_k: v})
                return dict(data=a, total=1)
        else:
            return {}

    def query(self, request):
        params = request.GET
        print(f'request.GET = {params}')
        # get发过来的参数是str，所以filters需要转成dict
        if params is not None and len(params) > 0:
            repo = params.get('repo')
            action = params.get('action')
            filters = json.loads(params.get('filters')) if params.get('filters') else {}
            page_size = int(params.get('pageSize')) or 10
            page_number = int(params.get('pageNumber')) or 1
            if repo is None or action is None:
                context = dict(message="参数错误"),
                return JsonResponse(context, status=500, safe=False)
            else:
                try:
                    context = self._query(repo, action, filters, page_size, page_number)
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
        # 把 {'t1__name': '1name','t2__name': '2name'}
        # 转成 {'t1': {'name': '1name'}, 't2': {'name': '2name'}}
        res = {}
        for k, v in data.items():
            x = k.split('__')
            y = res.get(x[0]) or {}
            y.update({x[1]: v})
            res.update({x[0]: y})
        return res

    @transaction.atomic
    def _group_save(self, data: dict, condition: list = None):
        if data is None or len(data) == 0:
            return {}
        data = self._format_data(data)
        repos = data.keys()
        all_data = {}
        # 把所有表都保存一遍，把结果保存下来
        for repo in repos:
            repo_name = repo + 'Repository'
            repository = getattr(self.repos, repo_name)
            r = repository().save_this(data.get(repo))
            all_data.update({repo: model_to_dict(r)})
        # 根据关联关系检查一遍结果，如果有值对不上的就要赋值
        # condition = ['t3__t1code=t1__code', 't3__t2code=t2__code']
        if condition is None or len(condition) == 0:
            pass
        else:
            flag = []
            for c in condition:
                c = c.split('=')
                left = c[0].split('__')
                left_repo_name = left[0]
                left_key = left[1]
                left_value = all_data.get(left_repo_name).get(left_key)
                right = c[1].split('__')
                right_repo_name = right[0]
                right_key = right[1]
                right_value = all_data.get(right_repo_name).get(right_key)
                if left_value == right_value:
                    continue
                else:
                    all_data[left_repo_name][left_key] = right_value
                    flag.append(left_repo_name)
            # 用set方法去重
            flag = set(flag)
            for repo in flag:
                repo_name = repo + 'Repository'
                repository = getattr(self.repos, repo_name)
                repository().save_this(all_data.get(repo))
                # self._do_commit(repo, 'save', data, None)
        return {}

    def _commit(self, repo: str, action: str, data: dict, condition: list = None) -> dict or list:
        if action == 'save_group':
            return self._group_save(data, condition)
        repository = getattr(self.repos, repo + 'Repository')
        if action == 'save':
            res = repository().save_this(data)
            return model_to_dict(res)
        elif action == 'saves':
            ll = []
            for d in data:
                res = repository().save_this(d)
                ll.append(model_to_dict(res))
            return ll
        elif action == 'del':
            res = repository().del_(data)
            return res
        else:
            raise Exception(f'不支持的action {action}')

    def commit(self, request):
        # post进来的是对象所以可以直接转成dict
        payload = json.loads(request.body) or {}
        print(f"payload = {payload}")
        repo = payload.get('repo')
        action = payload.get('action')
        data = payload.get('data')
        condition = payload.get('condition')
        try:
            context = self._commit(repo=repo, action=action, data=data, condition=condition)
            return JsonResponse(context, status=200, safe=False)
        except Exception as e:
            traceback.print_exc()
            context = dict(message=str(e))
            return JsonResponse(context, status=500, safe=False)
