import importlib
import json
from django.http import HttpResponseNotFound, JsonResponse
from pydantic import BaseModel


class QueryParams(BaseModel):
    service: str
    action: str
    filters: dict
    page: dict
    # page: {
    #     "pageSize": 10,
    #     "pageNumber": 1,
    #     "pageable": 'true'
    # }


class CommitParams(BaseModel):
    action: str
    params: dict


flow_service = importlib.import_module('flow.service')


def query(request):
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        context = {}
        params = request.GET
        print(f"params= {params}")
        if params and len(params) >= 1:
            # 这些取出来以后是str,可以用json.loads转一下，但是必须保证里面都是双引号
            service = params.get('service')
            action = params.get('action')
            filters = json.loads(params.get('filters'))
            page = json.loads(params.get('page'))
            pageable = page.get('pageable')
            pageSize = page.get('pageSize')
            pageNumber = page.get('pageNumber')
            service = getattr(flow_service, service)
            print(f"service= {service}")
            if action == 'all':
                result = service().get_all()
                context = dict(data=result, total=len(result))
            elif action == 'get':
                pass
            elif action == 'filter':
                pass
            return JsonResponse(context, status=200)



def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        context = {}
        return JsonResponse(context, status=200)
