import importlib
import json
import time

from django.core import serializers
from django.forms import model_to_dict
from django.http import HttpResponseNotFound, JsonResponse
from django.views.decorators.http import require_http_methods
from pydantic import BaseModel

from flow.service import *


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
    time.sleep(1)
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        context = {}
        params = request.GET
        print(f"params= {params}")
        print(f"request= {request}")
        if params and len(params) >= 1:
            # 这些取出来以后是str,可以用json.loads转一下，但是必须保证里面都是双引号
            service_name = params.get('service')
            action = params.get('action')
            filters = params.get('filters')
            filters = json.loads(filters)
            page = params.get('page')
            page = json.loads(page)
            pageable = page.get('pageable')
            pageSize = page.get('pageSize')
            pageNumber = page.get('pageNumber')
            service = getattr(flow_service, service_name)
            if action == 'all':
                result = service().get_all()
                context = dict(rows=result, total=len(result))
            elif action == 'getNodeList':
                result = FlowDesignService().get_node_list(filters.get('flowDesignId'))
                context = dict(rows=result, total=len(result))
            elif action == 'filter':
                pass
            return JsonResponse(context, status=200)


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        context = {}
        payload = json.loads(request.body)
        print(request)
        print(payload)
        service_name = payload.get('service')
        service = getattr(flow_service, service_name)
        print(f"service= {service}")
        action = payload.get('action')
        data = payload.get('data')
        if action == "instance":
            inst = FlowService.instance_flow(data.get('id'), data.get('flow_data'))
            context = dict(instance_id=inst.id)
        elif action == "run":
            pass
        return JsonResponse(context, status=200)
