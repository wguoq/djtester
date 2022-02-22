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
    time.sleep(0.5)
    if request.method != 'GET':
        return HttpResponseNotFound
    else:
        context = {}
        status = 200
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
            elif action == 'filter':
                result = service().filter_by(filters)
                context = dict(rows=result, total=len(result))
            elif action == 'getNodeList':
                result = FlowDesignService().get_node_list(filters.get('id'))
                context = dict(rows=result, total=len(result))
            elif action == 'getDesignTemp':
                result = service().get_design_temp()
                context = dict(des=result)
            return JsonResponse(context, status=status)


def commit(request):
    if request.method != 'POST':
        return HttpResponseNotFound
    else:
        context = {}
        status = 200
        payload = json.loads(request.body)
        service_name = payload.get('service')
        service = getattr(flow_service, service_name)
        print(f"payload = {payload}")
        action = payload.get('action')
        data = payload.get('data')
        if action == "instance":
            try:
                inst = FlowService.instance_flow(data.get('id'), data.get('flow_data'))
                context = dict(instance_id=inst.id, message="ok")
            except Exception as e:
                status = 500
                context = dict(message=str(e))
        elif action == "run":
            try:
                result = service().run_inst(data.get('id'))
                context = dict(instance_id=result, message="ok")
            except Exception as e:
                status = 500
                context = dict(message=str(e))
        elif action == 'add':
            try:
                result = service().add_flow_design(data)
                context = dict(flow_id=result, message="ok")
            except Exception as e:
                status = 500
                context = dict(message=str(e))
        elif action == 'edit':
            try:
                result = service().edit_flow_design(data)
                context = dict(flow_id=result, message="ok")
            except Exception as e:
                status = 500
                context = dict(message=str(e))
        return JsonResponse(context, status=status)
