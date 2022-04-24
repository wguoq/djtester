from django.contrib import admin
# Register your models here.
from .repositories import *


def get_field_name(field_info):
    ll = []
    for f in field_info:
        ll.append(f.get('name'))
    return ll


class Flow_Design_Admin(admin.ModelAdmin):
    field_info = FlowDesignRepository().get_field_info()
    list_display = get_field_name(field_info)


class Node_Design_Admin(admin.ModelAdmin):
    field_info = NodeDesignRepository().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Node_Oder_Admin(admin.ModelAdmin):
    field_info = FlowNodeOderRepository().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Instance_Admin(admin.ModelAdmin):
    field_info = FlowInstanceRepository().get_field_info()
    list_display = get_field_name(field_info)


class Node_Instance_Admin(admin.ModelAdmin):
    field_info = NodeInstanceRepository().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Result_Rule_Admin(admin.ModelAdmin):
    field_info = FlowResultRuleRepository().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Status_Rule_Admin(admin.ModelAdmin):
    field_info = FlowStatusRuleRepository().get_field_info()
    list_display = get_field_name(field_info)


class Node_Start_Rule_Admin(admin.ModelAdmin):
    field_info = NodeStartRuleRepository().get_field_info()
    list_display = get_field_name(field_info)


class Node_Status_Rule_Admin(admin.ModelAdmin):
    field_info = NodeStatusRuleRepository().get_field_info()
    list_display = get_field_name(field_info)


admin.site.register(FlowDesign, Flow_Design_Admin)
admin.site.register(FlowResultRule, Flow_Result_Rule_Admin)
admin.site.register(FlowStatusRule, Flow_Status_Rule_Admin)
admin.site.register(NodeDesign, Node_Design_Admin)
admin.site.register(NodeStartRule, Node_Start_Rule_Admin)
admin.site.register(NodeStatusRule, Node_Status_Rule_Admin)
admin.site.register(FlowNodeOder, Flow_Node_Oder_Admin)
admin.site.register(FlowInstance, Flow_Instance_Admin)
admin.site.register(NodeInstance, Node_Instance_Admin)
