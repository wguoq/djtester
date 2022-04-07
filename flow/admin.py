from django.contrib import admin
# Register your models here.
from .repositories import *


def get_field_name(field_info):
    ll = []
    for f in field_info:
        ll.append(f.get('name'))
    return ll


class Flow_Design_Admin(admin.ModelAdmin):
    field_info = FlowDesignDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Node_Design_Admin(admin.ModelAdmin):
    field_info = NodeDesignDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Node_Oder_Admin(admin.ModelAdmin):
    field_info = FlowNodeOderDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Instance_Admin(admin.ModelAdmin):
    field_info = FlowInstanceDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Node_Instance_Admin(admin.ModelAdmin):
    field_info = NodeInstanceDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Result_Rule_Admin(admin.ModelAdmin):
    field_info = FlowResultRuleDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Flow_Status_Rule_Admin(admin.ModelAdmin):
    field_info = FlowStatusRuleDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Node_Start_Rule_Admin(admin.ModelAdmin):
    field_info = NodeStartRuleDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Node_Status_Rule_Admin(admin.ModelAdmin):
    field_info = NodeStatusRuleDBHelper().get_field_info()
    list_display = get_field_name(field_info)


admin.site.register(Flow_Design, Flow_Design_Admin)
admin.site.register(Flow_Result_Rule, Flow_Result_Rule_Admin)
admin.site.register(Flow_Status_Rule, Flow_Status_Rule_Admin)
admin.site.register(Node_Design, Node_Design_Admin)
admin.site.register(Node_Start_Rule, Node_Start_Rule_Admin)
admin.site.register(Node_Status_Rule, Node_Status_Rule_Admin)
admin.site.register(Flow_Node_Oder, Flow_Node_Oder_Admin)
admin.site.register(Flow_Instance, Flow_Instance_Admin)
admin.site.register(Node_Instance, Node_Instance_Admin)
