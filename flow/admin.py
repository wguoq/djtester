from django.contrib import admin

# Register your models here.


from .models import *


class Flow_Design_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'code', 'flow_name', 'flow_type', 'flow_result_rule_id', 'flow_status_rule_id', 'version',
        'version_status')


class Node_Design_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'code', 'node_name', 'node_type', 'start_rule_type', 'node_func_code', 'node_func_data', 'version',
        'version_status')


class Flow_Node_Design_Oder_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'flow_design', 'node_order', 'node_design')


class Flow_Instance_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'flow_design', 'flow_data', 'flow_status', 'flow_result')


class Node_Instance_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'node_design', 'node_func_name', 'node_func_data', 'node_order', 'node_status', 'node_result',
        'flow_instance')


admin.site.register(Flow_Design, Flow_Design_Admin)
admin.site.register(Flow_Result_Rule)
admin.site.register(Flow_Status_Rule)
admin.site.register(Node_Design, Node_Design_Admin)
admin.site.register(Node_Start_Rule)
admin.site.register(Node_Status_Rule)
admin.site.register(Flow_Node_Design_Oder, Flow_Node_Design_Oder_Admin)
admin.site.register(Flow_Instance, Flow_Instance_Admin)
admin.site.register(Node_Instance, Node_Instance_Admin)
