from django.contrib import admin

# Register your models here.


from .models import *


class Flow_Design_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'code', 'fw_name', 'fw_type', 'fw_result_rule', 'fw_status_rule', 'version',
        'ver_status', 'created_time', 'modified_time')


class Node_Design_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'code', 'node_name', 'node_type', 'start_rule_type', 'node_func_code', 'node_func_data', 'version',
        'ver_status', 'created_time', 'modified_time')


class Flow_Node_Design_Oder_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'flow_design', 'node_order', 'node_design', 'created_time', 'modified_time')


class Flow_Instance_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'flow_design', 'flow_data', 'flow_status', 'flow_result', 'created_time', 'modified_time')


class Node_Instance_Admin(admin.ModelAdmin):
    list_display = (
        'id', 'node_design', 'node_func_code', 'node_func_data', 'node_order', 'node_status', 'node_result',
        'flow_instance', 'created_time', 'modified_time')


admin.site.register(Flow_Design, Flow_Design_Admin)
admin.site.register(Flow_Result_Rule)
admin.site.register(Flow_Status_Rule)
admin.site.register(Node_Design, Node_Design_Admin)
admin.site.register(Node_Start_Rule)
admin.site.register(Node_Status_Rule)
admin.site.register(Flow_Node_Oder, Flow_Node_Design_Oder_Admin)
admin.site.register(Flow_Instance, Flow_Instance_Admin)
admin.site.register(Node_Instance, Node_Instance_Admin)
