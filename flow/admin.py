from django.contrib import admin

# Register your models here.


from .models import *


class Flow_Design_Admin(admin.ModelAdmin):
    list_display = (
        'flow_code', 'flow_name', 'flow_type', 'flow_result_rule_id', 'flow_status_rule_id', 'version',
        'version_status')


class Node_Design_Admin(admin.ModelAdmin):
    list_display = (
        'node_code', 'node_name', 'node_type', 'start_rule_type', 'node_func_name', 'node_func_data', 'version',
        'version_status')


admin.site.register(Flow_Design, Flow_Design_Admin)
admin.site.register(Flow_Result_Rule)
admin.site.register(Flow_Status_Rule)
admin.site.register(Node_Design)
admin.site.register(Node_Start_Rule)
admin.site.register(Node_Status_Rule)
admin.site.register(Flow_Node_Design_Oder)
