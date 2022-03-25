from django.contrib import admin
from .models import *


class Test_Case_Admin(admin.ModelAdmin):
    list_display = ('id', 'code', 'tc_name', 'tc_type', 'created_time', 'modified_time')


class TC_Data_Admin(admin.ModelAdmin):
    list_display = ('id', 'test_case_id', 'data_name', 'data_script', 'created_time', 'modified_time')


class TC_CheckPoint_Admin(admin.ModelAdmin):
    list_display = ('id', 'tc_data_id', 'check_name', 'check_point', 'created_time', 'modified_time')


admin.site.register(Test_Case, Test_Case_Admin)
admin.site.register(Tc_Api,)
admin.site.register(Tc_Api_Data,)
admin.site.register(Tc_Data, TC_Data_Admin)
admin.site.register(Tc_CheckPoint, TC_CheckPoint_Admin)
