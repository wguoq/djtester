from django.contrib import admin
from .repositories import *


def get_field_name(field_info):
    ll = []
    for f in field_info:
        ll.append(f.get('name'))
    return ll


class Test_Case_Admin(admin.ModelAdmin):
    field_info = TestCaseDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Test_Case_Admin(admin.ModelAdmin):
    field_info = TestCaseDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Api_Admin(admin.ModelAdmin):
    field_info = TcApiDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Api_DataAdmin(admin.ModelAdmin):
    field_info = TcApiDataDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Data_DataAdmin(admin.ModelAdmin):
    field_info = TcDataDBHelper().get_field_info()
    list_display = get_field_name(field_info)


class Tc_CheckPoint_Admin(admin.ModelAdmin):
    field_info = TcCheckPointDBHelper().get_field_info()
    list_display = get_field_name(field_info)


admin.site.register(Test_Case, Test_Case_Admin)
admin.site.register(Tc_Api, Tc_Api_Admin)
admin.site.register(Tc_Api_Data, Tc_Api_DataAdmin)
admin.site.register(Tc_Data, Tc_Data_DataAdmin)
admin.site.register(Tc_CheckPoint, Tc_CheckPoint_Admin)
