from django.contrib import admin
from .repositories import *


def get_field_name(field_info):
    ll = []
    for f in field_info:
        ll.append(f.get('name'))
    return ll


class Test_Case_Admin(admin.ModelAdmin):
    field_info = TestCaseRepository().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Api_Admin(admin.ModelAdmin):
    field_info = TcApiRepository().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Api_DataAdmin(admin.ModelAdmin):
    field_info = TcApiDataRepository().get_field_info()
    list_display = get_field_name(field_info)


class Tc_Data_DataAdmin(admin.ModelAdmin):
    field_info = TcDataRepository().get_field_info()
    list_display = get_field_name(field_info)


class Tc_CheckPoint_Admin(admin.ModelAdmin):
    field_info = TcCheckPointRepository().get_field_info()
    list_display = get_field_name(field_info)


admin.site.register(TestCase, Test_Case_Admin)
admin.site.register(TcApi, Tc_Api_Admin)
admin.site.register(TcApiData, Tc_Api_DataAdmin)
admin.site.register(TcData, Tc_Data_DataAdmin)
admin.site.register(TcCheckPoint, Tc_CheckPoint_Admin)
