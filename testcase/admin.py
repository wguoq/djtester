from django.contrib import admin
from .repositories import *


def get_field_name(field_info):
    ll = []
    for f in field_info:
        ll.append(f.get('name'))
    return ll


class ApiTestCaseAdmin(admin.ModelAdmin):
    field_info = ApiTestCaseRepository().get_field_info()
    list_display = get_field_name(field_info)


class TestApiAdmin(admin.ModelAdmin):
    field_info = TestApiRepository().get_field_info()
    list_display = get_field_name(field_info)


class ApiTestDataAdmin(admin.ModelAdmin):
    field_info = ApiTestDataRepository().get_field_info()
    list_display = get_field_name(field_info)


class ApiTestCheckPointAdmin(admin.ModelAdmin):
    field_info = ApiTestCheckPointRepository().get_field_info()
    list_display = get_field_name(field_info)


admin.site.register(ApiTestCase, ApiTestCaseAdmin)
admin.site.register(TestApi, TestApiAdmin)
admin.site.register(ApiTestData, ApiTestDataAdmin)
admin.site.register(ApiTestCheckPoint, ApiTestCheckPointAdmin)
