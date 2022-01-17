from django.contrib import admin

# Register your models here.

from .models import *


class Test_Case_Admin(admin.ModelAdmin):
    list_display = ('test_case_code', 'test_case_name', 'test_case_type', 'tc_action', 'tc_data')  # list


# class Tc_Identity_Admin(admin.ModelAdmin):
#     list_display = ('test_case_id', 'test_case_name',)  # list


# admin.site.register(Identity, Tc_Identity_Admin)
admin.site.register(Action)
admin.site.register(TestData)
admin.site.register(Check_Point)
admin.site.register(Test_Case, Test_Case_Admin)
