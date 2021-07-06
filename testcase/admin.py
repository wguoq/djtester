from django.contrib import admin

# Register your models here.

from .models import *


class Test_Case_Admin(admin.ModelAdmin):
    list_display = ('tc_identity', 'tc_action', 'tc_data')  # list


class Tc_Identity_Admin(admin.ModelAdmin):
    list_display = ('test_case_id', 'test_case_name',)  # list


admin.site.register(Tc_Identity, Tc_Identity_Admin)
admin.site.register(Tc_Action)
admin.site.register(Tc_Data)
admin.site.register(Tc_Check_Point)
admin.site.register(Test_Case, Test_Case_Admin)
