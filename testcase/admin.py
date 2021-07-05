from django.contrib import admin

# Register your models here.

from .models import TestCaseIdentity, TestCaseAction, TestCaseS

admin.site.register(TestCaseIdentity)
admin.site.register(TestCaseAction)
admin.site.register(TestCaseS)
