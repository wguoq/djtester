from django.urls import path, re_path
from . import views

app_name = 'testcase'

urlpatterns = [
    path('', views.index, name='testcase_index'),
    path('init_api_testcase/', views.init_api_testcase, name='init_api_testcase'),
    path('init_ui_testcase/', views.init_ui_testcase, name='init_ui_testcase'),
    path('init_mobile_testcase/', views.init_mobile_testcase, name='init_mobile_testcase'),



]
