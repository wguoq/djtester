from django.urls import path, re_path
from . import views

app_name = 'tester'

urlpatterns = [
    # path('', views.index, name='testcase_index'),
    path('run/', views.run_test_case, name='run'),


]
