from django.urls import path, re_path
from . import views

app_name = 'testcase'

urlpatterns = [
    path('test_json/', views.test_json, name='test_json'),
    path('test_token/', views.test_token, name='test_token'),



]
