from django.urls import path, re_path
from . import views

app_name = 'mock'

urlpatterns = [
    path('', views.index, name='mock_index'),
    path('test_json/', views.test_json, name='test_json'),
    path('test_token/', views.test_token, name='test_token'),



]
