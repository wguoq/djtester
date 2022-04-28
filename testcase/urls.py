from django.urls import path
from . import views

app_name = 'testcase'

urlpatterns = [
    path('commit/', views.TestCaseViews().commit, name='commit'),
    path('query/', views.TestCaseViews().query, name='query'),
    path('fields', views.TestCaseViews().get_fields, name='fields'),
]
