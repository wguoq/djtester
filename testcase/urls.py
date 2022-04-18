from django.urls import path
from . import views

app_name = 'testcase'

urlpatterns = [
    path('commit/', views.TestCaseViews().commit, name='commit'),
    path('query/', views.TestCaseViews().query, name='query'),
]
