from django.urls import path
from . import views

app_name = 'testcase'

urlpatterns = [
    path('commit', views.TestCaseViews().commit, name='commit'),
    path('fields', views.TestCaseViews().get_fields, name='fields'),
    path('query', views.TestCaseViews().query, name='query'),
    path('save', views.TestCaseViews().save, name='save'),
]
