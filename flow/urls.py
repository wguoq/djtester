from django.urls import path, re_path
from . import views

app_name = 'flow'

urlpatterns = [
    path('commit', views.FlowViews().commit, name='commit'),
    path('fields', views.FlowViews().get_fields, name='fields'),
    path('query', views.FlowViews().query, name='query'),
    path('save', views.FlowViews().save, name='save'),
]
