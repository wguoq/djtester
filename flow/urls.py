from django.urls import path, re_path
from . import views

app_name = 'flow'

urlpatterns = [
    # path('', views.index, name='flow_index'),
    path('query/', views.FlowViews().query, name='query'),
    path('commit/', views.FlowViews().commit, name='commit'),
]
