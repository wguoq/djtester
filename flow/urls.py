from django.urls import path, re_path
from . import views

app_name = 'flow'

urlpatterns = [
    # path('', views.index, name='flow_index'),
    path('query/', views.query, name='query'),
    path('commit/', views.commit, name='commit'),
]
