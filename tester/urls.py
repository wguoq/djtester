from django.urls import path
from . import views

app_name = 'tester'

urlpatterns = [
    path('commit/', views.TesterViews().commit, name='commit'),
    path('query/', views.TesterViews().query, name='query'),
]
