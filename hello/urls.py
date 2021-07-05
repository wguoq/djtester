from django.urls import path
from . import views

app_name = 'hello'

urlpatterns = [
    path('', views.index, name='hello_index'),
]
