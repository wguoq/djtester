"""
还要补充
step表
step实例表???
flow+step关系表
step+flow关系表
flow实例表
result_rule和data可能也需要建表
"""
from django.db import models


# Create your models here.


class TestFlowS(models.Model):
    name = models.CharField(max_length=128,
                            blank=True,
                            null=True,
                            verbose_name="流程名")

    status = models.CharField(max_length=128,
                              blank=True,
                              null=True,
                              verbose_name="流程状态")
    result_rule = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="流程业务结果规则")
    result = models.CharField(max_length=128,
                              blank=True,
                              null=True,
                              verbose_name="流程业务结果")
    data = models.JSONField(null=True,
                            blank=True,
                            verbose_name="流程业务数据")
    step_list = models.JSONField(null=True,
                                 blank=True,
                                 verbose_name="流程步骤列表")

    objects = models.Manager()
