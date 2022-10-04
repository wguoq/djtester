from django.db import models
from djtester.models import TimeField


class TestApi(TimeField):

    name = models.TextField(blank=True,
                            null=True,
                            verbose_name="api名称")

    description = models.TextField(null=True,
                                   blank=True,
                                   verbose_name="api描述")

    type = models.CharField(max_length=64,
                            null=True,
                            default="RESTful",
                            verbose_name="api类型")

    protocol = models.CharField(max_length=32,
                                null=True,
                                blank=True,
                                default='http',
                                verbose_name='协议',
                                help_text='http | https')

    method = models.CharField(max_length=32,
                              null=True,
                              blank=True,
                              verbose_name='方法',
                              help_text='get | post')

    path = models.CharField(max_length=256,
                            null=True,
                            blank=True,
                            verbose_name="api路径")

    param_script = models.JSONField(null=True,
                                    blank=True,
                                    verbose_name="api参数")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class ApiTestCase(TimeField):

    name = models.TextField(blank=True,
                            null=True,
                            verbose_name="测试用例名称")

    description = models.TextField(null=True,
                                   blank=True,
                                   verbose_name="测试用例描述")

    FK_TestApi_pk = models.CharField(max_length=65,
                                     null=True,
                                     blank=True,
                                     verbose_name="关联测试Api")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class ApiTestData(TimeField):

    FK_ApiTestCase_pk = models.CharField(max_length=65,
                                         null=True,
                                         blank=True,
                                         verbose_name="关联测试用例id")

    name = models.TextField(blank=True,
                            null=True,
                            verbose_name="测试数据名称")

    description = models.TextField(null=True,
                                   blank=True,
                                   verbose_name="测试数据描述")

    host = models.CharField(max_length=256,
                            null=True,
                            blank=True,
                            default='127.0.0.1',
                            verbose_name="域名")

    port = models.IntegerField(null=True,
                               blank=True,
                               verbose_name="端口号", )

    timeout = models.IntegerField(null=True,
                                  blank=True,
                                  default=30000,
                                  verbose_name="超时时间")

    headers = models.JSONField(null=True,
                               blank=True,
                               verbose_name="headers")

    cookies = models.JSONField(null=True,
                               blank=True,
                               verbose_name="cookies")

    data = models.JSONField(null=True,
                            verbose_name="data")

    class Meta:
        ordering = ['-created_time']


class ApiTestCheckPoint(TimeField):

    FK_ApiTestData_pk = models.CharField(max_length=65,
                                         null=True,
                                         blank=True,
                                         verbose_name="关联测试数据id")

    name = models.TextField(blank=True,
                            null=True,
                            verbose_name="测试验证点名称")

    description = models.TextField(null=True,
                                   blank=True,
                                   verbose_name="测试验证点描述")

    target = models.CharField(max_length=32,
                              blank=True,
                              null=True,
                              verbose_name="验证目标",
                              help_text='status_code | json')

    rule = models.CharField(max_length=256,
                            blank=True,
                            null=True,
                            verbose_name="json取值规则",
                            help_text='data__rows__[2] | data__rows__{customerName=abc}__businessName')

    operator = models.CharField(max_length=32,
                                blank=True,
                                default='eq',
                                null=True,
                                verbose_name="比较方式",
                                help_text='eq | ne')

    expect = models.CharField(max_length=256,
                              blank=True,
                              null=True,
                              verbose_name="期望值")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']
