from django.db import models

from djtester.models import Time_Field, Code_Field


class Test_Case(Code_Field, Time_Field):
    tc_name = models.CharField(max_length=128,
                               null=True,
                               verbose_name="测试名称")

    tc_type = models.CharField(max_length=64,
                               null=True,
                               blank=True,
                               default="api",
                               verbose_name="测试类型",
                               help_text="api")

    tc_action_id = models.CharField(max_length=64,
                                    null=True,
                                    blank=True,
                                    verbose_name="测试行为id")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class Tc_Api(Time_Field):
    api_name = models.CharField(max_length=128,
                                null=True,
                                verbose_name="api名称")

    api_type = models.CharField(max_length=64,
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
                                    verbose_name="api参数脚本")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class Tc_Api_Data(Time_Field):
    test_case = models.ForeignKey(to=Test_Case,
                                  null=True,
                                  on_delete=models.SET_NULL,
                                  blank=True,
                                  verbose_name="测试用例id")

    data_name = models.CharField(max_length=256,
                                 null=True,
                                 blank=True,
                                 verbose_name="数据名称")

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
                            blank=True,
                            verbose_name="data")

    class Meta:
        ordering = ['-created_time']


class Tc_Data(Time_Field):
    test_case = models.ForeignKey(to=Test_Case,
                                  null=True,
                                  on_delete=models.SET_NULL,
                                  blank=True,
                                  verbose_name="测试用例id")
    data_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="数据名称")
    data_script = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="数据脚本")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class Tc_CheckPoint(Time_Field):
    tc_data_id = models.CharField(max_length=64,
                                  null=True,
                                  blank=True,
                                  verbose_name="测试数据id")

    check_name = models.CharField(max_length=128,
                                  blank=True,
                                  null=True,
                                  verbose_name="验证点名称")

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
