from django.db import models


class TimeFields(models.Model):
    created_time = models.DateTimeField(null=True,
                                        auto_now_add=True,
                                        verbose_name="创建时间")

    modified_time = models.DateTimeField(null=True,
                                         auto_now=True,
                                         verbose_name="修改时间")

    # 定义为抽象类,不会创建表
    class Meta:
        abstract = True


class CodeFields(models.Model):
    code = models.CharField(max_length=64,
                            blank=True,
                            null=True,
                            verbose_name="编码")

    version = models.IntegerField(blank=True,
                                  default=1,
                                  null=True,
                                  verbose_name="版本")

    ver_status = models.CharField(max_length=64,
                                  default="ENABLED",
                                  blank=True,
                                  null=True,
                                  verbose_name="版本状态",
                                  help_text='ENABLED | DISABLED')

    class Meta:
        abstract = True


class Test_Case(CodeFields, TimeFields):
    tc_name = models.CharField(max_length=256,
                               null=True,
                               verbose_name="测试名称")

    tc_type = models.CharField(max_length=64,
                               null=True,
                               blank=True,
                               verbose_name="测试类型")

    tc_action = models.JSONField(null=True,
                                 blank=True,
                                 verbose_name="测试行为")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']


class Tc_Data(TimeFields):
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


class Tc_CheckPoint(TimeFields):
    tc_data = models.ForeignKey(to=Tc_Data,
                                null=True,
                                on_delete=models.SET_NULL,
                                blank=True,
                                verbose_name="测试数据id")
    check_name = models.CharField(max_length=128,
                                  blank=True,
                                  null=True,
                                  verbose_name="验证点名称")
    check_point = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="验证点脚本")

    objects = models.Manager()

    class Meta:
        ordering = ['-created_time']
