from django.db import models


class Time_Field(models.Model):
    created_time = models.DateTimeField(null=True,
                                        auto_now_add=True,
                                        verbose_name="创建时间")

    modified_time = models.DateTimeField(null=True,
                                         auto_now=True,
                                         verbose_name="修改时间")

    # 定义为抽象类,不会创建表
    class Meta:
        abstract = True


class Code_Field(models.Model):
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
