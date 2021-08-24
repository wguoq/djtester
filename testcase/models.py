from django.db import models


# class ModelTools(object):
#     def all_fields_dict(self):
#         """
#         把model的所有Fields和对应的值组成一个dict
#         """
#         all_fields = []
#         for a in self._meta.fields:
#             all_fields.append(a.name)
#         fields_dict = {}
#         for field in all_fields:
#             fields_dict.update({field: self.__dict__.get(field)})
#         return fields_dict


class Tc_Identity(models.Model):
    id = models.AutoField(primary_key=True)

    test_case_id = models.CharField(max_length=32,  # 32字符=64字节
                                    unique=True,
                                    null=True,
                                    verbose_name="测试用例id")
    test_case_name = models.CharField(max_length=128,
                                      unique=True,
                                      null=True,
                                      verbose_name="测试用例名称")
    objects = models.Manager()

    class Meta:
        ordering = ['-id']

    def __str__(self):
        return self.test_case_name


class Tc_Action(models.Model):
    id = models.AutoField(primary_key=True)

    action_type = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="测试用例行为类型")
    action_name = models.CharField(max_length=128,
                                   blank=True,
                                   null=True,
                                   verbose_name="测试用例行为名称")
    action = models.JSONField(null=True,
                              blank=True,
                              verbose_name="测试用例行为")

    objects = models.Manager()

    class Meta:
        ordering = ['-id']

    def __str__(self):
        return self.action_name


class Tc_Data(models.Model):
    id = models.AutoField(primary_key=True)

    data_type = models.CharField(max_length=32,
                                 blank=True,
                                 null=True,
                                 verbose_name="测试用例数据类型")
    data_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="测试用例数据名称")
    data = models.JSONField(null=True,
                            blank=True,
                            verbose_name="测试用例数据")

    objects = models.Manager()

    class Meta:
        ordering = ['-id']

    def __str__(self):
        return self.data_name


class Tc_Check_Point(models.Model):
    id = models.AutoField(primary_key=True)

    check_point_type = models.CharField(max_length=32,
                                        blank=True,
                                        null=True,
                                        verbose_name="测试用例验证点类型")
    check_point_name = models.CharField(max_length=128,
                                        blank=True,
                                        null=True,
                                        verbose_name="测试用例验证点名称")
    check_point = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="测试用例验证点")

    objects = models.Manager()

    class Meta:
        ordering = ['-id']

    def __str__(self):
        return self.check_point_name


class Test_Case(models.Model):
    id = models.AutoField(primary_key=True)

    test_case_type = models.CharField(max_length=32,
                                      null=True,
                                      # 设置不用必填
                                      blank=True,
                                      verbose_name="测试用例类型")

    tc_identity = models.ForeignKey(to=Tc_Identity,
                                    null=True,
                                    blank=True,
                                    on_delete=models.CASCADE,
                                    verbose_name="测试用例身份信息")

    tc_action = models.ForeignKey(to=Tc_Action,
                                  null=True,
                                  on_delete=models.SET_NULL,
                                  blank=True,
                                  verbose_name="测试用例行为")

    tc_data = models.ForeignKey(to=Tc_Data,
                                null=True,
                                on_delete=models.SET_NULL,
                                blank=True,
                                verbose_name="测试用例数据")

    tc_check_list = models.ManyToManyField(to=Tc_Check_Point,
                                           blank=True,
                                           verbose_name="测试用例验证")

    version = models.IntegerField(null=True,
                                  blank=True,
                                  default=1,
                                  verbose_name="测试用例版本")

    objects = models.Manager()

    class Meta:
        ordering = ['-id']
