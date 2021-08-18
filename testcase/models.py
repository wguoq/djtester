from django.db import models


class Tc_Identity(models.Model):
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

    def fields_dict(self):
        return dict(test_case_id=self.test_case_id,
                    test_case_name=self.test_case_name)


class Tc_Action(models.Model):
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

    def fields_dict(self):
        return dict(action_type=self.action_type,
                    action_name=self.action_name,
                    action=self.action)


class Tc_Data(models.Model):
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

    def fields_dict(self):
        return dict(data_type=self.data_type,
                    data_name=self.data_name,
                    data=self.data)


class Tc_Check_Point(models.Model):
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

    def fields_dict(self):
        return dict(check_point_type=self.check_point_type,
                    check_point_name=self.check_point_name,
                    check_point=self.check_point)


class Test_Case(models.Model):
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

    def fields_dict(self):
        return dict(test_case_type=self.test_case_type,
                    tc_identity=self.tc_identity,
                    tc_action=self.tc_action,
                    tc_data=self.tc_data,
                    tc_check_list=self.tc_check_list)

    # def update_fields(self):
    #     a = not_none_fields(self.to_dict())
    #     if 'id' in a.keys():
    #         a.pop('id')
    #     if 'tc_check_list' in a.keys():
    #         a.pop('tc_check_list')
    #     return a.keys()

    class Meta:
        ordering = ['-id']
