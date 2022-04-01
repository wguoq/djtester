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


class Flow_Design(Time_Field, Code_Field):
    """
    流程设计表
    """

    fw_name = models.CharField(max_length=128,
                               blank=True,
                               null=True,
                               verbose_name="流程名称")

    fw_type = models.CharField(max_length=64,
                               blank=True,
                               null=True,
                               default='single',
                               verbose_name="流程类型",
                               help_text='single | multiple')

    fw_result_rule = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="结果规则id")

    fw_status_rule = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="状态规则id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_design"
        ordering = ['-id']


class Flow_Result_Rule(Time_Field):
    """
    流程结果规则表
    """
    rule_type = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 verbose_name="规则类型",
                                 help_text='default | script')

    rule_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="规则名称")

    rule_script = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="规则脚本")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_result_rule"
        ordering = ['-id']


class Flow_Status_Rule(Time_Field):
    """
    流程状态规则表
    """

    rule_type = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 verbose_name="规则类型",
                                 help_text='default | script')

    rule_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="规则名称")

    rule_script = models.JSONField(null=True,
                                   blank=True,
                                   verbose_name="规则脚本")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_status_rule"
        ordering = ['-id']


class Node_Design(Time_Field, Code_Field):
    """
    流程节点设计表
    """

    node_type = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 default='func',
                                 verbose_name="节点类型",
                                 help_text='func | flow')

    node_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="节点名称")

    start_rule_type = models.CharField(max_length=64,
                                       blank=True,
                                       null=True,
                                       default='and',
                                       verbose_name="启动条件类型",
                                       help_text='and | or')

    node_func_code = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="节点方法编码")

    node_func_data = models.JSONField(null=True,
                                      blank=True,
                                      verbose_name="节点数据")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_design"
        ordering = ['-id']


class Node_Start_Rule(Time_Field):
    rule_target = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="目标",
                                   help_text='flow_data | node_result | node_status')

    rule_where = models.CharField(max_length=64,
                                  blank=True,
                                  null=True,
                                  verbose_name="位置",
                                  help_text='flow_data用key | node_result和node_status用node_design_id')

    rule_operator = models.CharField(max_length=64,
                                     blank=True,
                                     null=True,
                                     verbose_name="操作",
                                     help_text='eq | ne')

    rule_value = models.CharField(max_length=64,
                                  blank=True,
                                  null=True,
                                  verbose_name="匹配值")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    class Meta:
        # 自定义表名
        db_table = "node_start_rule"
        ordering = ['-id']


class Node_Status_Rule(Time_Field):
    """
    节点状态规则表
    """

    status_operator = models.CharField(max_length=64,
                                       blank=True,
                                       null=True,
                                       verbose_name="操作",
                                       help_text='eq | ne')

    expect_result = models.CharField(max_length=128,
                                     null=True,
                                     blank=True,
                                     verbose_name="期望结果")

    node_status = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="节点状态")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_status_rule"
        ordering = ['-id']


class Flow_Node_Oder(Time_Field):
    """
    流程设计,节点顺序,节点设计关联表
    """

    flow_design = models.ForeignKey(to=Flow_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="流程设计id")

    node_order = models.IntegerField(blank=True,
                                     null=True,
                                     verbose_name="节点顺序")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    class Meta:
        # 自定义表名
        db_table = "flow_node_design_oder"
        ordering = ['-id']


class Flow_Instance(Time_Field):
    """
    流程实例表
    """

    flow_design = models.ForeignKey(to=Flow_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="流程设计id")

    flow_data = models.JSONField(null=True,
                                 blank=True,
                                 verbose_name="流程数据")

    flow_status = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   default='pending',
                                   verbose_name="流程状态")

    flow_result = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="流程结果")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_instance"
        ordering = ['-id']


class Node_Instance(Time_Field):
    """
    流程节点实例表
    """

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    node_func_code = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="节点方法编码")

    node_func_data = models.JSONField(null=True,
                                      blank=True,
                                      verbose_name="节点方法数据")

    node_order = models.IntegerField(blank=True,
                                     null=True,
                                     verbose_name="节点顺序")

    node_status = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   default='pending',
                                   verbose_name="节点状态")

    node_result = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="节点结果")

    flow_instance = models.ForeignKey(to=Flow_Instance,
                                      on_delete=models.SET_NULL,
                                      blank=True,
                                      null=True,
                                      verbose_name="流程实例id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_instance"
        ordering = ['-id']
