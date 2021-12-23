from django.db import models


class Public_Field(models.Model):
    created_time = models.DateTimeField(null=True,
                                        auto_now_add=True,
                                        verbose_name="创建时间")

    modified_time = models.DateTimeField(null=True,
                                         auto_now=True,
                                         verbose_name="修改时间")

    del_flag = models.CharField(max_length=1,
                                blank=True,
                                null=True,
                                verbose_name="逻辑删除标记")

    # 定义为抽象类,不会创建表
    class Meta:
        abstract = True


class Flow_Design(Public_Field):
    """
    流程设计表
    """

    flow_code = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 verbose_name="流程编码")

    flow_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="流程名称")

    flow_type = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 default='serial',
                                 verbose_name="流程类型: serial=串行;parallel=并行")

    flow_result_rule_id = models.CharField(max_length=64,
                                           blank=True,
                                           null=True,
                                           verbose_name="流程结果规则id")

    flow_status_rule_id = models.CharField(max_length=64,
                                           blank=True,
                                           null=True,
                                           verbose_name="流程状态规则id")

    version = models.IntegerField(blank=True,
                                  null=True,
                                  verbose_name="版本")

    version_status = models.IntegerField(blank=True,
                                         null=True,
                                         default=0,
                                         verbose_name="版本状态: -1=停用,0=草稿,1=启用")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_design"


class Flow_Result_Rule(Public_Field):
    """
    流程结果规则表
    """

    result_rule_type = models.CharField(max_length=64,
                                        blank=True,
                                        null=True,
                                        verbose_name="规则类型")

    result_rule_name = models.CharField(max_length=128,
                                        blank=True,
                                        null=True,
                                        verbose_name="规则名")

    flow_result = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="流程结果")

    result_rule_script = models.JSONField(null=True,
                                          blank=True,
                                          verbose_name="规则脚本")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_result_rule"


class Flow_Status_Rule(Public_Field):
    """
    流程状态规则表
    """

    status_rule_type = models.CharField(max_length=64,
                                        blank=True,
                                        null=True,
                                        verbose_name="规则类型")

    status_rule_name = models.CharField(max_length=128,
                                        blank=True,
                                        null=True,
                                        verbose_name="规则名称")

    flow_status = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="状态值")

    status_rule_script = models.JSONField(null=True,
                                          blank=True,
                                          verbose_name="规则脚本")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_status_rule"


class Node_Design(Public_Field):
    """
    流程节点设计表
    """

    node_code = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 verbose_name="节点编码")

    node_type = models.CharField(max_length=64,
                                 blank=True,
                                 null=True,
                                 default='func_node',
                                 verbose_name="节点类型: func_node | sub_flow")

    node_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="节点名称")

    start_rule_type = models.CharField(max_length=64,
                                       blank=True,
                                       null=True,
                                       default='and',
                                       verbose_name="启动条件类型：and | or | custom")

    node_func_name = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="节点业务方法名称")

    node_func_data = models.JSONField(null=True,
                                      blank=True,
                                      verbose_name="节点业务数据")

    version = models.IntegerField(blank=True,
                                  null=True,
                                  verbose_name="版本")

    version_status = models.IntegerField(blank=True,
                                         null=True,
                                         default=0,
                                         verbose_name="版本状态: -1=停用,0=草稿,1=启用")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_design"


class Node_Start_Rule(Public_Field):
    rule_target = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="目标: flow_data | node_result | node_status")

    rule_where = models.CharField(max_length=64,
                                  blank=True,
                                  null=True,
                                  verbose_name="目标flow_data用key | 目标node_result和node_status用node_design_id")

    rule_operator = models.CharField(max_length=64,
                                     blank=True,
                                     null=True,
                                     verbose_name="eq | nq | lt | le | gt | ge")

    rule_value = models.CharField(max_length=64,
                                  blank=True,
                                  null=True,
                                  verbose_name="匹配值")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="顺序id")


class Node_Status_Rule(Public_Field):
    """
    流程节点状态规则表
    """
    rule_order = models.IntegerField(blank=True,
                                     null=True,
                                     verbose_name="顺序")

    status_operator = models.CharField(max_length=64,
                                       blank=True,
                                       null=True,
                                       verbose_name="比较方式: eq;ne")

    status_target = models.CharField(max_length=128,
                                     null=True,
                                     blank=True,
                                     verbose_name="对比目标")

    node_status = models.CharField(max_length=64,
                                   blank=True,
                                   null=True,
                                   verbose_name="节点状态")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="流程设计id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_status_rule"


# class Node_Start_Rule_Design(Public_Field):
#     """
#     流程启动条件表
#     """
#     rule_type = models.CharField(max_length=64,
#                                  blank=True,
#                                  null=True,
#                                  default='and',
#                                  verbose_name="and | or")
#
#     rule_order = models.IntegerField(blank=True,
#                                      null=True,
#                                      verbose_name="顺序")
#
#     node_design = models.ForeignKey(to=Node_Design,
#                                     on_delete=models.SET_NULL,
#                                     blank=True,
#                                     null=True,
#                                     verbose_name="节点设计id")


class Flow_Node_Design_Oder(Public_Field):
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


class Flow_Instance(Public_Field):
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
                                 verbose_name="流程业务数据")

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


class Node_Instance(Public_Field):
    """
    流程节点实例表
    """

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    node_func_name = models.CharField(max_length=64,
                                      blank=True,
                                      null=True,
                                      verbose_name="节点业务方法名称")

    node_func_data = models.JSONField(null=True,
                                      blank=True,
                                      verbose_name="节点业务数据")

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
