from django.db import models


class Public_Field(models.Model):
    created_time = models.DateTimeField(blank=True,
                                        null=True,
                                        verbose_name="创建时间")

    modified_time = models.DateTimeField(blank=True,
                                         null=True,
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
    id = models.AutoField(primary_key=True)

    code = models.CharField(max_length=32,
                            blank=True,
                            null=True,
                            verbose_name="编码")

    flow_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="流程名称")

    flow_type = models.CharField(max_length=32,
                                 blank=True,
                                 null=True,
                                 verbose_name="流程类型: 1=串行;2=并行")

    # flow_node_list = models.JSONField(null=True,
    #                                   blank=True,
    #                                   verbose_name="流程节点列表")

    flow_data_type = models.CharField(max_length=32,
                                      blank=True,
                                      null=True,
                                      verbose_name="流程数据类型")

    # flow_status_rule = models.ForeignKey(to=Flow_Status_Rule,
    #                                      on_delete=models.SET_NULL,
    #                                      null=True,
    #                                      blank=True,
    #                                      verbose_name="状态判断id")

    # flow_result_rule = models.JSONField(null=True,
    #                                     blank=True,
    #                                     verbose_name="结果判断规则")

    version = models.IntegerField(blank=True,
                                  null=True,
                                  verbose_name="版本")

    version_status = models.CharField(max_length=8,
                                      blank=True,
                                      null=True,
                                      verbose_name="版本状态: -1=停用,0=草稿,1=启用")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_design"


class Flow_Status_Rule(Public_Field):
    id = models.AutoField(primary_key=True)

    flow_status = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="状态值")

    flow_status_rule = models.JSONField(null=True,
                                        blank=True,
                                        verbose_name="规则")

    flow_design = models.ForeignKey(to=Flow_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="关联流程设计id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_status_rule"


class Flow_Result_Rule(Public_Field):
    id = models.AutoField(primary_key=True)

    flow_result = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="状态值")

    flow_result_rule = models.JSONField(null=True,
                                        blank=True,
                                        verbose_name="规则")

    flow_design = models.ForeignKey(to=Flow_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="关联流程设计id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_result_rule"


class Node_Design(Public_Field):
    """
    流程节点设计表
    """
    id = models.AutoField(primary_key=True)

    code = models.CharField(max_length=32,
                            blank=True,
                            null=True,
                            verbose_name="编码")

    node_name = models.CharField(max_length=128,
                                 blank=True,
                                 null=True,
                                 verbose_name="节点名称")

    node_type = models.CharField(max_length=32,
                                 blank=True,
                                 null=True,
                                 verbose_name="节点类型")

    node_data_type = models.CharField(max_length=32,
                                      blank=True,
                                      null=True,
                                      verbose_name="节点数据类型")

    node_start_rule = models.JSONField(null=True,
                                       blank=True,
                                       verbose_name="节点启动规则")

    # node_status_rule = models.ForeignKey(to=Node_Status_Rule,
    #                                      on_delete=models.SET_NULL,
    #                                      null=True,
    #                                      blank=True,
    #                                      verbose_name="节点状态判断id")

    version = models.IntegerField(blank=True,
                                  null=True,
                                  verbose_name="版本")

    version_status = models.CharField(max_length=8,
                                      blank=True,
                                      null=True,
                                      verbose_name="版本状态: -1=停用,0=草稿,1=启用")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_design"


class Node_Status_Rule(Public_Field):
    id = models.AutoField(primary_key=True)

    node_status = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="状态值")
    operator = models.CharField(max_length=32,
                                blank=True,
                                null=True,
                                verbose_name="比较方式: eq;ne")

    return_result = models.JSONField(null=True,
                                     blank=True,
                                     verbose_name="返回的结果值")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="关联流程设计id")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_status_rule"


class Flow_Instance(Public_Field):
    """
    流程实例表
    """
    id = models.AutoField(primary_key=True)

    code = models.CharField(max_length=32,
                            blank=True,
                            null=True,
                            verbose_name="编码")

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
                                   default='ready',
                                   verbose_name="流程状态")

    flow_result = models.CharField(max_length=32,
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
    id = models.AutoField(primary_key=True)

    code = models.CharField(max_length=32,
                            blank=True,
                            null=True,
                            verbose_name="编码")

    node_design = models.ForeignKey(to=Node_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="节点设计id")

    node_data = models.JSONField(null=True,
                                 blank=True,
                                 verbose_name="节点数据")

    node_status = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="节点状态")

    node_result = models.CharField(max_length=32,
                                   blank=True,
                                   null=True,
                                   verbose_name="节点结果")

    objects = models.Manager()

    class Meta:
        # 自定义表名
        db_table = "flow_node_instance"


class Flow_Design_Node(Public_Field):
    id = models.AutoField(primary_key=True)

    node_order = models.IntegerField(blank=True,
                                     null=True,
                                     verbose_name="节点顺序")

    node_instance = models.ForeignKey(to=Node_Instance,
                                      on_delete=models.SET_NULL,
                                      null=True,
                                      verbose_name="关联节点实例id")

    flow_design = models.ForeignKey(to=Flow_Design,
                                    on_delete=models.SET_NULL,
                                    blank=True,
                                    null=True,
                                    verbose_name="关联节点设计id")

    class Meta:
        # 自定义表名
        db_table = "flow_design_node"
