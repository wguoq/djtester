from testflow.models import Test_Flow


class TestFlowSDBHelper:
    def __init__(self, test_flow: Test_Flow):
        self.tf = test_flow

    def save_this_one(self):
        # 通过pk判断是否新增
        if self.tf.pk:
            # 修改
            # 查出数据修改
            try:
                self.tf = self.get_by(dict(pk=self.tf.pk))
            except Exception as e:
                raise Exception(f'TestFlowS get id为 {self.tf.pk} 的数据报错: {e}')
            self.tf.save(update_fields=self.tf.update_fields())
            return self.tf

        else:
            # 新增
            self.tf.save()
            return self.tf

    @staticmethod
    def get_all(offset: int, limit: int):
        return Test_Flow.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by(kwargs):
        return Test_Flow.objects.get(**kwargs)

    @staticmethod
    def filter_by(kwargs):
        return Test_Flow.objects.filter(**kwargs)
