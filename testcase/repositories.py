from testcase.models import Tc_Identity, Tc_Action, Tc_Data, Tc_Check_Point


class TestCaseIdentityDBHelper:
    def __init__(self, testcase_identity: Tc_Identity):
        self.identity = testcase_identity

    def save_this_one(self) -> Tc_Identity:
        # 无论新增还是修改都要判断id和name是否重复
        if self.has_case_id(self.identity.test_case_id):
            raise Exception(f'test_case_id {self.identity.test_case_id} 已存在')
        if self.has_case_name(self.identity.test_case_name):
            raise Exception(f'test_case_name {self.identity.test_case_name} 已存在')

        # 通过pk判断是否新增
        if self.identity.pk:
            # 修改
            # 查出数据修改
            try:
                self.identity = self.get_by(dict(pk=self.identity.pk))
            except Exception as e:
                raise Exception(f'TestCaseIdentity get id为 {self.identity.pk} 的数据报错: {e}')
            self.identity.save(update_fields=self.identity.update_fields())
            return self.identity
        else:
            # 新增
            self.identity.save()
            return self.identity

    @staticmethod
    def get_all(offset: int, limit: int):
        return Tc_Identity.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by(kwargs):
        a = Tc_Identity.objects.get(**kwargs)
        return a

    @staticmethod
    def filter_by(kwargs):
        return Tc_Identity.objects.filter(**kwargs)

    def has_case_id(self, test_case_id):
        if self.filter_by({"test_case_id": test_case_id}).count():
            return True
        else:
            return False

    def has_case_name(self, test_case_name):
        if self.filter_by({"test_case_name": test_case_name}).count():
            return True
        else:
            return False


class TestCaseActionDBHelper:
    def __init__(self, testcase_action: Tc_Action):
        self.action = testcase_action

    def save_this_one(self):
        # 通过pk判断是否新增
        if self.action.pk:
            # 修改
            # 查出数据修改
            try:
                self.action = self.get_by(dict(pk=self.action.pk))
            except Exception as e:
                raise Exception(f'TestCaseAction get id为 {self.action.pk} 的数据报错: {e}')
            self.action.save(update_fields=self.action.update_fields())
            return self.action

        else:
            # 新增
            self.action.save()
            return self.action

    @staticmethod
    def get_all(offset: int, limit: int):
        return Tc_Action.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by(kwargs):
        return Tc_Action.objects.get(**kwargs)

    @staticmethod
    def filter_by(kwargs):
        return Tc_Action.objects.filter(**kwargs)


class TestCaseDataDBHelper:
    def __init__(self, testcase_data: Tc_Data):
        self.data = testcase_data

    def save_this_one(self):
        # 通过pk判断是否新增
        if self.data.pk:
            # 修改
            # 查出数据修改
            try:
                self.data = self.get_by(dict(pk=self.data.pk))
            except Exception as e:
                raise Exception(f'TestCaseData get id为 {self.data.pk} 的数据报错: {e}')
            self.data.save(update_fields=self.data.update_fields())
            return self.data
        else:
            # 新增
            self.data.save()
            return self.data

    @staticmethod
    def get_all(offset: int, limit: int):
        return Tc_Data.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by(kwargs):
        return Tc_Data.objects.get(**kwargs)

    @staticmethod
    def filter_by(kwargs):
        return Tc_Data.objects.filter(**kwargs)


class TestCaseCheckPointDBHelper:
    def __init__(self, testcase_check_point: Tc_Check_Point):
        self.check_point = testcase_check_point

    def save_this_one(self):
        # 通过pk判断是否新增
        if self.check_point.pk:
            # 修改
            # 查出数据修改
            try:
                self.check_point = self.get_by(dict(pk=self.check_point.pk))
            except Exception as e:
                raise Exception(f'TestCaseCheckPoint get id为 {self.check_point.pk} 的数据报错: {e}')
            self.check_point.save(update_fields=self.check_point.update_fields())
            return self.check_point
        else:
            # 新增
            self.check_point.save()
            return self.check_point

    @staticmethod
    def get_all(offset: int, limit: int):
        return Tc_Check_Point.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by(kwargs):
        return Tc_Check_Point.objects.get(**kwargs)

    @staticmethod
    def filter_by(kwargs):
        return Tc_Check_Point.objects.filter(**kwargs)
