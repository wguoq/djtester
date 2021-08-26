from djtester.all_app_service import TestCase


class TestCaseServiceAdapterLocal:
    @staticmethod
    def testcase_service():
        return TestCase.service()

    @staticmethod
    def get_test_case_by_pk(pk):
        a = TestCase.service()
        return a.TestCaseServicer().get_by_pk(pk)
