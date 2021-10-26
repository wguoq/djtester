import importlib


class TestCaseService:
    @staticmethod
    def test_case_servicer():
        module = importlib.import_module('testcase.service')
        return getattr(module, 'TestCaseServicer')


class TesterService:
    @staticmethod
    def tester_servicer():
        module = importlib.import_module('tester.service')
        return getattr(module, 'TesterServicer')
