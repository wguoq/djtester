import importlib


class TestCaseService:
    @staticmethod
    def test_case_service():
        module = importlib.import_module('testcase.service')
        return getattr(module, 'TestCaseService')


class TesterService:
    @staticmethod
    def tester_service():
        module = importlib.import_module('tester.service')
        return getattr(module, 'TesterService')
