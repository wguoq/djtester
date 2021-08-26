import importlib


class TestCase:
    @staticmethod
    def service():
        return importlib.import_module('testcase.service')


class Tester:
    @staticmethod
    def service():
        return importlib.import_module('tester.service')

