import abc


class Tester:
    def __init__(self):
        self.test_case_code = None
        self.test_case_name = None
        self.test_case_result = None
        self.message = None

    @abc.abstractmethod
    def run(self, *args, **kwargs):
        pass

