import abc

from pydantic import BaseModel


# class Tester:
#     def __init__(self):
#         self.test_case_code = None
#         self.test_case_name = None
#         self.test_case_result = None
#         self.message = None
#
#     @abc.abstractmethod
#     def run(self, *args, **kwargs):
#         pass


class RunTestResult(BaseModel):
    test_case_id: int = None
    test_case_code: str = None
    test_case_name: str = None
    test_case_result: str = None
    log: str = None
