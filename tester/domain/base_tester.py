import operator
from pydantic import BaseModel
from djtester.enums import Operators


class BaseTester:
    def __init__(self):
        self.test_case_id = None
        self.test_case_name = None
        self.test_case_result = None
        self.message = None

    @staticmethod
    def verify_str(operator_, target, expect):
        if operator_ == Operators.EQ.value:
            return operator.eq(str(target), str(expect))
        elif operator_ == Operators.NE.value:
            return operator.ne(str(target), str(expect))
        else:
            raise Exception(f'无法识别的 check_point.operator = {operator_}')

