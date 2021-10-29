import operator
from pydantic import BaseModel
from djtester.enums import Operators


class BaseTester:
    def __init__(self):
        self.test_case_id = None
        self.test_case_name = None
        self.test_case_result = None
        self.message = None


