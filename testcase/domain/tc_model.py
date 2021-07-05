
import random
import time
from typing import Union

from pydantic import BaseModel


class TcTestCase(BaseModel):
    id: int = None
    test_case_type: str = None
    tc_identity: dict = None
    tc_action: dict = None
    tc_data: dict = None
    tc_check_list: list[dict] = None

    @property
    def to_dict(self):
        return self.__dict__
