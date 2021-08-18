import random
import time
from typing import Union

from pydantic import BaseModel

from testcase.domain.models.tc_api_model import ApiAction, ApiParams, ApiStrCheck, ApiJsonSchemaCheck
from testcase.domain.tc_enums import CaseType
from testcase.models import Tc_Identity, Tc_Action, Tc_Data, Tc_Check_Point


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

    def new_api_testcase(self) -> dict:
        test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
        self.test_case_type = CaseType.API.value
        self.tc_identity = Tc_Identity(test_case_id=test_case_id).fields_dict()
        self.tc_action = Tc_Action(action_type=ApiAction.__name__,
                                   action_name="", action=ApiAction().to_dict).fields_dict()
        self.tc_data = Tc_Data(data_type=ApiParams.__name__,
                               data_name="", data=ApiParams().to_dict).fields_dict()
        self.tc_check_list = [Tc_Check_Point(check_point_type=ApiStrCheck.__name__,
                                             check_point_name="",
                                             check_point=ApiStrCheck().to_dict).fields_dict(),
                              Tc_Check_Point(check_point_type=ApiJsonSchemaCheck.__name__,
                                             check_point_name="",
                                             check_point=ApiJsonSchemaCheck().to_dict).fields_dict()]

        return self.__dict__
