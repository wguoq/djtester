from typing import Union
from pydantic import BaseModel


class MobileAction(BaseModel):
    name: str = ""
    selector: str = ""
    xpath: str = ""
    action: str = ""
    data_key: str = ""

    @property
    def to_dict(self):
        return self.__dict__


class MobileData(BaseModel):
    name: str = ""
    mobile_data: dict = {}

    @property
    def to_dict(self):
        return self.__dict__


class MobileCheck(BaseModel):
    name: str = ""
    check_name: str = ""
    selector: str = ""
    xpath: str = ""
    attribute: str = ""
    operator: str = ""
    expect: str = ""

    @property
    def to_dict(self):
        return self.__dict__
