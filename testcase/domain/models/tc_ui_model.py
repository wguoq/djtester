from typing import Union
from pydantic import BaseModel


class UiAction(BaseModel):
    name: str = ""
    selector: str = ""
    xpath: str = ""
    action: str = ""
    data_key: str = ""

    @property
    def to_dict(self):
        return self.__dict__


class UiData(BaseModel):
    name: str = ""
    ui_data: dict = {}

    @property
    def to_dict(self):
        return self.__dict__


class UiCheck(BaseModel):
    name: str = ""
    selector: str = ""
    xpath: str = ""
    attribute: str = ""
    operator: str = ""
    expect: str = ""

    @property
    def to_dict(self):
        return self.__dict__
