from pydantic import BaseModel


class ApiAction(BaseModel):
    method: str = ""
    protocol: str = ""
    host: str = ""
    port: int = ""
    path: str = ""

    @property
    def to_dict(self):
        return self.__dict__


class ApiParams(BaseModel):
    timeout: float = 120
    allow_redirects: bool = True
    verify: bool = False
    headers: dict = {}
    cookies: dict = {}
    data: dict = {}
    json_data: dict = {}
    files: dict = {}

    @property
    def to_dict(self):
        return self.__dict__


class ApiStrCheck(BaseModel):
    response_property: str = ""
    property_key: str = ""
    operator: str = ""
    expect: str = ""

    @property
    def to_dict(self):
        return self.__dict__


class ApiJsonSchemaCheck(BaseModel):
    json_schema: str = ""

    @property
    def to_dict(self):
        return self.__dict__
