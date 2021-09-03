from pydantic import BaseModel


class ApiAction(BaseModel):
    method: str = ""
    protocol: str = ""
    host: str = ""
    port: int = ""
    path: str = ""


class ApiParams(BaseModel):
    timeout: float = 120
    allow_redirects: bool = True
    verify: bool = False
    headers: dict = {}
    cookies: dict = {}
    data: dict = {}
    json_data: dict = {}
    files: dict = {}


class ApiCheckPoint(BaseModel):
    response_property: str = None
    rule: str = None
    operator: str = None
    expect: str = None


class ApiJsonSchemaCheckPoint(BaseModel):
    response_property: str = 'json'
    rule: str = None
    json_schema: str = None

