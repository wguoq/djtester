from djtester.repositories import BaseRepository
from .apps import TestcaseConfig
from .models import *


class TestApiRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, TestApi.__name__)


class ApiTestCaseRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, ApiTestCase.__name__)


class ApiTestDataRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, ApiTestData.__name__)


class ApiTestCheckPointRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, ApiTestCheckPoint.__name__)
