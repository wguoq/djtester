import operator
import os
from ruamel import yaml

from djtester.enums import Operators


def get_node_func_list() -> dict:
    cur_path = os.path.dirname(os.path.realpath(__file__))
    node_func_list = os.path.join(cur_path, "node_func_list.yaml")
    file = open(node_func_list, 'r', encoding="utf-8")
    read = file.read()
    file.close()
    data: dict = yaml.load(read, Loader=yaml.Loader)
    if data is None:
        data = {}
    return data


def verify_str(a, operator_, b):
    if operator_ == Operators.EQ.value:
        return operator.eq(str(a), str(b))
    elif operator_ == Operators.NE.value:
        return operator.ne(str(a), str(b))
    elif operator_ == Operators.LT.value:
        return operator.lt(str(a), str(b))
    elif operator_ == Operators.LE.value:
        return operator.le(str(a), str(b))
    elif operator_ == Operators.GT.value:
        return operator.gt(str(a), str(b))
    elif operator_ == Operators.GE.value:
        return operator.ge(str(a), str(b))
    else:
        raise Exception(f'无法识别的 check_point.operator = {operator_}')