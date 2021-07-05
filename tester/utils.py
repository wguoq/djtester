import operator
import jsonschema
from tester.domain.tt_enums import CheckOperator


def str_check(check_target, check_operator, check_expect):
    if check_operator == CheckOperator.EQ.value:
        return operator.eq(str(check_target), str(check_expect))
    elif check_operator == CheckOperator.NE.value:
        return operator.ne(str(check_target), str(check_expect))
    else:
        raise Exception(f'输入的是 {check_operator} 现在只支持 equals 和 not equals ')


def json_schema_check(target, json_schema):
    if target is None or len(target) == 0:
        raise Exception('target is None')
    elif json_schema is None or len(json_schema) == 0:
        raise Exception('json_schema is None')
    else:
        return jsonschema.validate(target, json_schema)

