import operator
from operator import itemgetter
import jsonschema
from tester.domain.tt_enums import CheckOperator


def str_check(check_target, check_operator, check_expect):
    print(f'check_target=={check_target}\n check_expect===={check_expect}')
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

# j1 = 'data__rows__[2]'
# j2 = 'data__rows__{customerName=非洲客户abc}__businessName'
def jsom_value_getter(data, rule: str):
    # 是[1]模式,data需要是list
    if rule.startswith('[') and rule.endswith(']'):
        # 去掉头尾的[]
        rule = rule.strip('[').strip(']')
        return data[int(rule)]
    # 是{k=v}模式,data需要是list[dict]
    elif rule.startswith('{') and rule.endswith('}'):
        # 去掉头尾的{}
        rule = rule.strip('{').strip('}')
        # 以=分隔
        r_list = rule.split('=')
        # 遍历取出指定字段比较,返回第一个匹配到的
        for d in data:
            getter = itemgetter(r_list[0])
            if operator.eq(getter(d), r_list[1]):
                return d
            else:
                continue
        return None
    # 是key模式data需要是dict
    else:
        return data.get(rule)


def get_json_value(data, rules):
    _data = data
    for rule in rules.split('__'):
        a = jsom_value_getter(_data, rule)
        if a is None:
            return None
        else:
            _data = a
            continue
    return _data


