import operator
from operator import itemgetter
import jsonschema


def check_json_schema(data, json_schema):
    if data is None or len(data) == 0:
        raise Exception('data is None')
    elif json_schema is None or len(json_schema) == 0:
        raise Exception('json_schema is None')
    else:
        return jsonschema.validate(data, json_schema)


# j1 = 'data__rows__[2]'
# j2 = 'data__rows__{customerName=非洲客户abc}__businessName'
def get_json_value_by_rule(data, rule: str):
    """
    1.取字典key里面对应的数组的值:data__rows__[3]
    2.取字典key里面对应的数组里匹配值的那一条:data__rows__{customerName=abc}
    3.取字典key对应的值
    """
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
    data_ = data
    for rule in rules.split('__'):
        a = get_json_value_by_rule(data_, rule)
        if a is None:
            return None
        else:
            data_ = a
            continue
    return data_
