import os
from ruamel import yaml


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
