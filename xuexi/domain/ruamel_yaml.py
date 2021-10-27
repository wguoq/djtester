"""
yaml作为配置文件是非常友好的一种格式，前面一篇讲了yaml的一些基础语法和读取方法，本篇继续讲yaml文件写入方法
用yaml模块写入字典嵌套字典这种复杂的数据，会出现大括号{ }，不是真正的yaml文件数据，可以用ruamel模块就解决。
安装方法：
pip install ruamel.yaml
"""

import os
from ruamel import yaml



# 将字典写入到yaml
desired_caps = {
    'platformName': 'Android',
    'platformVersion': '7.0',
    'deviceName': 'A5RNW18316011440',
    'appPackage': 'com.tencent.mm',
    'appActivity': '.ui.LauncherUI',
    'automationName': 'Uiautomator2',
    'unicodeKeyboard': True,
    'resetKeyboard': True,
    'noReset': True,
    'chromeOptions': {'androidProcess': 'com.tencent.mm:tools'}
}

curpath = os.path.dirname(os.path.realpath(__file__))
yamlpath = os.path.join(curpath, "caps.yaml")

# 写入到yaml文件
with open(yamlpath, "w", encoding="utf-8") as f:
    yaml.dump(desired_caps, f, Dumper=yaml.RoundTripDumper)

# 读取
file = open(yamlpath, "r", encoding="utf-8")
file_data = file.read()
file.close()
a = yaml.load(file_data, Loader=yaml.Loader)
print(a)
