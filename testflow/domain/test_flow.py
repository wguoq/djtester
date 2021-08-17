"""
单数情况:
(A)用户(B)执行(C)case
A:  不同类型/权限-不会做;
B:  时间:定时执行-不会做;
    空间:远程/分布执行-不会做;
    不同的执行方式:命令行,web-不会做;
C:  不同类型的case ->tester类自己识别;
    不同版本的case-不会做
    不同测试环境的case->env_config todo;
复数情况:
(nA)用户(nB)执行(nC)case
nA: 性能-不会做;
nB: 多线程无序执行-不会做;
    有序执行->需要做个test_flow todo;
    ->从顺序派生出因果关系所以需要条件控制 todo;
    ->条件派生出需要状态机制 todo;
    时间:考虑长时间执行会中断,要从中断点继续,需要保存执行阶段到数据库...类似工作流-不会做;
nC: 有序有条件不同的case:
    派生出可以动态获取测试数据的需求->dynamic_data_rule todo;


test_flow设计:
主键->id
名称->name
存放各种数据比如config之类->data
流程状态->status: 'not run','running','break','finish'
存放复数case->step_list
step:
    类型->type
    顺序->order 一定要从0开始每次+1
    执行条件控制->rule
    状态->status:'not run','running','finish'
    case的主键->step_pk
    动态配置->dynamic_config
    case的结果->result

考虑到test_flow自身也可以作为一个step::
1.需要区分step里面是case还是flow-> step_type
2.flow也需要返回result->
就要有判断result的规则->result_rule:{}

可能应该区分流程设计表和流程实例表
"""
import time
from operator import itemgetter

from django.forms import model_to_dict

from testcase.repositories import TcIdentityDBHelper
from testflow.repositories import TestFlowSDBHelper

test_flow1 = {
    "id": None,
    "name": None,
    "result_rule": {},
    "result": None,
    "status": None,
    "data": {},
    "step_list": [
        {
            "step_id": None,
            "step_type": "test_case",
            "step_pk": 1,
            "step_name": "step01",
            "step_order": 0,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
    ]
}


# 先不考虑存数据库
class TestFlow:
    def __init__(self, test_flow: dict, tf_data: dict = None, n=0):
        self._n = n
        if self._n > 10:
            raise Exception(f'递归次数控制 n > 10 中断')
        self._tf = test_flow
        self._id = self._tf.get('id')
        self._name: str = self._tf.get('name')
        self._result_rule: dict = self._tf.get('result_rule')
        self._result: str = self._tf.get('result')
        self._status: str = self._tf.get('status')
        self._data: dict = self._tf.get('data')
        self._step_list: list = self._tf.get('step_list')
        if tf_data is None or len(tf_data) == 0:
            pass
        else:
            self._data.update(tf_data)
        self._step_result_list: list[dict] = []

    @property
    def id(self):
        return self._id

    @property
    def name(self):
        return self._name

    @property
    def result(self):
        return self._result

    @property
    def status(self):
        return self._status

    @property
    def step_result_list(self):
        return self._step_result_list

    def run(self):
        # 先判断流程状态,未完成的才运行
        # if self._status is None:
        #     raise Exception('流程状态status is None')
        if self._status == 'finish':
            raise Exception(f'流程已完成 status为 {self._status}')
        else:
            self._status = 'running'
            # 执行step_list
            self._run_step_list()
            # 根据_result_rule决定流程结果
            self._check_result_rule(self._step_result_list)
            # 根据流程结果决定流程状态
            self._set_flow_status()
        return self

    def _run_step_list(self):
        # 先对_step_list排一下序
        # 排序之前判断一下order字段有没有值
        for step in self._step_list:
            step_order = step.get('step_order')
            step_name = step.get('step_name')
            if step_order is None or len(str(step_order)) == 0:
                raise Exception(f'{step_name} 的step_order字段有没有值')
            elif not isinstance(step_order, int):
                raise Exception(f'{step_name} 的step_order字段 {step_order} 不是int ')
            elif step_order < 0:
                raise Exception(f'step_order 不能小于0 {step_order}')

        self._step_list = sorted(self._step_list, key=itemgetter('step_order'), reverse=False)

        for step in self._step_list:
            step_result = self._run_step(step)
            self._step_result_list.append(step_result)

    def _run_step(self, step):
        step_id = step.get('step_id')
        step_type = step.get('step_type')
        step_pk = step.get('step_pk')
        step_name = step.get('step_name')
        step_order = step.get('step_order')
        step_rule = step.get('step_rule')
        step_result = step.get('step_result')
        step_status = step.get('step_status')
        dynamic_data_rule = step.get('dynamic_data_rule')
        step_data = self._data

        def _step_return():
            return dict(step_id=step_id,
                        step_type=step_type,
                        step_name=step_name,
                        step_order=step_order,
                        step_result=step_result,
                        step_status=step_status)

        # 先判断是否满足运行条件
        # 1.上一步的step_status =='finish';
        # 2.本step的step_status !='finish';
        # 3._check_step_rule = True

        if step_order == 0:
            pass
        if step_order > 0:
            # 先排序保证index = step_order
            self._step_result_list = sorted(self._step_result_list, key=itemgetter('step_order'), reverse=False)
            pre_step_status = self._step_result_list[step_order - 1].get('step_status')
            if pre_step_status != 'finish':
                return _step_return()

        if step_status == 'finish':
            return _step_return()

        if self._check_step_rule(step_rule) is False:
            step_status = 'not run'
            return _step_return()
        else:
            step_status = 'running'

        # 先获取dynamic_data
        if dynamic_data_rule is None or len(dynamic_data_rule) == 0:
            pass
        else:
            dynamic_data = self._get_dynamic_data(dynamic_data_rule)
            step_data = self._data.update(dynamic_data)

        # 判断step_type,执行不同方法
        if step_type is None or len('step_type') == 0:
            raise Exception('step_type字段没有值')
        else:
            if step_type == 'test_case':
                step_result = self._run_test_case(step_pk, step_data)
            elif step_type == 'test_flow':
                step_result = self._run_test_flow(step_pk)
            else:
                raise Exception(f'不支持的的step_type {step_type}')

            if step_result:
                step_status = 'finish'
            return _step_return()

    def _check_step_rule(self, step_rule):
        # todo
        return True

    def _get_dynamic_data(self, dynamic_data_rule):
        # todo
        return {}

    @staticmethod
    def _run_test_case(step_pk, step_data):
        print(f'====_run_test_case==== {step_pk}')
        from djtester.testcase_service import TestCaseServicer
        from djtester.tester_service import TesterService
        test_case = TestCaseServicer.query.get_by_pk(step_pk)
        a = TesterService.run_testcase(test_case=test_case, test_config=step_data)
        return a

    def _run_test_flow(self, step_pk):
        self._n += 1
        print(f'=====_run_test_flow==== {step_pk}')
        tf = TestFlowSDBHelper.get_by(dict(pk=step_pk))
        run_tf = TestFlow(model_to_dict(tf), self._data, self._n)
        run_tf.run()
        # 子流程要先判断状态是不是完成或者终止
        if run_tf.status in ['finish', 'break']:
            return dict(result=run_tf.result)
        else:
            return None

    def _check_result_rule(self, step_list_result):
        # 需要定义各种类型的规则
        # todo
        self._result = 'pass'

    def _set_flow_status(self):
        # todo
        if self._result:
            self._status = 'finish'
        else:
            self._status = 'running'
