import asyncio
import time

#
# async def count():
#     print("One")
#     await asyncio.sleep(1)
#     print("Two")
#
#
# async def main():
#     await asyncio.gather(count(), count(), count())
#
# asyncio.run(main())
from concurrent.futures import ThreadPoolExecutor

"""
函数前面加上 async 关键字，就变成了 async 函数。这种函数最大特点是执行可以暂停，交出执行权。
await 用来调用一个标记为 async 协程的方法
在 async 函数main的里面，asyncio.gather() 方法将多个异步任务（三个 count()）包装成一个新的异步任务，必须等到内部的多个异步任务都执行结束，这个新的异步任务才会结束。
三个 count() 依次执行，打印完 One，就休眠1秒钟，把执行权交给下一个 count()，所以先连续打印出三个 One。等到1秒钟休眠结束，执行权重新交回第一个 count()，开始执行 await 命令下一行的语句，所以会接着打印出三个Two。脚本总的运行时间是1秒。
"""

"""
使用 asyncio.get_event_loop() 可以构造并发执行
"""
#
# now_ = lambda: time.time()
#
#
# async def do_some_work(x):
#     print(f'开始等待: {x}')
#     await asyncio.sleep(x)
#     print(f'等待完成: {x}')
#     return f'等待时间为: {x}'
#
#
# start = now_()
#
# loop = asyncio.get_event_loop()

# tasks = []
# for i in range(5):
#     tasks.append(asyncio.ensure_future(do_some_work(i)))
# ttt = asyncio.wait(tasks)
# print(f'开始loop')
# loop.run_until_complete(ttt)
# print(f'loop完成')
# loop.close()
#
# print('TIME: ', now_() - start)
# for task in tasks:
#     print(task.result())

#
# def callback(future):
#     print(f'返回值：{future.result()}')
#
#
# tasks = []
# for i in range(5):
#     task = loop.create_task(do_some_work(i))
#     task.add_done_callback(callback)
#     tasks.append(task)
#
# ttt = asyncio.wait(tasks)
# print(f'开始loop')
# loop.run_until_complete(ttt)
# print(f'loop完成')
# loop.close()


# 你可能发现，移动任何 ORM 代码到它自己的函数中并使用 sync_to_async() 来调用整个函数会更容易。例如：
#
# from asgiref.sync import sync_to_async
#
# def _get_blog(pk):
#     return Blog.objects.select_related('author').get(pk=pk)
#
# get_blog = sync_to_async(_get_blog, thread_sensitive=True)


# from concurrent.futures import ThreadPoolExecutor
# import asyncio
# from goods import models
# from . import items
#
#
# class WebspidersPipeline:
#     '''todo 异步存储'''
#
#     # 创建事件循环对象
#     loop = asyncio.get_event_loop()
#     # 创建线程池
#     executor = ThreadPoolExecutor()
#     # 任务队列
#     tasks = []
#
#     # 处理不同的pipline
#     async def process_item(self, item, spider):
#         if isinstance(item, items.GoodsItem):
#             return self.process_goods_item(item, spider)
#         elif isinstance(item, items.GoodsSizeItem):
#             return self.process_goods_size_item(item, spider)
#         elif isinstance(item, items.GoodsStockItem):
#             return self.process_goods_stock_item(item, spider)
#         return item
#
#     def process_goods_item(self, item, spider):
#         '''将保存数据的处理方法加入到任务队列'''
#         task = self.loop.run_in_executor(self.executor, self.executor_func(models.Goods, item), )
#         self.tasks.append(task)
#         return item
#
#     def process_goods_size_item(self, item, spider):
#         task = self.loop.run_in_executor(self.executor, self.executor_func(models.GoodsSize, item), )
#         self.tasks.append(task)
#         return item
#
#     def process_goods_stock_item(self, item, spider):
#         task = self.loop.run_in_executor(self.executor, self.executor_func(models.GoodsStock, item), )
#         self.tasks.append(task)
#         return item
#
#     @staticmethod
#     def executor_func(model, item):
#         '''主要作用是将有参数的函数转换为无参数的函数返回,方便run_in_executor方法调用,这个方法它只接受位置传参，不接受关键字传参'''
#
#         def func():
#             return model.objects.create(**item)
#
#         return func
#
#     def close_spider(self, spider):
#         '''当爬虫关闭的时候调用这个方法保存数据'''
#         self.loop.run_until_complete(asyncio.wait(self.tasks))


def long_blocking_function():
    print(time.time())
    time.sleep(2)
    return True



loop = asyncio.get_event_loop()
# 新建线程池
pool = ThreadPoolExecutor()
# 任务列表
tasks = [loop.run_in_executor(pool, long_blocking_function),
         loop.run_in_executor(pool, long_blocking_function)]
# 等待所有任务结束并返回
ttt = asyncio.gather(*tasks)

now = time.time()
loop.run_until_complete(ttt)
print(time.time() - now)