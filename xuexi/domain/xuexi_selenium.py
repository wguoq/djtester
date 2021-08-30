from enum import Enum
from time import sleep
from selenium import webdriver
from selenium.webdriver import ActionChains, TouchActions
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait


def sele_wait(d):
    d.get('https://ceshiren.com/')
    sleep(1)
    # 全局隐式等待间隔0.5秒
    d.implicitly_wait(3)
    # a = d.find_element(By.ID, 'ember402').click()
    # 显示等待间隔0.5秒
    # 给显示等待的专用方法 expected_conditions
    # locator是一个list=(By.ID, selector)
    # WebDriverWait(d, 5).until(expected_conditions.element_to_be_clickable((By.CSS_SELECTOR, '*[title="在最近的一年，一月，一周或一天最活跃的话题"]')))
    WebDriverWait(d, 5).until(expected_conditions.element_to_be_clickable((By.LINK_TEXT, "热门")))
    # d.find_element_by_css_selector('*[title="在最近的一年，一月，一周或一天最活跃的话题"]').click()
    d.find_element(By.LINK_TEXT, "热门").click()
    sleep(1)

    # WebDriverWait(d, 5).until(expected_conditions.element_to_be_clickable((By.XPATH, '//*[@title="有新帖的话题"]')))
    WebDriverWait(d, 5).until(expected_conditions.element_to_be_clickable((By.LINK_TEXT, "最新")))
    # d.find_element_by_xpath('//*[@title="有新帖的话题"]').click()
    d.find_element(By.LINK_TEXT, "最新").click()
    sleep(1)


def sele_click_send_keys(d):
    d.get('https://ceshiren.com/')
    sleep(1)
    WebDriverWait(d, 5).until(expected_conditions.visibility_of_any_elements_located((By.ID, 'search-button')))
    d.find_element_by_id('search-button').click()
    sleep(1)

    WebDriverWait(d, 5).until(expected_conditions.visibility_of_any_elements_located((By.ID, 'search-term')))
    d.find_element_by_id('search-term').send_keys('selenium')
    sleep(3)

    WebDriverWait(d, 5).until(expected_conditions.visibility_of_any_elements_located((By.CSS_SELECTOR,
                                                                                      '*#ember7 > header > div > div > div.panel.clearfix > div > div > div > div > div.results > div.main-results > div > ul > li:nth-child(1) > a')))
    # d.find_element_by_xpath('//div[@id="ember7"]/header/div/div/div[2]/div/div/div/div/div[3]/div/div/ul/li/a/span/span[2]').click()
    d.find_element_by_css_selector('*#ember7 > header > div > div > div.panel.clearfix > div > div > div > div > '
                                   'div.results > div.main-results > div > ul > li:nth-child(1) > a').click()
    sleep(3)

    text = d.find_element_by_css_selector(
        '*#post_1 > div > div.topic-body.clearfix > div.regular.contents > div > h2:nth-child(5)').text


def sele_ActionChains(d):
    # ActionChains 按顺序装入对元素的操作行为,最后用action.perform()实施,.pause暂停1秒
    # ActionChains的典型用法1:拖拽操作
    d.get('http://sahitest.com/demo/dragDropMooTools.htm')
    dragger = d.find_element(By.CSS_SELECTOR, "*#dragger")
    item1 = d.find_element(By.CSS_SELECTOR, "body > div:nth-child(4)")
    action = ActionChains(d)
    action.click_and_hold(dragger).pause(1)
    action.move_to_element(item1).pause(1)
    action.release()
    action.perform()
    sleep(5)

    # ActionChains的典型用法2:模拟键盘特殊按键,或者逐个输入字符
    d.get('https://ceshiren.com/')
    sleep(1)
    WebDriverWait(d, 5).until(expected_conditions.visibility_of_any_elements_located((By.ID, 'search-button')))
    d.find_element_by_id('search-button').click()
    sleep(1)

    WebDriverWait(d, 5).until(expected_conditions.visibility_of_any_elements_located((By.ID, 'search-term')))
    search_term = d.find_element_by_id('search-term')
    # 因为ActionChains只能传入driver,所以要指定一个输入框操作的话就要先点击这个输入框
    search_term.click()
    action = ActionChains(d)
    action.send_keys("selenium").pause(1)
    # Keys类有所有键盘上的特殊按钮
    action.send_keys(Keys.SPACE).pause(1)
    action.send_keys('webdriver').pause(1)
    action.send_keys(Keys.ENTER).pause(1)
    action.perform()
    sleep(5)


def sele_TouchActions(d: webdriver):
    # TouchActions 提供H5页面的滑动,点击等操作,需要添加option才行
    # option = webdriver.ChromeOptions();
    # option.add_experimental_option('w3c',False);
    # d = webdriver.Chrome(options=option)
    d.get('https://ceshiren.com/')
    t_action = TouchActions(d)
    t_action.scroll(0, 2000)
    t_action.perform()
    sleep(5)


def sele_window_handle(d: webdriver):
    d.get('https://www.jd.com/')
    # 对于打开新窗口的情况,获取当前窗口,打开新窗口,获取所有窗口,用window_handle切换
    current_window_handle = d.current_window_handle
    print(current_window_handle)
    d.find_element_by_link_text('你好，请登录').click()
    d.find_element_by_link_text('立即注册').click()
    window_handles = d.window_handles
    print(window_handles)
    window_handles.remove(current_window_handle)
    print(window_handles)
    d.switch_to.window(window_handles[-1])
    d.find_element(By.LINK_TEXT, '《京东用户注册协议》').click()
    sleep(5)

def sele_frame(d: webdriver):
    # frameset,frame,iframe
    # frameset没变化直接用id定位
    # frame: 指定切换d.switch_to.frame();切换到默认d.switch_to.default_content(); 切换到父节点 d.switch_to.parent_frame()
    # iframe
    d.get('https://www.runoob.com/try/try.php?filename=tryhtml5_draganddrop')
    # ID或者name
    d.switch_to.frame('iframeResult')
    d.switch_to
    drag = d.find_element(By.ID, "drag1")
    drop = d.find_element(By.ID, "div1")
    actions = ActionChains(d)
    actions.drag_and_drop(drag, drop).perform()
    sleep(3)

class CaseType(Enum):
    API = 'api'


if __name__ == '__main__':
    # option = webdriver.ChromeOptions()
    # option.add_experimental_option('w3c', False)
    # driver = webdriver.Chrome(options=option)
    # driver.implicitly_wait(3)
    # sele_wait(driver)
    # sele_click_send_keys(driver)
    # sele_TouchActions(driver)
    # sele_window_handle(driver)
    # sele_frame(driver)
    # driver.quit()

    print(CaseType.__name__)