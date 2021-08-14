import time

from django.test import TestCase
from appium import webdriver


# Create your tests here.


class TestTT(TestCase):

    def test_appium(self):
        desired_caps = dict()
        desired_caps['platformName'] = 'Android'
        desired_caps['platformVersion'] = '7.1.2'
        desired_caps['deviceName'] = 'emulator-5558'
        desired_caps['appPackage'] = 'com.android.settings'
        desired_caps['appActivity'] = '.Settings'
        driver = webdriver.Remote('http://127.0.0.1:4723/wd/hub', desired_caps)
        time.sleep(1)
        print(driver.session)
        time.sleep(5)

        print('aaaaaaaaaaaaa')
        driver.quit()

