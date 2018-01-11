import time
import pyautogui
import selenium
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
driver = webdriver.Chrome(executable_path='C:\\Users\chromedriver.exe')
time.sleep(3)
pyautogui.click(1233,34)
time.sleep(0.5)
pyautogui.click(1857,54)
time.sleep(0.5)
pyautogui.click(1833,246)
while not pyautogui.pixelMatchesColor(275, 432, (98,85,204)):
    time.sleep(0.5)
pyautogui.click(275,432)
time.sleep(5)
pyautogui.click(718,16)
time.sleep(1)
pyautogui.click(275,432)

while not pyautogui.pixelMatchesColor(375,272, (80,128,216)):
    time.sleep(0.5)
pyautogui.click(263,209); pyautogui.typewrite('Form Filler')
time.sleep(0.5)
pyautogui.press('enter')

while not pyautogui.pixelMatchesColor(1570,591,(192,209,238)):
    time.sleep(0.5)
    
pyautogui.click(1570,591)

while not pyautogui.pixelMatchesColor(1112,152, (68,163,223)):
    time.sleep(0.5)
pyautogui.click(1020,296)

while not pyautogui.pixelMatchesColor(1863,59, (68,164,224)):
    time.sleep(0.5)

pyautogui.click(718,21)
time.sleep(0.5)
pyautogui.click(478,18)
time.sleep(1)

for x in range(5):
    driver.get("https://missouristatechhs.az1.qualtrics.com/jfe/form/SV_e2wpkl3sGPWJnIV")
    while not pyautogui.pixelMatchesColor(758,191, (112,9,12)):
        time.sleep(0.5)
    pyautogui.click(1859,54)
    time.sleep(2)
    pyautogui.press('end')
    
    while not pyautogui.pixelMatchesColor(1386,937, (124, 183, 235)):
        time.sleep(0.5)
    pyautogui.click(1386,937)
    while not pyautogui.pixelMatchesColor(1324,290, (205,224,238)):
        time.sleep(0.5)

driver.close()








