@echo off
REM 一键提交并推送 RStudio Project 到 GitHub
REM 修改下面的路径为你的项目路径
cd "C:\Users\Rao Wanglin\Desktop\homework"

REM 添加所有修改
git add .

REM 提交，修改这里的提交信息即可
set /p msg=Enter commit message: 
git commit -m "%msg%"

REM 推送到 GitHub
git push origin main

pause
