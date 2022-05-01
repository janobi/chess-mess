@echo off
cd /d %~dp0
taskkill /F /IM "ffpproject-exe.exe" /T > nul 2> nul
stack build
explorer http://127.0.0.1:8023
stack exec ffpproject-exe