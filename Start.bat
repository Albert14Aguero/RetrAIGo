@echo off
start cmd /k "cd /d retraigo-fe && npm start"
cd Prolog
swipl -s pl_server.pl
pause