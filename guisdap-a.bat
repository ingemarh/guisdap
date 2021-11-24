@echo off
set x=echo ;%PATH%;
if exist "guisdap_path.txt" (set /p mypath=<guisdap_path.txt) else (set /p mypath="Enter whole guisdap9 path: ")
if not exist "guisdap_path.txt" (echo %mypath% > guisdap_path.txt) 

matlab -nosplash -minimize -nodesktop /r "mypath=strtrim('%mypath%'); guisdap(mypath); analyse" -logfile c:\temp\logfile