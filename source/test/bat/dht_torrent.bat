echo off
rem Test trackereditor.exe via command line.

rem remove all the trackers inside the torrent files

SETLOCAL

rem default variables
call variables.bat

rem empty remove_trackers_txt
echo.> %remove_trackers_txt%

rem empty remove_trackers_txt
echo.> %add_trackers_txt%

rem modify the torrent folder
%path_to_enduser%trackereditor.exe %path_to_torrent% 

ENDLOCAL
exit /b