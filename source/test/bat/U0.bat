echo off
rem Test trackereditor.exe via command line.

rem Console parameter: -U0
rem 	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
rem 	And remove possible duplicated trackers from the ORIGINAL trackers list.

SETLOCAL

rem clean all torrent trackers
call dht_torrent.bat

rem default variables
call variables.bat

rem add some ORIGINAL trackers 
echo %t_d%> %add_trackers_txt%
echo %t_a%>> %add_trackers_txt%
echo %t_c%>> %add_trackers_txt%
echo %t_b%>> %add_trackers_txt%

rem remove_trackers_txt file.
del %remove_trackers_txt%

rem modify the torrent folder
%path_to_enduser%trackereditor.exe %path_to_torrent% -U0

rem add some NEW trackers 
echo %t_o%> %add_trackers_txt%
echo %t_b%>> %add_trackers_txt%
echo %t_p%>> %add_trackers_txt%
echo %t_m%>> %add_trackers_txt%

rem modify the torrent folder
%path_to_enduser%trackereditor.exe %path_to_torrent% -U0

rem torrent must filled with trackers in this order: new(o,b,p,m) + original(d,a,c,b) 
rem		-> remove duplicated from original -> o,b,p,m,d,a,c,

ENDLOCAL
exit /b
