echo off
rem Test trackereditor.exe via command line.

rem Console parameter: -U3
rem 	Append new trackers list AFTER, the original trackers list inside the torrent file.
rem 	And remove possible duplicated trackers from the NEW trackers list.

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
%path_to_enduser%trackereditor.exe %path_to_torrent% -U3

rem add some NEW trackers 
echo %t_d%> %add_trackers_txt%
echo %t_n%>> %add_trackers_txt%
echo %t_a%>> %add_trackers_txt%
echo %t_m%>> %add_trackers_txt%

rem modify the torrent folder
%path_to_enduser%trackereditor.exe %path_to_torrent% -U3

rem torrent must filled with trackers in this order: original(d,a,c,b) + new(d,n,a,m)
rem		-> remove duplicated from new  -> d,a,c,b,n,m

ENDLOCAL
exit /b
