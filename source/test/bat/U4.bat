echo off
rem Test trackereditor.exe via command line.

rem Console parameter: -U4
rem 	Sort the trackers list by name.

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
%path_to_enduser%trackereditor.exe %path_to_torrent% -U4

rem add some NEW trackers 
echo %t_b%> %add_trackers_txt%
echo %t_n%>> %add_trackers_txt%
echo %t_p%>> %add_trackers_txt%
echo %t_m%>> %add_trackers_txt%

rem modify the torrent folder
%path_to_enduser%trackereditor.exe %path_to_torrent% -U4

rem torrent must filled with trackers in this order: original(d,a,c,b) + new(b,n,p,m)
rem		-> sort my name -> a,b,c,d,m,n,p

ENDLOCAL
exit /b
