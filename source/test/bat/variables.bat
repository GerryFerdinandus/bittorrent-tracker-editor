echo off
rem Test trackereditor.exe via command line.

rem create default variable for the test.

set "path_to_enduser=..\..\..\enduser\"
set "path_to_torrent=%cd%\..\torrent"
set "add_trackers_txt=%path_to_enduser%add_trackers.txt"
set "remove_trackers_txt=%path_to_enduser%remove_trackers.txt"

rem trackers
set "t_a=http://a.com/announce"
set "t_b=https://b.com/announce"
set "t_c=udp://c.com:80/announce"
set "t_d=udp://d.com:80/announce"


set "t_m=udp://m.com:80/announce"
set "t_n=udp://n.com:80/announce"
set "t_o=udp://o.com:80/announce"
set "t_p=udp://p.com:80/announce"

exit /b
