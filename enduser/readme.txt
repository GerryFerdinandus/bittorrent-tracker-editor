The below this line is for more the 'automated' use of trackereditor.
If you do not want to use it this way, then just remove both the files 'add_trackers.txt' and 'remove_trackers.txt' from the directory.

These '.txt' files must be place in the same directory as the program.

--------------------
--- Linux usage example : 1 (public trackers)
Update all the torrent files with only the latest tested stable tracker.

curl https://newtrackon.com/api/stable --output add_trackers.txt
echo > remove_trackers.txt
./trackereditor ../test_torrent -U0

Line 1: This will download the latest stable trackers into add_trackers.txt
Line 2: Remove_trackers.txt is now a empty file. All trackers from the present torrent will be REMOVED.
Line 3: Update all the torrent files inside the test_torrent folder.

note -U0 parameter can be change to -U4 (sort by name)
This is my prefer setting for the rtorrent client.
rtorrent client announce to all the trackers inside the torrent file.
I prefer to see all the trackers in alphabetical order inside rtorrent client console view.
rtorrent client need to have the 'session' folder cleared and restart rtorrent to make this working.
This can be run via regular cron job to keep the client running with 100% functional trackers.

--- Linux usage example: 2 (public trackers)
Mix the latest tested stable tracker with the present trackers already present inside the torrent files.

curl https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt  --output add_trackers.txt
curl https://raw.githubusercontent.com/ngosang/trackerslist/master/blacklist.txt  --output remove_trackers.txt
./trackereditor ../test_torrent -U0

Line 1: This will download the latest stable trackers into add_trackers.txt
Line 2: Remove_trackers.txt now contain blacklisted trackers
Line 3: Update all the torrent files inside the test_torrent folder.

Diference betwean example 1 vs 2
Example 1 is guarantee that all the trackers are working.
Example 2 You are responseble for the trackers working that are not part of add_trackers.txt

--- Linux usage example: 3 (private trackers)
In add_trackers.txt file manualy the private tracker URL
echo > remove_trackers.txt
./trackereditor ../test_torrent -U0 -SAC -SOURCE "abcd"

Line 2: Remove_trackers.txt is now a empty file. All trackers from the present torrent will be REMOVED.
Line 3: Update all the torrent files inside the test_torrent folder.
        -SAC (Skip Annouce Check) This is needed to skip private tracker URL check.
        -SOURCE Add private tracker source tag "abcd"

-SOURCE is optionally.
-SOURCE "" Empty sting will remove all the source tag
--------------------

Usage example Windows desktop short cut for private tracker user.
This is the same idea as "Usage example: 3 (private trackers)"
But start it from the desktop shortcut (double click) and not from windows console via bat file etc.

Desktop shortcut can have extra parameter append.
C:\Users\root\Documents\github\bittorrent-tracker-editor\enduser\trackereditor.exe ..\test_torrent -U0 -SAC -SOURCE abc

Make sure that add_trackers.txt is filled with the private URL
And remove_trackers.txt is a empty file.

--------------------

Console mode windows example:
Start program with a parameter to torrent file or dir
trackereditor.exe "C:\dir\torrent\file.torrent" -U4
trackereditor.exe "C:\dir\torrent" -U4
What tracker will be added/removed depend the content of the add_trackers.txt and remove_trackers.txt files.


Additional Files: Trackers file and log file 
These 3 files can be optionally present in the same dir as the trackereditor executable file.
'add_trackers.txt' and 'remove_trackers.txt' are used in 'hidden console' mode and normal 'windows mode'


add_trackers.txt
List of all the trackers that must added.


remove_trackers.txt
List of all the trackers that must removed
note: if the file is empty then all trackers from the present torrent will be REMOVED.
note: if the file is not present then no trackers will be automatic removed.


log.txt is only created in console mode.
Show the in console mode the success/failure of the torrent update.
First line status: 'OK' or 'ERROR: xxxxxxx' xxxxxxx = error description
Second line files count: '1'
Third line tracker count: 23
Second and third line info are only valid if the first line is 'OK'

--------------------

3 possible use case scenario in updating torrent file.
Add tracker and/or remove tracker

Add new tracker + keep tracker inside torrent
	Add trackers to the add_trackers.txt file
	remove_trackers.txt MUST NOT be present.	


Add new tracker + remove some or all tracker inside torrent.
	Add trackers to the add_trackers.txt file

	Add trackers to the remove_trackers.txt file
	or 
	Empty remove_trackers.txt file to remove all trackers from the present torrent


no new tracker + remove some or all tracker inside torrent.
	Empty add_trackers.txt file. 

	Add trackers to the remove_trackers.txt file.
	or 
	Empty remove_trackers.txt file to remove all trackers from the present torrent.
	This will create torrent files with out any tracker. (DHT torrent)

--------------------

Updated torrent file trackers list order with '-Ux' parameter.
This is a mandatory parameter.

trackereditor.exe "C:\dir\torrent" -U0

Console parameter: -U0
	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
	And remove possible duplicated trackers from the ORIGINAL trackers list.

Console parameter: -U1
	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
	And remove possible duplicated trackers from the NEW trackers list.

Console parameter: -U2
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	And remove possible duplicated trackers from the ORIGINAL trackers list.

Console parameter: -U3
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	And remove possible duplicated trackers from the NEW trackers list.

Console parameter: -U4
	Sort the trackers list by name.

Console parameter: -U5
	Append new trackers list BEFORE, the original trackers list inside the torrent file.
	Keep original tracker list unchanged and remove nothing.

Console parameter: -U6
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	Keep original tracker list unchanged and remove nothing.

Console parameter: -U7
	Randomize the trackers list.

--------------------
Acknowledgment:
This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit (http://www.openssl.org/)
