====================
Console / automated mode
====================
Below this line is documentation for the more 'automated' (command line) use of trackereditor.
If you do not want to use it this way, then just remove both the files 'add_trackers.txt' and
'remove_trackers.txt' from the directory, and use the normal graphical program instead.

Console mode works identically on Windows, Linux and macOS.
Only the executable name/path and the shell syntax (curl, cron, .bat, etc.) differ per platform.

  Windows  : trackereditor.exe "C:\dir\torrent" -U4
  Linux    : ./trackereditor ../test_torrent -U4
  macOS    : /Applications/trackereditor.app/Contents/MacOS/trackereditor ../test_torrent -U4

These '.txt' files must be placed in the same directory as the program (on macOS: ~/.config/trackereditor/).
Exception: for the Linux AppImage, the files must instead be placed in the 'Original Working Directory' -
the directory the user was located in when they called/executed the AppImage, not the directory
where the AppImage file itself resides.

--------------------
trackereditor_cli (standalone console-only program)
--------------------
trackereditor_cli is a separate executable that runs the same console pipeline described above,
without any GUI/widgetset dependency.

Available for Windows and Linux only. There is no macOS build of trackereditor_cli;
use the trackereditor.app console mode shown above instead.

  Windows  : trackereditor_cli.exe "C:\dir\torrent" -U4
  Linux    : ./trackereditor_cli ../test_torrent -U4

Same command line syntax, add_trackers.txt/remove_trackers.txt files, and '-Ux'/-SAC/-SOURCE
parameters described in this document apply to trackereditor_cli as well.

--------------------
Basic command line syntax
--------------------
trackereditor <file_or_folder> -Ux [-SAC] [-SOURCE "tag"]

<file_or_folder>   Path to one '.torrent' file, or a folder containing '.torrent' files.
-Ux                 Mandatory. Selects the tracker list order/merge mode. See '-Ux parameter' below.
-SAC                Optional. Skip Announce Check (needed for private trackers).
-SOURCE "tag"       Optional. Set (or clear) the private tracker 'info:source' tag.

What tracker will be added/removed depends on the content of the add_trackers.txt
and remove_trackers.txt files described below.

--------------------
Additional Files: Trackers file, export file and log file
--------------------
These files can be optionally present in the same dir as the trackereditor executable file.
On macOS, the tracker files are read from and written to ~/.config/trackereditor/ (create the folder if needed).
Exception: for the Linux AppImage, these files are read from and written to the 'Original Working Directory' -
the directory the user was located in when they called/executed the AppImage, not the directory
where the AppImage file itself resides.
'add_trackers.txt' and 'remove_trackers.txt' are used in 'hidden console' mode and normal 'windows mode'.
'export_trackers.txt' and 'console_log.txt' are only created, never read.

add_trackers.txt
List of all the trackers that must added.


remove_trackers.txt
List of all the trackers that must removed
note: if the file is empty then all trackers from the present torrent will be REMOVED.
note: if the file is not present then no trackers will be automatic removed.
note: -U5 and -U6 keep the original tracker list of each torrent intact and remove nothing,
      so remove_trackers.txt is effectively ignored in those two modes.


export_trackers.txt
Created (overwritten) after every update with the final tracker list of the LAST processed torrent file.
Each tracker is followed by an empty line, so the content can be copy/pasted directly into
clients like uTorrent as separate tracker groups.


console_log.txt is only created in console mode.
Show the in console mode the success/failure of the torrent update.
First line status: 'OK' or 'ERROR: xxxxxxx' xxxxxxx = error description
Second line files count: '1'
Third line tracker count: 23
Second and third line info are only valid if the first line is 'OK'

--------------------
--- Linux usage example: 1 (public trackers)
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

--- Linux usage example: 4 (mixed torrents, do not touch removals via cron)
Some torrent folders contain a mix of private and public torrents, each with their own curated
tracker list. You only want to ADD a couple of extra public trackers without ever removing
anything, even if remove_trackers.txt is used elsewhere for other folders.

echo "udp://tracker.opentrackr.org:1337/announce" > add_trackers.txt
./trackereditor ../test_torrent -U6

Line 1: add_trackers.txt now contains one extra tracker to add.
Line 2: -U6 appends it AFTER each torrent's own existing tracker list, and removes nothing
        (remove_trackers.txt, if present, is ignored for this run). Every torrent keeps its
        own original tracker list fully intact.

Add this as a cron job (crontab -e), for example run once a day at 04:00:
0 4 * * * cd /path/to/enduser && ./trackereditor ../test_torrent -U6 >> cron.log 2>&1

--------------------
--- macOS usage example (public trackers)
macOS works the same way as Linux, the executable is inside the App bundle.

curl https://newtrackon.com/api/stable --output ~/.config/trackereditor/add_trackers.txt
echo > ~/.config/trackereditor/remove_trackers.txt
/Applications/trackereditor.app/Contents/MacOS/trackereditor ../test_torrent -U0

To run this automatically, add a launchd 'user agent' (~/Library/LaunchAgents/*.plist) instead
of cron, since recent macOS versions restrict cron's access to files/network by default.
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

Windows console example, using a .bat file with the same idea as "Linux usage example: 1":
curl https://newtrackon.com/api/stable --output add_trackers.txt
echo. > remove_trackers.txt
trackereditor.exe "C:\dir\torrent" -U0

This .bat file can be scheduled with Windows Task Scheduler to run automatically,
the same way cron is used on Linux/macOS.

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

'NEW' trackers  = the trackers coming from add_trackers.txt
'ORIGINAL' trackers = the trackers already present inside the torrent file being updated.

"Keep xxx intact" means that list stays together as one complete, unbroken block, in its
original order, at its named position (BEGIN or END) of the final trackers list.
When a tracker is present in both lists, the duplicate is always removed from the OTHER
(non-intact) list, never from the intact one.

Console parameter: -U0
	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
	The NEW trackers list is kept intact at the BEGIN.
	Duplicated trackers are removed from the ORIGINAL trackers list.

Console parameter: -U1
	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
	The ORIGINAL trackers list is kept intact at the END.
	Duplicated trackers are removed from the NEW trackers list.

Console parameter: -U2
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	The NEW trackers list is kept intact at the END.
	Duplicated trackers are removed from the ORIGINAL trackers list.

Console parameter: -U3
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	The ORIGINAL trackers list is kept intact at the BEGIN.
	Duplicated trackers are removed from the NEW trackers list.

Console parameter: -U4
	Sort the trackers list by name.
	Builds ONE combined list: the NEW trackers list plus the UNION of the ORIGINAL trackers
	found across ALL the torrent files being processed (not just each file's own trackers),
	duplicates removed. This combined list is sorted once, and the exact SAME sorted list
	is then written into every processed torrent file.
	note: every torrent file ends up with an identical tracker list, even if their original
	tracker lists were different from each other.

Console parameter: -U5
	Insert new trackers list BEFORE, the original trackers list inside the torrent file.
	Keep original tracker list unchanged and remove nothing.
	Each torrent file keeps its own original tracker list, even if they differ per file.
	Same duplicate removal rule as -U1: duplicated trackers are removed from the NEW trackers list,
	the ORIGINAL trackers list of each torrent stays fully intact.

Console parameter: -U6
	Append new trackers list AFTER, the original trackers list inside the torrent file.
	Keep original tracker list unchanged and remove nothing.
	Each torrent file keeps its own original tracker list, even if they differ per file.
	Same duplicate removal rule as -U3: duplicated trackers are removed from the NEW trackers list,
	the ORIGINAL trackers list of each torrent stays fully intact.

Console parameter: -U7
	Randomize the trackers list.
	Uses the same combined list as -U4 (the NEW trackers list plus the UNION of the ORIGINAL
	trackers found across ALL the torrent files being processed, duplicates removed), but
	shuffles it into a random order instead of sorting it.
	note: the random order is re-shuffled separately for each torrent file, so every file gets
	a different order, but every file still gets the same combined set of trackers.

--------------------
Acknowledgment:
This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit (http://www.openssl.org/)

