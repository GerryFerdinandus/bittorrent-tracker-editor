The below this line is for more the 'automated' use of trackereditor.
If you do not want to use it this way, then just remove both the files 'add_trackers.txt' and 'remove_trackers.txt' from the directory.

These '.txt' files must be place in the same directory as the program.

--------------------

Console mode:
Start program with a parameter to torrent file or dir
trackereditor.exe "C:\dir\torrent\file.torrent" -U4
trackereditor.exe "C:\dir\torrent" -U4
What tracker will be added/removed depend the content of the add_trackers.txt and remove_trackers.txt files.

--------------------

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
