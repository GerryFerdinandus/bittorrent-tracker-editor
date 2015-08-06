The below this line is for more the 'automated' use of trackereditor.
If you do not want to use it this way, then just remove both the files 'add_trackers.txt' and 'remove_trackers.txt' from the directory.

These '.txt' files must be place in the same directory as the program.


Console mode:
Start program with a parameter to torrent file or dir
trackereditor.exe "C:\dir\torrent\file.torrent"
trackereditor.exe "C:\dir\torrent"
What tracker will be added/removed depend the content of the add_trackers.txt and remove_trackers.txt files.

--------------------
Additional Files: Trackers file and log file 
These 3 files can be optionaly present in the same dir as the trackereditor excutable file.
'add_trackers.txt' and 'remove_trackers.txt' are used in 'hidden console' mode and normal 'windows mode'


add_trackers.txt
List of all the trackers that must added.


remove_trackers.txt
List of all the trackers that must removed
note: if the file is empty then all trackers from the present torrent will be REMOVED.
note: if the file is not present then no trackers will be automatickly removed.



log.txt is only created in console mode.
Show the in console mode the succes/failure of the torrent update.
First line status: 'OK' or 'ERROR: xxxxxxx' xxxxxxx = error description
Second line files count: '1'
Third line tracker count: 23
Second and third line info are only valid if the first line is 'OK'

--------------------


3 posible use case scenario in updating torrent file.
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




