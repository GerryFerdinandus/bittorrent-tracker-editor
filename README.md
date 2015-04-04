**bittorrent-tracker-editor** will add/remove bittorrent tracker from the torrent file(s).
This software works on Windows XP SP3, Windows 7+

---

## Warning: ##
There is no backup function in this software. Used it at your own risk. Bittorrent work fine without this program. You probably don't need this software.

---

## Which program to use for add/remove bittorrent trackers? ##
  * **Edit one torrent file:** You can use http://torrenteditor.com/
  * **Edit multiple torrent file:** Use this program. It is made for changing multiple torrent files.

---

## Features: ##
  * Select one torrent file or a folder with torrent files.
  * Add one or more tracker at the same time.
  * Remove one or more tracker at the same time.
  * Remove all the tracker to create tracker less torrent. DHT torrent
  * Change public/private flag. Warning: This will change the torrent info HASH.
  * Preset add/remove tracker via add\_trackers.txt and remove\_trackers.txt files when present in the same folder as the executable file.
  * Optional start as console program. (See readme.txt inside download)
  * Show torrent files content.

---

## Downloads: ##
  * [From GitHub: Executable file for win32](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)

---

## Software version ##
### 1.31 ###
  * Add: Edit comment in data/info grid column.
  * FIX:  ([Issue 6](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/6))

### 1.30 ###
  * Add: Tab page Files/trackers/info
  * Add: Optional start as console program. (See readme.txt inside download)
  * Add: remove\_trackers.txt will remove specific trackers form torrent.
  * Add: export\_trackers.txt is created after updating the torrent.
  * Add: drag and drop of trackers file (with '.txt' file extension)
  * FIX:  ([Issue 4](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/4)) + ([Issue 5](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/5))

### 1.21 ###
  * FIX: Support for Unicode in filename. (Chinese etc.)

### 1.20 ###
  * Add: Tab page torrent info/data.
  * Add: Drag & Drop torrent files or a folder with torrent files inside.

### 1.10 ###
  * Add: Tab page for public/private flag. ([Issue 1](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/1))
  * Add: Load tracker list from file via menu or at start-up, when file add\_trackers.txt is present in the same folder as the executable file.

### 1.00 ###
  * First release

---

![https://8d7d8faefd7e24e8562f235c97f55f248c7636d1.googledrive.com/host/0B4lG_fvmX5NZRkMtcjgzT2k5QTg/trackereditor.png](https://8d7d8faefd7e24e8562f235c97f55f248c7636d1.googledrive.com/host/0B4lG_fvmX5NZRkMtcjgzT2k5QTg/trackereditor.png)

This screen shot show the program, after a folder is selected with 97 torrent files inside. The normal procedure is to deselect the trackers that no longer working. Optionally add your one trackers. And press the 'Update torrent'

---

![https://8d7d8faefd7e24e8562f235c97f55f248c7636d1.googledrive.com/host/0B4lG_fvmX5NZRkMtcjgzT2k5QTg/filestrackersinfo.png](https://8d7d8faefd7e24e8562f235c97f55f248c7636d1.googledrive.com/host/0B4lG_fvmX5NZRkMtcjgzT2k5QTg/filestrackersinfo.png)

---

This program is developed using [Lazarus](http://lazarus.freepascal.org/) RAD and [Free Pascal](http://www.freepascal.org/) compiler.
