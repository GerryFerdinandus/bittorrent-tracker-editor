**bittorrent-tracker-editor** will add/remove bittorrent tracker from the torrent file(s).
This software works on Windows XP SP3, Windows 7+, macOS and Linux.

---

## Code Status: ##
[![Build Status](https://travis-ci.org/GerryFerdinandus/bittorrent-tracker-editor.svg?branch=master)](https://travis-ci.org/GerryFerdinandus/bittorrent-tracker-editor)
for Windows, macOS and Linux build.

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
  * [From GitHub: Executable file for Windows, macOS and Linux.](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
  * [From GitHub: Trackers list (from GitHub user: ngosang )](https://github.com/ngosang/trackerslist)

---

## Software version ##

### 1.32.0.RC.4 ###
  * ADD:  Add more options for updating the torrent tracker list. ([Issue 8](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/8))
  * ADD:  Add trackers but keep the original unique trackers list instact. ([Issue 12](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/12))
  * FIX:  Trackers with https ([Issue 9](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/9))
  * FIX:  Read torrent file with string size langer that 1MB ([Issue 10](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/10))
  * FIX:  Give user a warning when torrent file is read only file. ([Issue 14](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/14))
  * FIX:  Give user a warning when torrent file update failed. ([Issue 15](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/15))

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

![](pictures/trackereditor.png?raw=true "Trackers List")

This screen shot show the program, after a folder is selected with 97 torrent files inside. The normal procedure is to deselect the trackers that no longer working. Optionally add your own trackers. And press the 'Update torrent'

---

![](pictures/filestrackersinfo.png?raw=true "Files/Trackers/Info")

---

## License: ##
**bittorrent-tracker-editor** is released under [MIT License](http://www.opensource.org/licenses/MIT)

---

This program is developed using [Lazarus](http://lazarus.freepascal.org/) RAD and [Free Pascal](http://www.freepascal.org/) compiler.
