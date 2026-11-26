---
title: Home
layout: home
nav_order: 1
---

# BitTorrent Tracker Editor

**bittorrent-tracker-editor** adds, removes and reorders the trackers of one or many torrent files at once.
It works on Windows 7+, macOS and Linux, and supports BitTorrent V1, V2 and hybrid torrent files.

[![GitHub Latest release](https://img.shields.io/github/release/GerryFerdinandus/bittorrent-tracker-editor/all.svg)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
[![GitHub commits since latest release](https://img.shields.io/github/commits-since/gerryferdinandus/bittorrent-tracker-editor/latest)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/commits/main)

---

## Where to start

| You want to... | Go to |
|:---------------|:------|
| Use the program with the windows (GUI) | [GUI guide](user/gui.html) |
| Run it from a script, cron job or scheduled task | [Console mode and parameters](user/console-mode.html) |
| Understand the `-U0` to `-U7` tracker order modes | [Tracker order modes](user/tracker-order-modes.html) |
| See what changed in each version | [Software history](history.html) |
| Learn how the source code works | [Programmer guide](developer/) |

---

## Warning

There is **no backup function** in this software. Use it at your own risk.
Bittorrent works fine without this program. You probably don't need this software.

---

## Which program to use for add/remove bittorrent trackers?

- **Edit one torrent file:** you can use <http://torrenteditor.com/>
- **Edit multiple torrent files:** use this program. It is made for changing multiple torrent files.

---

## Features

- Select one torrent file or a folder with torrent files.
- Add one or more trackers at the same time.
- Remove one or more trackers at the same time.
- Remove all the trackers to create a trackerless torrent (DHT torrent).
- Change the public/private flag. Warning: this will change the torrent info HASH.
- Preset the trackers to add or remove with `add_trackers.txt` and `remove_trackers.txt`, see [Files and folders](user/files-and-folders.html).
- Optional start as a console program, also as a separate GUI-free program `trackereditor_cli`. See [Console mode](user/console-mode.html).
- Show the content of the torrent files.
- Check the status of trackers and download stable trackers from newTrackon or ngosang.

---

## Downloads

- [Executable files for Windows, macOS and Linux (GitHub releases)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
- [Snap Store](https://snapcraft.io/bittorrent-tracker-editor) (Linux amd64 and arm64)
- [Flathub](https://flathub.org/apps/io.github.gerryferdinandus.bittorrent-tracker-editor) (Linux amd64 and arm64)
- [Trackers list from ngosang](https://github.com/ngosang/trackerslist)
- [Trackers list from newTrackon](https://newtrackon.com)

---

## Build from source

Install **GNU Make** and **Lazarus with Free Pascal**, then:

```sh
git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git
cd bittorrent-tracker-editor
make help
make all
make test
```

See [Build and test](developer/build-and-test.html) for all the details.

---

## Screenshots

![Trackers List](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases/download/V1.32.0/trackereditor_list_windows.png "Trackers List")

The program after a folder with torrent files is selected. The normal procedure is to deselect the trackers that are no longer working,
optionally add your own trackers, and select the *Update torrent* menu.

![Files/Trackers/Info](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases/download/V1.32.0/trackereditor_info_windows.png "Files/Trackers/Info")

---

## License

**bittorrent-tracker-editor** is released under the [MIT License](https://opensource.org/licenses/MIT).

This program is developed using the [Lazarus](https://www.lazarus-ide.org/) RAD and the [Free Pascal](https://www.freepascal.org/) compiler.

This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit (<https://www.openssl.org/>).
