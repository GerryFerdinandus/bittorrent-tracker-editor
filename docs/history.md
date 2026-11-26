---
title: Software history
nav_order: 4
---

# Software history

Newest version first. `Compiler Lazarus` is the Lazarus version that was used to build that release (not recorded for every release).

When you make a new release, add a section at the top of this page. See [Maintaining this documentation](developer/docs-maintenance.html).

## 1.33.1

- FIX: Cannot open torrent file V2 format. ([Issue 51](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/51))

Compiler Lazarus: v3.6

## 1.33.0

- ADD: Support for OpenSSL 3
- FIX: Handle dark theme on MacOS. ([Issue 49](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/49))
- ADD: Direct download support for ngosang via menu. ([Issue 35](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/35))
- ADD: Extra tabpage 'private torrent'. For issue 31 and 34
- ADD: Check box 'Skip Announce Check in the URL' ([Issue 31](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/31))
- ADD: Command parameter '-SAC' -> 'Skip Announce Check' in the URL ([Issue 31](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/31))
- ADD: Support 'Info Source' tag for private trackers ([Issue 34](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/34))
- ADD: Command parameter '-SOURCE' -> info Source tag for private trackers. See [Console mode](user/console-mode.html) ([Issue 34](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/34))
- FIX: support for '/announce.php' ([Issue 27](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/27))
- FIX: There was an issue with uploading tracker list to newTrackon.
- FIX: WebTorrent do not have '/announce' ([Issue 24](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/24))
- ADD: Wrong tracker URL format from torrent files should be unselected by default ([Issue 22](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/22))
- ADD: Upload trackers to newTrackon ([Issue 23](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/23))
- ADD: Verify the working status of public trackers. (Data from newTrackon) ([Issue 21](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/21))

Compiler Lazarus: v2.26

## 1.32.0

- ADD: Add more options for updating the torrent tracker list. ([Issue 8](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/8))
- ADD: Add trackers but keep the original unique trackers list intact. ([Issue 12](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/12))
- ADD: Randomize tracker order for each torrent file. ([Issue 19](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/19))
- FIX: Trackers with https ([Issue 9](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/9))
- FIX: Read torrent file with string size larger that 1MB ([Issue 10](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/10))
- FIX: Give user a warning when torrent file is read only file. ([Issue 14](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/14))
- FIX: Give user a warning when torrent file update failed. ([Issue 15](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/15))
- FIX: Can not be opened by dragging a file. ([Issue 17](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/17))
- FIX: Need sanitize URL tracker. ([Issue 18](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/18))
- FIX: WebSocket ws:// and wss:// should be accepted as input. ([Issue 20](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/20))

Compiler Lazarus: v1.62

## 1.31

- ADD: Edit comment in data/info grid column.
- FIX: The data/info column can be moved but it is not updated correctly when torrent is reloaded. ([Issue 6](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/6))

## 1.30

- ADD: Tab page Files/trackers/info
- ADD: Optional start as console program. (See [Console mode](user/console-mode.html))
- ADD: remove_trackers.txt will remove specific trackers from torrent.
- ADD: export_trackers.txt is created after updating the torrent.
- ADD: drag and drop of trackers file (with '.txt' file extension)
- FIX: Can not remove duplicate Tracker automatically ([Issue 4](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/4))
- FIX: Can't open some of .torrent files. ([Issue 5](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/5))

Compiler Lazarus: v1.24

## 1.21

- FIX: Support for Unicode in filename. (Chinese etc.)

Compiler Lazarus: v1.0.12

## 1.20

- ADD: Tab page torrent info/data.
- ADD: Drag & Drop torrent files or a folder with torrent files inside.

Compiler Lazarus: v1.0.10

## 1.10 (2013-05-10)

- ADD: Tab page for public/private flag. ([Issue 1](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues/1))
- ADD: Load tracker list from file via menu or at start-up, when file add_trackers.txt is present in the same folder as the executable file.

Compiler Lazarus: v1.0.8

## 1.00 (2012-03-03)

- First release

Compiler Lazarus: v0.9.30
