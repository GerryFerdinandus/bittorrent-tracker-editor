---
title: Troubleshooting
parent: User guide
nav_order: 5
---

# Troubleshooting
{: .no_toc }

1. TOC
{:toc}

## Nothing happens in console mode

Open `console_log.txt` (see [Files and folders](files-and-folders.html) for the place). Line 1 is `OK` or the error. The exit code of the program is `1` for every error.

| Error text | Cause and solution |
|:-----------|:-------------------|
| `ERROR: There are no parameter detected.` | You started `trackereditor_cli` without parameters. Give a torrent file or folder, and `-Ux`. |
| `ERROR: Can not find update parameter -U in the given parameters.` | With two or more parameters one must be `-U0` ... `-U7`. |
| `ERROR: can not decode update parameter -U : ...` | Only `-U0` to `-U7` exist. |
| `ERROR: Unknown parameter: ...` | Only `-SAC` and `-SOURCE "value"` are allowed after the first two parameters. They are case sensitive. |
| `ERROR: There is no value after -SOURCE` | Write `-SOURCE "tag"`. Use `-SOURCE ""` to remove the tag. |
| `ERROR: Tracker URL must begin with udp://, http://, https://, ws:// or wss://` | A line in `add_trackers.txt` is wrong. **No torrent file was changed.** The wrong line is in the log. |
| `ERROR: Tracker URL must end with /announce or /announce.php` | Add `-SAC` (needed for most private trackers), or fix the URL. WebTorrent URLs (`ws://`, `wss://`) never need `/announce`. |
| `ERROR: Can not read remove_trackers.txt` | The file exists but cannot be read. **No torrent file was changed.** |
| `Can not load torrent via folder` / `Can not load torrent file.` / `Error: Can not read torrent.` | The folder has no torrent file, or a file is not a valid torrent. Nothing was changed. |
| `ERROR: No torrent file selected.` | The path is not a `.torrent` file or a folder. |
| `ERROR: Some torrent files are READ-ONLY and were not updated.` | Remove the read-only attribute of those files. |
| `ERROR: Some torrent files failed to write and were not updated.` | Disk full, no permission, or the file is in use by another program. |

## The torrent files were not changed, but there is no error

- With `-U5` and `-U6` the file `remove_trackers.txt` is ignored. These modes remove nothing.
- With a **missing** `remove_trackers.txt` nothing is removed. Make it an empty file to remove all trackers.
- Look at the [tracker order mode](tracker-order-modes.html) you selected: with `-U0` to `-U3` the trackers of the other torrent files are appended at the end.

## My torrent client does not see the new trackers

Most clients read the torrent file once and keep the trackers in their own database. Remove the torrent from the client and add the file again, or ask the client to re-read the trackers.
*rtorrent* needs the `session` folder cleared and a restart.

If you changed the **public/private flag** or the **source tag**, the info hash changed: the client treats the file as a different torrent.

## The download from newTrackon or ngosang fails

The menu items *Online Check* and *ngosang/trackerslist* use HTTPS.

- **Windows:** the files `libssl-3-x64.dll` and `libcrypto-3-x64.dll` (OpenSSL 3) must be in the same folder as `trackereditor.exe`. They are in the zip.
- **Linux and macOS:** the OpenSSL 3 library must be installed on the system.
- The network or firewall blocks the connection, or the server is down. The program waits at most 10 seconds to connect and 15 seconds for the data.

To test the HTTPS connection, start the program with the one parameter `-TEST_SSL`: it does not start the normal program, it only tests the connection and exits with exit code `0` when a HTTPS connection to GitHub works and `1` when it does not. (This is only in `trackereditor`, not in `trackereditor_cli`.)

## The files `add_trackers.txt` or `remove_trackers.txt` are ignored

The program looks in a folder that depends on the installation (Snap, Flatpak, AppImage, macOS). See [Files and folders](files-and-folders.html#where-must-the-files-be).
For an AppImage it is the folder you started it from.

## A tracker is unchecked and I do not know why

A tracker is unchecked when:

- its URL has no valid start (`udp://`, `http://`, `https://`, `ws://`, `wss://`),
- or it does not end with `/announce` or `/announce.php` and *Skip Announce Check* is off (tab *Private Trackers*),
- or it is listed in `remove_trackers.txt`, or `remove_trackers.txt` is empty.

## Reporting a problem

Use the menu *Help > Reporting Issue* or go to <https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues>. Tell us the program version (it is in the window title), the operating system, and the content of `console_log.txt` for console problems.
