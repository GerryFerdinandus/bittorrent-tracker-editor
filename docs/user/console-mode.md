---
title: Console mode and parameters
parent: User guide
nav_order: 2
---

# Console mode and parameters
{: .no_toc }

The program can run without a window: from a script, a cron job, a Windows scheduled task or a desktop shortcut.
This is the *console* (or *automated*) mode. The GUI and the console mode work on the same torrent files and use the same `add_trackers.txt` and `remove_trackers.txt` files.

1. TOC
{:toc}

{: .tip }
If you do not want to use the console mode, remove the files `add_trackers.txt` and `remove_trackers.txt` from the program folder and use the [GUI](gui.html) instead.

## Two programs, one engine

| Program | Windows | Linux | macOS | Window (GUI) |
|:--------|:--------|:------|:------|:-------------|
| `trackereditor` | `trackereditor.exe` | `trackereditor` | `trackereditor.app` | yes. Starting it with **two or more** parameters runs the console mode and never shows a window. |
| `trackereditor_cli` | `trackereditor_cli.exe` | `trackereditor_cli` | *not available* | no. It is a separate console-only program without any GUI/widgetset dependency. |

Console mode works identically on Windows, Linux and macOS. Only the executable name/path and the shell syntax (curl, cron, `.bat`, etc.) differ per platform.

```text
Windows  : trackereditor.exe "C:\dir\torrent" -U4
Linux    : ./trackereditor ../test_torrent -U4
macOS    : /Applications/trackereditor.app/Contents/MacOS/trackereditor ../test_torrent -U4
```

`trackereditor_cli` runs the same console pipeline, with the same command line syntax, the same `add_trackers.txt`/`remove_trackers.txt` files and the same `-Ux`, `-SAC` and `-SOURCE` parameters.
It is available for Windows and Linux only. There is no macOS build: use the console mode of `trackereditor.app` shown above.

```text
Windows  : trackereditor_cli.exe "C:\dir\torrent" -U4
Linux    : ./trackereditor_cli ../test_torrent -U4
```

{: .note }
Where the `.txt` files must be placed depends on the platform and on the package format, see [Files and folders](files-and-folders.html).

## Command line syntax

```text
trackereditor <file_or_folder> -Ux [-SAC] [-SOURCE "tag"]
```

| Parameter | Meaning |
|:----------|:--------|
| `<file_or_folder>` | Path to one `.torrent` file, or a folder containing `.torrent` files. |
| `-Ux` | **Mandatory.** Selects the tracker list order/merge mode: `-U0` ... `-U7`. See [Tracker order modes](tracker-order-modes.html). It can be the first or the second parameter. |
| `-SAC` | Optional. **S**kip **A**nnounce **C**heck: accept tracker URLs that do not end with `/announce` or `/announce.php` (needed for private trackers). |
| `-SOURCE "tag"` | Optional. Set the private tracker `info:source` tag. `-SOURCE ""` (empty string) removes the source tag. Warning: this changes the torrent info HASH. |

Rules:

- Any other parameter is an **error**, for example `-sac` or `-SORUCE`. The parameters are case sensitive.
- The value after `-SOURCE` is never read as a parameter: `-SOURCE -SAC` is a missing value, not the tag `-SAC`.
- Both `"C:\dir\torrent" -U0 -SAC` and `-U0 "C:\dir\torrent" -SAC` are accepted.

What tracker will be added or removed depends on the content of `add_trackers.txt` and `remove_trackers.txt`.

## The text files

These files are optionally present in the same folder as the `trackereditor` executable. On macOS they are in `~/.config/trackereditor/` (create the folder if needed).
For the Linux AppImage they are in the *Original Working Directory*: the directory the user was in when the AppImage was started, not the directory of the AppImage file. See [Files and folders](files-and-folders.html).

`add_trackers.txt` and `remove_trackers.txt` are used in console mode and in the normal GUI mode. `export_trackers.txt` and `console_log.txt` are only created, never read.

| File | Purpose |
|:-----|:--------|
| `add_trackers.txt` | List of all the trackers that must be **added**. |
| `remove_trackers.txt` | List of all the trackers that must be **removed**. |
| `export_trackers.txt` | Created (overwritten) after every update with the final tracker list of the **last** processed torrent file. |
| `console_log.txt` | Created in console mode. The result of the update. |

### add_trackers.txt

One tracker URL per line. The URL must begin with `udp://`, `http://`, `https://`, `ws://` or `wss://`, and (unless `-SAC` is used) end with `/announce` or `/announce.php`. WebTorrent trackers (`ws://`, `wss://`) never need `/announce`.
Text after the first space or tab on a line is a comment and is ignored.

If one URL is invalid, **no torrent file is changed**. The error is written in `console_log.txt` and the exit code is 1.

### remove_trackers.txt

One tracker URL per line.

- If the file is **empty**, all trackers from the torrent files are **REMOVED**.
- If the file is **not present**, no trackers are removed automatically.
- The lines are not checked as tracker URLs. A line that matches no tracker is skipped.
- `-U5` and `-U6` keep the original tracker list of each torrent intact and remove nothing, so `remove_trackers.txt` is effectively ignored in those two modes.
- If the file exists but cannot be read, no torrent file is changed (error in `console_log.txt`, exit code 1).

### export_trackers.txt

Created (overwritten) after every update with the final tracker list of the last processed torrent file.
Each tracker is followed by an empty line, so the content can be copy/pasted directly into clients like uTorrent as separate tracker groups.

### console_log.txt

Only created in console mode. It shows the success or failure of the torrent update.

| Line | Content |
|:-----|:--------|
| 1 | Status: `OK` or `ERROR: xxxxxxx` where `xxxxxxx` is the error description. |
| 2 | Number of torrent files, for example `1` |
| 3 | Number of trackers, for example `23` |

Line 2 and 3 are only valid if line 1 is `OK`.

The exit code of the program is `0` on success and `1` on any error.

## Three possible scenarios

You can add trackers and/or remove trackers.

**Add new trackers and keep the trackers inside the torrent**

- Add the trackers to `add_trackers.txt`.
- `remove_trackers.txt` MUST NOT be present.

**Add new trackers and remove some or all trackers inside the torrent**

- Add the trackers to `add_trackers.txt`.
- Add the trackers to remove to `remove_trackers.txt`, or make `remove_trackers.txt` an empty file to remove all trackers from the torrents.

**No new trackers and remove some or all trackers inside the torrent**

- Make `add_trackers.txt` an empty file.
- Add the trackers to remove to `remove_trackers.txt`, or make it an empty file to remove all trackers from the torrents. This creates torrent files without any tracker (DHT torrents).

## Linux examples

### Example 1: public trackers, only stable trackers

Update all torrent files with only the latest tested stable trackers.

```sh
curl https://newtrackon.com/api/stable --output add_trackers.txt
echo > remove_trackers.txt
./trackereditor ../test_torrent -U0
```

1. Downloads the latest stable trackers into `add_trackers.txt`.
2. `remove_trackers.txt` is now an empty file: all trackers from the present torrents will be REMOVED.
3. Updates all torrent files inside the `test_torrent` folder.

The `-U0` parameter can be changed to `-U4` (sort by name). This is the preferred setting for the *rtorrent* client: rtorrent announces to all the trackers inside the torrent file, so all trackers in alphabetical order is the nicest view in the rtorrent console.
rtorrent needs the `session` folder cleared and a restart to make this work.
This can be run from a regular cron job to keep the client running with 100% functional trackers.

### Example 2: public trackers, mixed with the present trackers

Mix the latest tested stable trackers with the trackers already inside the torrent files.

```sh
curl https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt --output add_trackers.txt
curl https://raw.githubusercontent.com/ngosang/trackerslist/master/blacklist.txt --output remove_trackers.txt
./trackereditor ../test_torrent -U0
```

1. Downloads the latest stable trackers into `add_trackers.txt`.
2. `remove_trackers.txt` now contains the blacklisted trackers.
3. Updates all torrent files inside the `test_torrent` folder.

Difference between example 1 and 2: example 1 guarantees that all the trackers are working. In example 2 you are responsible for the working of the trackers that are not part of `add_trackers.txt`.

### Example 3: private trackers

Manually put the private tracker URL in `add_trackers.txt`, then:

```sh
echo > remove_trackers.txt
./trackereditor ../test_torrent -U0 -SAC -SOURCE "abcd"
```

1. `remove_trackers.txt` is now an empty file: all trackers from the present torrents will be REMOVED.
2. Updates all torrent files inside the `test_torrent` folder.
   - `-SAC` (Skip Announce Check) is needed to skip the private tracker URL check.
   - `-SOURCE` adds the private tracker source tag `abcd`.

`-SOURCE` is optional. `-SOURCE ""` (an empty string) removes all the source tags.

### Example 4: mixed torrents, never remove anything (cron)

Some folders contain a mix of private and public torrents, each with their own curated tracker list. You only want to **add** a couple of extra public trackers without ever removing anything, even if `remove_trackers.txt` is used elsewhere for other folders.

```sh
echo "udp://tracker.opentrackr.org:1337/announce" > add_trackers.txt
./trackereditor ../test_torrent -U6
```

1. `add_trackers.txt` now contains one extra tracker to add.
2. `-U6` appends it AFTER each torrent's own existing tracker list and removes nothing (`remove_trackers.txt`, if present, is ignored for this run). Every torrent keeps its own original tracker list fully intact.

Add this as a cron job (`crontab -e`), for example run once a day at 04:00:

```text
0 4 * * * cd /path/to/enduser && ./trackereditor ../test_torrent -U6 >> cron.log 2>&1
```

## macOS example (public trackers)

macOS works the same way as Linux. The executable is inside the app bundle.

```sh
curl https://newtrackon.com/api/stable --output ~/.config/trackereditor/add_trackers.txt
echo > ~/.config/trackereditor/remove_trackers.txt
/Applications/trackereditor.app/Contents/MacOS/trackereditor ../test_torrent -U0
```

To run this automatically, add a launchd *user agent* (`~/Library/LaunchAgents/*.plist`) instead of cron, since recent macOS versions restrict cron's access to files and the network by default.

## Windows examples

### Console

Start the program with a parameter to a torrent file or folder:

```bat
trackereditor.exe "C:\dir\torrent\file.torrent" -U4
trackereditor.exe "C:\dir\torrent" -U4
```

What trackers will be added/removed depends on the content of `add_trackers.txt` and `remove_trackers.txt`.

### .bat file

The same idea as Linux example 1:

```bat
curl https://newtrackon.com/api/stable --output add_trackers.txt
echo. > remove_trackers.txt
trackereditor.exe "C:\dir\torrent" -U0
```

This `.bat` file can be scheduled with the Windows Task Scheduler to run automatically, the same way cron is used on Linux and macOS.

### Desktop shortcut for private tracker users

The same idea as Linux example 3, but started from a desktop shortcut (double click) and not from a Windows console via a `.bat` file.
A desktop shortcut can have extra parameters appended:

```text
C:\Users\root\Documents\github\bittorrent-tracker-editor\enduser\trackereditor.exe ..\test_torrent -U0 -SAC -SOURCE abc
```

Make sure that `add_trackers.txt` is filled with the private URL and that `remove_trackers.txt` is an empty file.

## What about one parameter?

The GUI program (`trackereditor`) started with **one** parameter does not run the console mode. It opens the window and loads that torrent file or folder, as if you dropped it on the program. This is what happens when you drag a torrent file or folder onto the program icon or a shortcut.
See [GUI guide](gui.html#start-the-program-with-a-file-or-folder).

`trackereditor_cli` with one parameter runs the console pipeline with the default order mode `-U4` (sort by name).

## The tracker order modes

The `-Ux` parameter selects how the NEW trackers (from `add_trackers.txt`) and the ORIGINAL trackers (already inside the torrent) are merged. See [Tracker order modes](tracker-order-modes.html) for the full explanation with pictures.

| Parameter | Short description |
|:----------|:------------------|
| `-U0` | NEW before ORIGINAL. NEW kept intact. Duplicates removed from ORIGINAL. |
| `-U1` | NEW before ORIGINAL. ORIGINAL kept intact. Duplicates removed from NEW. |
| `-U2` | NEW after ORIGINAL. NEW kept intact. Duplicates removed from ORIGINAL. |
| `-U3` | NEW after ORIGINAL. ORIGINAL kept intact. Duplicates removed from NEW. |
| `-U4` | Sort all trackers by name. All torrents get the same list. |
| `-U5` | Like `-U1`, but remove nothing. Each torrent keeps its own list. |
| `-U6` | Like `-U3`, but remove nothing. Each torrent keeps its own list. |
| `-U7` | Randomize the trackers. Each torrent gets another random order. |

{: .note }
With `-U0` to `-U3` the trackers found in the **other** torrent files being processed (and not removed with `remove_trackers.txt`) are appended at the end of the list of each torrent. `-U5` and `-U6` never add them.

---

*This page is the web version of the console part of the `readme.txt` file that is inside the download. Both must say the same.*

This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit (<https://www.openssl.org/>).
