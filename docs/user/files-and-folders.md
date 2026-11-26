---
title: Files and folders
parent: User guide
nav_order: 4
---

# Files and folders
{: .no_toc }

The program uses four optional text files. Two are read (`add_trackers.txt`, `remove_trackers.txt`), two are written (`export_trackers.txt`, `console_log.txt`).
This page shows what they are and **where the program looks for them**.

1. TOC
{:toc}

## The four files

| File | Read or written | Used in | Purpose |
|:-----|:----------------|:--------|:--------|
| `add_trackers.txt` | read | GUI and console | The trackers to add. In the GUI they are shown in the *Add new trackers* box at the start. |
| `remove_trackers.txt` | read | GUI and console | The trackers to remove. An **empty** file means: remove **all** trackers. A missing file means: remove nothing. |
| `export_trackers.txt` | written | GUI and console | After every update: the final tracker list of the **last** processed torrent, with an empty line after each tracker. |
| `console_log.txt` | written | console only | The result of the update. Line 1 is `OK` or `ERROR: ...`. See [Console mode](console-mode.html#console_logtxt). |

The files in the download (`add_trackers.txt` and `remove_trackers.txt`) are *examples*. The format is one tracker URL per line. Text after the first space or tab on a line is ignored, so you can write a comment behind a URL.

## Where must the files be?

The folder depends on how you installed the program.

| Installation | Folder |
|:-------------|:-------|
| Windows (zip) | The same folder as `trackereditor.exe` / `trackereditor_cli.exe`. |
| Linux (zip, from your own build) | The same folder as the `trackereditor` / `trackereditor_cli` executable. |
| macOS | `~/.config/trackereditor/` (the program creates this folder when it is missing). |
| Linux **Snap** | `$SNAP_USER_COMMON`, normally `~/snap/bittorrent-tracker-editor/common/`. |
| Linux **Flatpak** | The `$XDG_DATA_HOME` folder of the Flatpak application. |
| Linux **AppImage** | The *Original Working Directory* (`$OWD`): the folder you were in when you started the AppImage. **Not** the folder where the AppImage file is. |

If the packaging folder is not set, the folder of the executable is used.

{: .note }
The folder of the executable must be writable, or the program cannot write `export_trackers.txt` (and `console_log.txt`). In the GUI you get a warning after the update, in console mode the update is reported as an error.

## Overriding the files in the GUI

- *File > Open tracker file list* loads another `.txt` file in the *Add new trackers* box.
- Drag and drop of a `.txt` file on the window appends it to the *Add new trackers* box.

See the [GUI guide](gui.html#startup-files).

## Example files

`add_trackers.txt`:

```text
udp://tracker.opentrackr.org:1337/announce
udp://open.stealth.si:80/announce  this is a comment
wss://tracker.openwebtorrent.com
```

`remove_trackers.txt` (remove one dead tracker and one malformed URL):

```text
udp://tracker.coppersurfer.tk:6969/announce
http://old-tracker.example/announce.php
```

An **empty** `remove_trackers.txt` (create it with `echo > remove_trackers.txt`) removes all trackers from all torrent files.
