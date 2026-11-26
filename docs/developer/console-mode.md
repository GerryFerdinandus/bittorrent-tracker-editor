---
title: Console mode
parent: Programmer guide
nav_order: 4
---

# Console mode: `main_common.pas` and `main_cli.pas`
{: .no_toc }

For the user view of the same feature, see [Console mode and parameters](../user/console-mode.html).

1. TOC
{:toc}

## Two entry points, one engine

```mermaid
flowchart LR
    subgraph GUIexe["trackereditor (GUI)"]
        FC["TFormTrackerModify.FormCreate<br/>ParamCount >= 2"]
    end
    subgraph CLIexe["trackereditor_cli"]
        LPR["trackereditor_cli.lpr"] --> MC["main_cli.RunConsoleApplication"]
    end
    FC --> DF["main_common.DetermineTrackerListFolder(ParamStr 0)"]
    MC --> DF
    DF --> RCM["main_common.RunConsoleMode(folder)"]
    RCM -->|"True"| OK["exit code 0"]
    RCM -->|"False"| ERR["System.ExitCode := 1"]
```

- GUI program: `FormCreate` calls `RunConsoleMode` **first** when `ParamCount >= 2`, sets `ExitCode := 1` on failure, and calls `Application.Terminate`. The form is never built, and `FormShow` hides it if it is shown anyway.
- CLI program: `main_cli.RunConsoleApplication` does the same without any `Forms`/widgetset unit. It does not use `fix_openssl` and has no networking code.

`RunConsoleMode(const FolderForTrackerListLoadAndSave: string): boolean` reads `ParamStr`/`ParamCount` itself and returns `True` on success.

## Where the text files are: `DetermineTrackerListFolder`

```mermaid
flowchart TD
    A["DetermineTrackerListFolder(ExeFileName)"] --> OS{"compile target"}
    OS -- "Linux" --> L1["Result := $SNAP_USER_COMMON"]
    L1 --> L2{"$container = 'flatpak' ?"}
    L2 -- yes --> L3["Result := $XDG_DATA_HOME"]
    L2 -- no --> L4
    L3 --> L4{"$APPIMAGE not empty ?"}
    L4 -- yes --> L5["Result := $OWD<br/>(original working directory)"]
    L4 -- no --> E
    L5 --> E
    OS -- "macOS (Darwin)" --> M["Result := GetAppConfigDir(False)<br/>= ~/.config/trackereditor/<br/>(created when missing)"]
    OS -- "Windows, other" --> E
    M --> E{"Result = '' ?"}
    E -- yes --> X["Result := ExtractFilePath(ExeFileName)"]
    E -- no --> P
    X --> P["AppendPathDelim(Result)"]
```

The Linux rules are in a separate pure function, `PackagedTrackerListFolder(SnapUserCommon, Container, XdgDataHome, AppImage, OriginalWorkDir)`, so that they can be unit tested without setting environment variables. **The last rule that applies wins**: Snap, then Flatpak, then AppImage.

An AppImage must use `$OWD` because the AppImage is mounted read-only in a temporary location: the folder of the executable is useless.

## `RunConsoleMode` step by step

```mermaid
flowchart TD
    S(["RunConsoleMode(Folder)"]) --> C0["CreateTrackerList, TDecodeTorrent,<br/>AddedTrackersRawList"]
    C0 --> C1["LoadAddTrackersRaw<br/>add_trackers.txt, or RECOMMENDED_TRACKERS<br/>when the file is missing or unreadable"]
    C1 --> C2["ValidateAndSanitizeTrackers(... Temporary_SkipAnnounceCheck = True)<br/>only the URL scheme is checked now: -SAC is not known yet"]
    C2 --> V1{"valid ?"}
    V1 -- no --> C2e["log the error, clear the raw list<br/>AddTrackersFileIsValid = False"]
    V1 -- yes --> C3
    C2e --> C3["LoadRemoveTrackers<br/>remove_trackers.txt"]
    C3 --> V2{"readable ?"}
    V2 -- no --> C3e["log 'ERROR: Can not read remove_trackers.txt'"]
    V2 -- yes --> C4
    C3e --> C4["Open console_log.txt (overwrite)"]
    C4 --> G{"AddTrackersFileIsValid<br/>and RemoveTrackersFileIsReadable<br/>and ConsoleModeDecodeParameter ?"}
    G -- no --> LOG
    G -- yes --> F{"PathIsTorrentFolder ?"}
    F -- yes --> D1["ConsoleDecodeTorrentFolder"]
    F -- no --> F2{"PathIsTorrentFile ?"}
    F2 -- yes --> D2["ConsoleDecodeSingleTorrentFile"]
    F2 -- no --> E1["log 'ERROR: No torrent file selected.'"]
    D1 --> DD{"decoded ?"}
    D2 --> DD
    DD -- no --> E2["log 'Can not load ...'<br/>(or 'Error: Can not read torrent.')"]
    DD -- yes --> AB["ApplyBanListRemoval"]
    AB --> AB2{"ok ?"}
    AB2 -- yes --> RUP["RunUpdateTorrentPipeline"]
    AB2 -- no --> LOG
    RUP --> LOG
    E1 --> LOG
    E2 --> LOG
    LOG["exit code 1 when LogStringList[0] is not 'OK'<br/>write LogStringList to console_log.txt<br/>free everything"] --> R(["Result = not MustExitWithErrorCode"])
```

Notes:

- **Nothing is written to a torrent file** when `add_trackers.txt` is invalid, `remove_trackers.txt` is unreadable, a parameter is wrong, or any torrent file in the selection cannot be decoded. The first decode failure clears `TorrentFileNameList` and the tracker lists (`ConsoleDecodeTorrentFiles`).
- Any exception is caught: the exit code is set to 1 and `'ERROR: ' + E.Message` is inserted as **line 1** of the log, so an earlier `OK` cannot make it look like a success. If the log file cannot be written, the exit code is also 1.
- The log file is opened (and so truncated) **before** the arguments are decoded, so there is always a fresh log, also for a parameter error.
- A tracker that is in both `add_trackers.txt` and `remove_trackers.txt` is **not** added: `ApplyBanListRemoval` removes the banned trackers from the NEW list.

### `ApplyBanListRemoval`

The console equivalent of the GUI's `UpdateViewRemoveTracker`, but without a grid. It exits at once for `-U5` and `-U6`.

1. If `remove_trackers.txt` is present but empty: `TrackerManuallyDeselectedByUserList := TrackerFromInsideTorrentFilesList` (every tracker inside the torrents is "unchecked").
2. Validate the NEW list (now with the final `-SAC` state).
3. `RemoveTrackersFromList(TrackerBanByUserList, AddedTrackersRawList)`.
4. Validate and sanitize again.

### `RunUpdateTorrentPipeline`

1. No torrent file: log `ERROR: No torrent file selected`.
2. Validate the NEW list again (needed because `ApplyBanListRemoval` skips this for `-U5`/`-U6`).
3. `CombineFiveTrackerListToOne(tloSort, ...)` for the initial count.
4. `UpdateTorrentFileList(...)`. See [Tracker pipeline](tracker-pipeline.html).
5. `SaveTrackerFinalListToFile` writes `export_trackers.txt`.
6. Partial failures are logged: read-only files, files that failed to write, files that could not be decoded.
7. If the log is still empty: `OK`, the number of files, the number of trackers.

## Decoding the arguments

`ConsoleModeDecodeParameter` copies `ParamStr(1..ParamCount)` into a list and calls the testable `ConsoleModeDecodeArguments(Arguments, out FileNameOrDirStr, var TrackerList)` (`Arguments[0]` is the first parameter, so without the program name).

```mermaid
flowchart TD
    A["Arguments.Count"] --> N0{"0"}
    A --> N1{"1"}
    A --> N2{">= 2"}
    N0 --> E0["log 'ERROR: There are no parameter detected.'<br/>return False"]
    N1 --> R1["path := Arguments[0]<br/>order := tloSort (-U4)<br/>return True"]
    N2 --> U0{"Arguments[0] starts with '-U' ?"}
    U0 -- yes --> D0["DecodeConsoleUpdateParameter(Arguments[0])<br/>path := Arguments[1]"]
    U0 -- no --> U1{"Arguments[1] starts with '-U' ?"}
    U1 -- yes --> D1["DecodeConsoleUpdateParameter(Arguments[1])<br/>path := Arguments[0]"]
    U1 -- no --> E1["log 'ERROR: Can not find update parameter -U ...'<br/>return False"]
    D0 --> OPT
    D1 --> OPT
    OPT["i := 2, then loop over the rest"] --> T{"Arguments[i]"}
    T -- "-SAC" --> S1["SkipAnnounceCheck := True"]
    T -- "-SOURCE" --> S2{"value present and<br/>not -SAC / -SOURCE ?"}
    S2 -- yes --> S3["SourceTag := value<br/>RemoveAllSourceTag := (value = '')<br/>skip the value"]
    S2 -- no --> E2["log 'ERROR: There is no value after -SOURCE'<br/>return False"]
    T -- "anything else" --> E3["log 'ERROR: Unknown parameter: ...'<br/>return False"]
```

`DecodeConsoleUpdateParameter` accepts exactly three characters, `-U` plus one digit within `Ord(low(TTrackerListOrder))..Ord(high(TTrackerListOrder))`. So `-U8` and `-U10` fail with `ERROR: can not decode update parameter -U : ...`.

The same function is used by the GUI for the **one parameter** start (`DragAndDropStartupMode`), where it only returns the path.

{: .note }
The single parameter case defaults to `tloSort` "to keep the behaviour of the previous software version" (backward compatibility). `readme.txt` calls `-Ux` mandatory because that is the documented usage.

## Text files used by the console mode

| Constant (`torrent_miscellaneous.pas`) | File | Reader / writer |
|:----------------------------------------|:-----|:----------------|
| `FILE_NAME_ADD_TRACKERS` | `add_trackers.txt` | `ReadAddTrackersFile`, `LoadAddTrackersRaw` |
| `FILE_NAME_REMOVE_TRACKERS` | `remove_trackers.txt` | `LoadRemoveTrackers` |
| `FILE_NAME_EXPORT_TRACKERS` | `export_trackers.txt` | `SaveTrackerFinalListToFile` / `TrySaveTrackerFinalListToFile` |
| `FILE_NAME_CONSOLE_LOG` | `console_log.txt` | `RunConsoleMode` |
| `CONSOLE_SUCCESS_STATUS` | `OK` | first line of the log |

Design decisions that are tested (see `TTestStartUpParameterCli`):

- `remove_trackers.txt` lines are **not** validated as tracker URLs (unlike `add_trackers.txt`): a user may want to ban a malformed or obsolete URL that is still inside a torrent. A line that matches no tracker is skipped.
- An empty, existing `remove_trackers.txt` removes every tracker. A missing one removes nothing. An unreadable one aborts the run (a half read file must not remove anything).
- Console mode changes no torrent file when `add_trackers.txt` has an invalid URL.

## GUI and console: what is shared

| Shared function | Where | Used by the GUI in |
|:----------------|:------|:-------------------|
| `CreateTrackerList` / `FreeTrackerList` | `main_common` | `FormCreate` / `FormDestroy` |
| `DetermineTrackerListFolder` | `main_common` | `FormCreate` |
| `LoadAddTrackersRaw`, `ReadAddTrackersFile` | `main_common` | `LoadTrackersTextFileAddTrackers`, `MenuFileOpenTrackerListClick` |
| `LoadRemoveTrackers` | `main_common` | `FormCreate` |
| `TrySaveTrackerFinalListToFile` | `main_common` | `UpdateTorrent` |
| `ValidateNewTrackerLines` | `torrent_miscellaneous` | `CopyUserInputNewTrackersToList` |
| `AddTorrentFileTrackers` | `torrent_miscellaneous` | `UpdateTorrentTrackerList` |
| `ConsoleModeDecodeParameter` | `torrent_miscellaneous` | `DragAndDropStartupMode` |
| `CombineFiveTrackerListToOne`, `UpdateTorrentFileList` | `torrent_miscellaneous`, `update_torrent` | `UpdateTorrent` |

What is **not** shared (because the GUI has a grid and dialogs): decoding and showing the torrents (`AddTorrentFileList`, `ViewUpdate*`), `UpdateViewRemoveTracker` (unchecks rows, console equivalent is `ApplyBanListRemoval`), the confirmation dialogs.

## Tests

`source/test/test_start_up_parameter.pas`:

- `TTestStartUpParameter` starts the real GUI executable with parameters (console mode needs no display).
- `TTestStartUpParameterCli` descends from it and starts `trackereditor_cli`. It adds the CLI specific tests.
- The tests run in their own temporary folder with a copy of the program and of `test_torrent/*.torrent`. Parameters are quoted (`QuoteParameter`) so paths with spaces work.
- Tests that download from the internet are skipped (`Ignore`) when the server cannot be reached.
