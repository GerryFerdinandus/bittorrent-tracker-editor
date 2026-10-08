# Project Guidelines

## Overview

BitTorrent Tracker Editor — a Free Pascal/Lazarus desktop app for batch-editing tracker lists in torrent files. Cross-platform (Windows/macOS/Linux), supports BitTorrent V1, V2, and Hybrid formats. MIT licensed.

## Code Style

- All source files start with `// SPDX-License-Identifier: MIT`
- All units use `{$mode objfpc}{$H+}` (ObjFPC mode, long strings)
- All strings are UTF-8 (`UTF8String` is equivalent to `string` under `{$H+}`)
- Classes: `T` prefix (`TDecodeTorrent`, `TFormTrackerModify`)
- Enums: `T` prefix, values use module-specific prefix (`tv_V1`, `tloSort`, `ntl_URL_All` for newtrackon, `ngl_URL_All` for ngosang)
- Fields: `F` prefix (`FTrackerList`, `FMemoryStream`)
- Constants: `UPPER_SNAKE_CASE` with domain prefix (`BK_ANNOUNCE`, `BK_INFO`)
- Functions/procedures: `CamelCase` (`SanitizeTrackerList`, `GetAnnounceList`)
- Use named constants instead of magic strings — see `BK_*` constants in `decodetorrent.pas`

## Architecture

```
source/code/
├── bencode.pas                       # Bencode codec (low-level format layer)
├── decodetorrent.pas                 # Torrent file model (parse, modify, save)
├── torrent_miscellaneous.pas         # Tracker list utilities, ordering modes, URL validation
├── newtrackon.pas                    # API client: newtrackon.com
├── ngosang_trackerslist.pas          # API client: ngosang/trackerslist GitHub
├── trackerlist_online.pas            # Tracker status classification
├── controller_trackerlist_online.pas  # Controller: online tracker grid
├── controller_treeview_torrent_data.pas # Controller: torrent contents tree
├── controllergridtorrentdata.pas      # Controller: torrent metadata grid
├── update_torrent.pas                # Applies tracker add/remove/order changes to a torrent
├── fix_openssl.pas                   # FPC 3.2.2 OpenSSL 3 detection workaround (init-only unit)
├── main_common.pas                   # Headless console engine shared by trackereditor and trackereditor_cli
├── main_cli.pas                      # Entry point for trackereditor_cli (headless, no Forms/widgetset)
├── main.pas + main.lfm              # Main form (GUI + event handlers)
```

MVC pattern: controllers in `controller_*.pas` mediate between data models and GUI grids/trees. `main.pas` orchestrates everything. `trackereditor_cli` (project at `source/project/tracker_editor/trackereditor_cli.lpi`) is a separate console-only executable built from `main_cli.pas` + `main_common.pas`, sharing the same pipeline as the GUI's console mode.

Console mode of the GUI program (two or more parameters) is the first thing `FormCreate` does: it calls `main_common.RunConsoleMode` and terminates, the form is never built. The code that the GUI and the console share lives in `main_common.pas` (`CreateTrackerList`/`FreeTrackerList`, `DetermineTrackerListFolder`/`PackagedTrackerListFolder`, `LoadAddTrackersRaw`, `ReadAddTrackersFile`, `LoadRemoveTrackers`, `SaveTrackerFinalListToFile`/`TrySaveTrackerFinalListToFile`, `RECOMMENDED_TRACKERS`) and in `torrent_miscellaneous.pas` (`ValidateNewTrackerLines`, `InvalidTrackerURLMessage`, `ConsoleModeDecodeArguments`, `AddTorrentFileTrackers`, `GetTrackersForOnlineSubmit`, `PathIsTorrentFile`/`PathIsTrackerListFile`, `TrackerDependsOnSkipAnnounceCheck`). Do not copy this code into `main.pas`. Logic of a GUI event handler that can be a pure function goes into one of these units, because `main.pas` can not be unit tested.

Dependency flow: `main` → `decodetorrent` → `bencode`; `main` → `torrent_miscellaneous`; `main` → `main_common`; API clients → `torrent_miscellaneous`; `main_cli` → `main_common` → `decodetorrent`/`torrent_miscellaneous`/`update_torrent`.

## Build and Test

```bash
# Clone with submodule (DCPcrypt2 for SHA256)
git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git

# Build (Release)
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi

# Build trackereditor_cli (Release) — console-only, no widgetset needed
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi

# Build unit tests (Debug)
lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi

# Run tests
enduser/test_trackereditor -a --format=plain
```

Build modes: `Debug` (assertions, range checks, DWARF symbols) and `Release` (optimized, stripped).
Linux widget sets: `--widgetset=gtk2`, `--widgetset=qt5`, `--widgetset=qt6`.
macOS: `--widgetset=cocoa`.
Output goes to `enduser/` (executables) and `lib/` (compiled units).

### Testing the Linux code path via WSL

**Mandatory: after adding or changing any test (or code under test), always run the tests on Windows AND in WSL (Linux). Never skip the WSL run, even when the change looks platform independent.** Linux CI has failed on things Windows hides (e.g. `CopyFile` not keeping the execute bit, case sensitive file names, `{$IFDEF UNIX}` code).

On a Windows dev machine, `wsl.exe` can run `lazbuild`/`fpc` inside a WSL distro (e.g. Ubuntu) against the same workspace, mounted at `/mnt/<drive letter>/...`. This is the only way to actually compile and run `{$IFDEF UNIX}`/`{$IFDEF LINUX}` code paths (e.g. `BaseUnix.FpChmod`) without a Linux machine or CI.

Step 1, Windows (build `trackereditor_cli` too: the console tests start the real executables from `enduser/`):

```powershell
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi
lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi
enduser\test_trackereditor.exe -a --format=plain
```

Step 2, WSL (Ubuntu is the default WSL distro here; it has `lazarus-4.4`). For the GUI test build use qt6 (recommended over gtk2, one widgetset is enough, no need to test both). It needs `libqt6pas-dev`: `sudo apt install libqt6pas-dev`; without it the link fails with `cannot find -lQt6Pas`. The gtk2 build (`--widgetset=gtk2`, needs `libgtk2.0-dev`) also works:

```powershell
wsl.exe -e bash -lc "cd /mnt/f/github_project/lazarus/bittorrent-tracker-editor && \
  lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --widgetset=qt6 --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi && \
  lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi && \
  lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi"

wsl.exe -e bash -lc "cd /mnt/f/github_project/lazarus/bittorrent-tracker-editor/enduser && ./test_trackereditor -a --format=plain --suite=TTestDecodeTorrent"
```

Run every suite in WSL, with its literal name in `--suite=` (or the whole binary without `--suite`): `TTestBEncode`, `TTestDecodeTorrent`, `TTestUpdateTorrent`, `TTestTorrentMiscellaneous`, `TTestTrackerListOnline`, `TTestNewTrackon`, `TTestNgosangTrackersList`, `TTestStartUpParameterCli`, plus the new test by name (`--suite=TTestClass.Test_Name`). `TTestStartUpParameter` starts the Linux GUI executable `enduser/trackereditor`, so build it first (qt6 command above). It only runs the console mode, so no X server/`DISPLAY` is needed (verified with gtk2 and with qt6: all 15 tests pass in WSL without `DISPLAY`). If the GUI build is not possible, say so explicitly in the report, do not silently skip it.

Notes:
- `--lazarusdir=...` may be required if the WSL distro's `~/.lazarus/environmentoptions.xml` points at a stale/missing Lazarus version; check the installed version under `/usr/lib/lazarus/`.
- In PowerShell, do not put `$variable` inside the double-quoted `bash -lc "..."` string: PowerShell expands it before bash sees it (a `for s in ...; do ... $s` loop silently runs with an empty value). Write the suite names literally.
- The Linux build produces ELF binaries `enduser/test_trackereditor`, `enduser/trackereditor`, `enduser/trackereditor_cli` (no extension) alongside the Windows `.exe` files, plus `lib/*/x86_64-linux/` compiled units. Delete these after testing so they don't linger in the Windows-facing `enduser/`/`lib/` folders, then rebuild the Windows `trackereditor_cli`/test binaries if you need them again.
- The integration tests (`TTestStartUpParameter*`) run in their own temp folder, with a copy of the program and a copy of `test_torrent/*.torrent`. They do not change the files of the project (macOS: the txt files are in `~/.config/trackereditor/`). Only `-TEST_SSL` uses the program in `enduser/`, because the copy has no DLL files. The program is started with the work folder as current directory, because an AppImage reads and writes the txt files in `$OWD` (the working directory at launch), not next to itself; CI tests the AppImage as `enduser/trackereditor`. The paths in their command lines are quoted (`QuoteParameter`), so a path with spaces works. The V2 and hybrid test torrents have no trackers: a test that needs trackers inside the torrent must fill them first.
- `TTestNewTrackon`, `TTestNgosangTrackersList` and the `TTestStartUpParameter*` mode tests download from the internet and are skipped (`Ignore`) when the server is unreachable. The API clients have a `BaseURL` property: set it to `http://999.999.999.999/` to test a failing download offline, it fails at once.

## Conventions

- Test framework: FPCUnit. Test files live in `source/test/`, test project in `source/project/unit_test/`.
- Submodule `submodule/dcpcrypt/` provides SHA256 — never modify it directly.
- `enduser/add_trackers.txt` and `enduser/remove_trackers.txt` are user-facing preset files.
- Console mode supports `-U0` through `-U7` for 8 tracker ordering modes, plus `-SAC` and `-SOURCE`. Any other parameter is an error, and the value after `-SOURCE` is never read as a parameter.
- Console mode changes no torrent file when `add_trackers.txt` has an invalid URL or `remove_trackers.txt` can not be read: the error is in `console_log.txt` and the exit code is 1.
- `remove_trackers.txt`: the lines are by design not validated as tracker URLs (unlike `add_trackers.txt`). A line that matches no tracker is skipped. An empty file (present) means: remove every tracker inside the torrents.
- Trackers of private torrents are in `TTrackerList.TrackerFromPrivateTorrentsList` and are never sent to newTrackon (the URL can have a passkey). Use `GetTrackersForOnlineSubmit`, also when the same URL is in a public torrent.
- `TDecodeTorrent.SaveTorrent` writes a temp file and renames it. It keeps the owner and permissions, and on Unix it follows a symlink, so the link stays a symlink.
- `ValidTrackerURL` accepts only a lower case scheme (`udp://`, not `UDP://`). This is the current behaviour, not a decision: RFC 3986 allows any case. Change it only on purpose, together with `WebTorrentTrackerURL` and its test.
- The files in `test_torrent/` are test fixtures: no test or program run may change them. Check `git status test_torrent` after a test run.
- Design choice, known minor issue: tracker lists are `TStringList`s with the default `CaseSensitive = False`, so `IndexOf`/`dupIgnore` treat URLs that differ only in case (e.g. a passkey in the path or query) as the same tracker, although only the scheme and host are case-insensitive (RFC 3986). Left as is for now: it is rare and the console version never shows the duplicate end result. Do not report it again as a bug.
- OpenSSL 3 DLLs (`libssl-3-x64.dll`, `libcrypto-3-x64.dll`) are required at runtime on Windows for HTTPS.
- The `{$IFDEF VER3_2_2}` blocks in `fix_openssl.pas` work around FPC 3.2.2 missing OpenSSL 3 detection — keep until FPC updates.
- On Linux, the folder used to load/save `add_trackers.txt`/`remove_trackers.txt`/etc. depends on the packaging format (see `FormCreate` in `main.pas`): Snap → `$SNAP_USER_COMMON`, Flatpak → `$XDG_DATA_HOME`, AppImage → `$OWD` (original working directory, not the AppImage's own path), otherwise same dir as the executable. On macOS it is always `~/.config/trackereditor/`.
- CI/CD workflows live in `.github/workflows/`: `cicd_windows.yaml`, `cicd_macos.yaml`, `cicd_ubuntu.yaml` (builds gtk2/qt5/qt6 zips plus AppImage amd64/arm64 via linuxdeploy), and `snap.yml` for the Snap Store package.
- `trackereditor_cli` is built for Windows and Linux only — `cicd_macos.yaml` has no CLI build job. Its own FPCUnit suite is `TTestStartUpParameterCli` (see `source/test/test_start_up_parameter.pas`); it has no networking code, so `-TEST_SSL` is not supported.

## Commit Messages

Use the [Conventional Commits](https://www.conventionalcommits.org/) standard (the same as `.github/dependabot.yml`, prefix `chore`).

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

- Types: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `build`, `ci`, `chore`, `revert`.
- Scope is optional and names the area, e.g. `console`, `gui`, `decode`, `tests`: `fix(console): reject unknown parameters`.
- The description is imperative, lower case, without a trailing period, and about 72 characters or less.
- The body explains what was wrong and why the change was made, not how.
- Breaking change: `!` after the type/scope (`feat(console)!: ...`) and/or a `BREAKING CHANGE:` footer.
- A fix always comes with its test in the same commit (`fix`, not a separate `test` commit). Use `test` only for tests without a code change.
- When asked to write commit text, follow this format. Append the `Co-authored-by` trailer as a footer.