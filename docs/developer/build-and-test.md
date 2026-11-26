---
title: Build and test
parent: Programmer guide
nav_order: 7
---

# Build and test
{: .no_toc }

1. TOC
{:toc}

## Tools

| Tool | Notes |
|:-----|:------|
| Lazarus + Free Pascal | `lazbuild` must be on the `PATH` (or set `LAZBUILD`). Current releases are built with Lazarus 3.x / FPC 3.2.2. |
| GNU Make | Embarcadero Make and nmake do **not** work. Check `make --version`. |
| Git submodule | `submodule/dcpcrypt` (SHA256). Clone with `--recursive`, or run `git submodule update --init --recursive`. |
| Linux GUI toolkit | One of `gtk2`, `gtk3`, `qt5`, `qt6` with its development libraries (`libgtk2.0-dev`, `libgtk-3-dev`, `libqt5pas-dev`, `libqt6pas-dev` on Debian/Ubuntu). Default is `qt6`. |
| Windows | `libssl-3-x64.dll` and `libcrypto-3-x64.dll` in `enduser/` for HTTPS. |

See also `SourceCodeBuild.txt` in the repository root for the platform prerequisites and troubleshooting.

## Make targets

```sh
git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git
cd bittorrent-tracker-editor
make help        # all targets and overrides
make all         # GUI + CLI (macOS: GUI only)
make test        # build the applications and the Debug test runner, then run all tests
```

| Target | What it does |
|:-------|:-------------|
| `make help` | Default target. Works without Lazarus. |
| `make gui` | Builds `trackereditor` (the GUI). |
| `make cli` | Builds `trackereditor_cli`. Windows and Linux only. |
| `make all` | `gui` and `cli`. |
| `make test` | `test-build`, then `test-run`. |
| `make test-build` | Applications + the Debug test runner. |
| `make test-runner-build` | Only the Debug test runner (used by CLI-only CI jobs). |
| `make test-run` | Runs the already built test binary. Does not rebuild (so an AppImage that CI substituted for `enduser/trackereditor` is tested). |

Overrides: `BUILD_MODE=Debug` (default `Release`), `WIDGETSET=qt6|gtk2|gtk3|qt5` (Windows `win32`, macOS `cocoa`), `LAZBUILD=...`, `LAZBUILD_FLAGS="..."` (for example `--lazarusdir=...`), `TEST_ARGS="..."` (default `-a --format=plain`) and `TEST_RUNNER="..."` (for example `xvfb-run --auto-servernum`).

The Makefile is `.NOTPARALLEL`: Lazarus can rebuild shared LCL packages and must not race.
Tests always use `Debug` (assertions and range checks), also when the applications are `Release`.

## Without Make: lazbuild

```sh
# GUI (Release)
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi
# Console only, no widgetset needed
lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi
# Unit tests (Debug)
lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi
# Run the tests
enduser/test_trackereditor -a --format=plain
```

Build modes: `Debug` (assertions, range checks, DWARF symbols) and `Release` (optimized, stripped). Linux widgetsets: `--widgetset=gtk2`, `gtk3`, `qt5`, `qt6`. macOS: `--widgetset=cocoa`. The executables go to `enduser/`, the compiled units to `lib/`.

## The tests

Framework: **FPCUnit** (`consoletestrunner`). The runner `tracker_editor_test.lpr` sets exit code 1 when a test fails or errors.

| Suite (class) | Unit | Tests |
|:--------------|:-----|:------|
| `TTestBEncode` | `test_bencode.pas` | Bencode decode/encode, damaged input. |
| `TTestDecodeTorrent` | `test_decodetorrent.pas` | V1/V2/hybrid detection, hashes, announce, flags, save. |
| `TTestUpdateTorrent` | `test_update_torrent.pas` | `UpdateTorrentFileList` on copies of real torrents. |
| `TTestTorrentMiscellaneous` | `test_torrent_miscellaneous.pas` | The 8 order modes, URL validation, argument decoder, private trackers, helpers. |
| `TTestTrackerListOnline` | `test_trackerlist_online.pas` | Tracker status. |
| `TTestNewTrackon` | `test_newtrackon.pas` | newTrackon (internet, skipped when unreachable). |
| `TTestNgosangTrackersList` | `test_ngosang_trackers_list.pas` | ngosang (internet, skipped when unreachable). |
| `TTestStartUpParameter` | `test_start_up_parameter.pas` | Starts the real **GUI executable** with parameters (console mode). |
| `TTestStartUpParameterCli` | `test_start_up_parameter.pas` | Same for `trackereditor_cli`, plus CLI specific tests. |

(`test_miscellaneous.pas` holds shared test helpers.)

Run one suite or one test:

```sh
enduser/test_trackereditor -a --format=plain --suite=TTestDecodeTorrent
enduser/test_trackereditor -a --format=plain --suite=TTestTorrentMiscellaneous.Test_Combine_U0_Insert_Before_Keep_New_Intact
```

Rules for tests:

- Test files are in `source/test/`, the project in `source/project/unit_test/`. Every test unit must be in the `uses` of `tracker_editor_test.lpr`.
- The files in `test_torrent/` are **fixtures**: no test or program run may change them. Check `git status test_torrent` after a test run. The integration tests work on a copy in a temp folder.
- The V2 and hybrid test torrents have no trackers: a test that needs trackers inside the torrent must fill them first.
- A fix comes with its test in the same commit.
- Tests that download use `Ignore` when the server is unreachable; a failing download is tested with `BaseURL := 'http://999.999.999.999/'`.
- The integration tests start the program with the work folder as the current directory, because an AppImage reads and writes the txt files in `$OWD`.

### Linux code path from a Windows machine (WSL)

**After adding or changing any test (or code under test), run the tests on Windows AND in WSL (Linux).** Linux CI has failed on things Windows hides: `CopyFile` that does not keep the execute bit, case sensitive file names, `{$IFDEF UNIX}` code.

1. Windows (build `trackereditor_cli` too: the console tests start the real executables from `enduser/`):

   ```powershell
   lazbuild --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi
   lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi
   enduser\test_trackereditor.exe -a --format=plain
   ```

2. WSL (Ubuntu with Lazarus, one widgetset is enough, `qt6` recommended; it needs `libqt6pas-dev`):

   ```powershell
   wsl.exe -e bash -lc "cd /mnt/f/github_project/lazarus/bittorrent-tracker-editor && \
     lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --widgetset=qt6 --build-all --build-mode=Release source/project/tracker_editor/trackereditor.lpi && \
     lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --build-all --build-mode=Release source/project/tracker_editor/trackereditor_cli.lpi && \
     lazbuild --lazarusdir=/usr/lib/lazarus/4.4 --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi"
   wsl.exe -e bash -lc "cd /mnt/f/github_project/lazarus/bittorrent-tracker-editor/enduser && ./test_trackereditor -a --format=plain"
   ```

Notes: `TTestStartUpParameter` runs only the console mode, so no X server/`DISPLAY` is needed. In PowerShell do not put `$variable` inside the double-quoted `bash -lc "..."` string. The Linux build leaves ELF binaries (no extension) in `enduser/` and `lib/*/x86_64-linux/`: delete them after testing.

## CI/CD (`.github/workflows/`)

| Workflow | Builds | Notes |
|:---------|:-------|:------|
| `cicd_windows.yaml` | Windows amd64 | `make test-build`, `make test-run`, zip with `enduser\*.txt`, `*.svg`, `*.exe`, `*.dll`. |
| `cicd_ubuntu.yaml` | Linux zips for **gtk2, gtk3, qt5, qt6** (amd64), **AppImage** (amd64 and arm64, with `linuxdeploy`), **CLI** zips (amd64 and arm64) | Runs the tests (GUI builds under `xvfb-run`); the AppImage is tested as `enduser/trackereditor`; the CLI job runs only `TTestStartUpParameterCli`. |
| `cicd_macos.yaml` | macOS universal app (x86_64 + aarch64), dmg | Cocoa. **No CLI build job.** |
| `ci_freebsd.yaml` | FreeBSD | Uses `gmake test-build` / `gmake test-run`. |
| `snap.yml` | Snap (amd64) | `snap/snapcraft.yaml`. |

The release zips contain `enduser/*.txt` and `enduser/*.svg`. That is how `readme.txt`, `add_trackers.txt`, `remove_trackers.txt` and `tracker-order-modes.svg` reach the user. The workflows delete `console_log.txt` and `export_trackers.txt` and `git restore` the two preset `.txt` files first, so that test runs do not leak into the zip.

Adding a file to `enduser/` that must ship? Check the globs above. A new file type (for example `*.md`) needs a change in the workflows.
