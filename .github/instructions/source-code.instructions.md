---
description: "Use when writing, modifying, or reviewing Free Pascal source units in the bittorrent-tracker-editor project. Covers file headers, naming conventions, architecture boundaries, and constant usage."
applyTo: "source/code/**/*.pas"
---
# Source Code Guidelines

## Required File Header

Every unit starts with:

```pascal
// SPDX-License-Identifier: MIT
unit <unit_name>;

{$mode objfpc}{$H+}
```

Exception: `bencode.pas` uses `// SPDX-License-Identifier: CC0-1.0` (third-party origin).

## Naming Conventions

| Element | Pattern | Examples |
|---------|---------|----------|
| Classes | `T` prefix | `TDecodeTorrent`, `TControllerTrackerListOnline` |
| Enums | `T` prefix, values with module prefix | `TTorrentVersion` → `tv_V1`, `tv_Hybrid` |
| Fields | `F` prefix | `FTrackerList`, `FMemoryStream` |
| Constants | `UPPER_SNAKE_CASE` with domain prefix | `BK_ANNOUNCE`, `TEST_TORRENT_FILES_COUNT` |
| Functions | `CamelCase` | `SanitizeTrackerList`, `GetAnnounceList` |

## No Magic Strings

Use named constants for bencode dictionary keys. See `BK_*` constants in `decodetorrent.pas`:

```pascal
// Good
TempBEncoded := FBEncoded.ListData.FindElement(BK_ANNOUNCE);

// Bad
TempBEncoded := FBEncoded.ListData.FindElement('announce');
```

## Architecture Boundaries

- **Models** (`bencode.pas`, `decodetorrent.pas`): No GUI imports. No `Forms`, `Controls`, `Grids`.
- **Controllers** (`controller_*.pas`): Bridge between models and GUI. Import `Grids`/`ComCtrls` but not `Forms`.
- **API clients** (`newtrackon.pas`, `ngosang_trackerslist.pas`): Import `fphttpclient`. Use `torrent_miscellaneous` for tracker list cleanup.
- **Main form** (`main.pas`): Orchestrates everything. Only unit that imports `Forms`. It can not be unit tested, so keep the decisions out of the event handlers: put them in a function in `main_common.pas` or `torrent_miscellaneous.pas` and test that.
- **Utilities** (`torrent_miscellaneous.pas`, `trackerlist_online.pas`): No GUI imports.
- **Console engine** (`main_common.pas`, `update_torrent.pas`): No GUI imports. Shared by the GUI and `trackereditor_cli`.
- **Test seams**: a class that downloads has a `BaseURL` property (`TNewTrackon`, `TngosangTrackerList`), so a test can force a failing download without a network.

Dependency flow: `main` → controllers → models → `bencode`. Never import upward.

## Strings

All strings are UTF-8. Use `UTF8String` or `string` (equivalent under `{$H+}`). Use `LazUTF8` functions (`UTF8CompareText`, `UTF8Pos`, `UTF8Trim`) for string operations.

## Submodule

`submodule/dcpcrypt/` provides SHA256 via `DCPsha256`. Never modify files in this directory.
