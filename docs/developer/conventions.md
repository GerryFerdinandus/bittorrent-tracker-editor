---
title: Conventions and design decisions
parent: Programmer guide
nav_order: 8
---

# Conventions and design decisions
{: .no_toc }

1. TOC
{:toc}

## Code style

- Every source file starts with `// SPDX-License-Identifier: MIT`. The exception is `bencode.pas` (`CC0-1.0`, third party origin).
- Every unit uses `{$mode objfpc}{$H+}` (ObjFPC mode, long strings).
- All strings are UTF-8. `UTF8String` is the same as `string` under `{$H+}`. Use the `LazUTF8` functions (`UTF8CompareText`, `UTF8Pos`, `UTF8Trim`, `UTF8LowerCase`) for string operations.
- No magic strings: use named constants, for the bencode keys the `BK_*` constants of `decodetorrent.pas`.

| Element | Pattern | Examples |
|:--------|:--------|:---------|
| Classes | `T` prefix | `TDecodeTorrent`, `TFormTrackerModify` |
| Enums | `T` prefix; values with a module specific prefix | `TTorrentVersion` -> `tv_V1`; `TTrackerListOrder` -> `tloSort`; newTrackon `ntl_URL_All`; ngosang `ngl_URL_All` |
| Fields | `F` prefix | `FTrackerList`, `FMemoryStream` |
| Constants | `UPPER_SNAKE_CASE` with a domain prefix | `BK_ANNOUNCE`, `FILE_NAME_ADD_TRACKERS` |
| Functions, procedures | `CamelCase` | `SanitizeTrackerList`, `GetAnnounceList` |

Comments: only where the code needs a bit of clarification.

## Architecture rules

- Never import upward: `main` -> controllers -> engine/model -> `bencode`. See [Architecture](architecture.html).
- Models, utilities and the console engine have **no GUI imports** (`Forms`, `Controls`, `Grids`).
- Controllers import `Grids`/`ComCtrls` but not `Forms`.
- API clients import `fphttpclient` and use `torrent_miscellaneous` for list cleaning.
- `main.pas` is the only unit that imports `Forms`. It cannot be unit tested: put every decision in `main_common.pas` or `torrent_miscellaneous.pas`.
- The code that the GUI and the console share lives in `main_common.pas` and `torrent_miscellaneous.pas`. Do not copy it into `main.pas`.
- `submodule/dcpcrypt/` is never modified.

## Design decisions and known minor issues

These are **accepted**: do not report them again as a bug.

| Topic | Decision |
|:------|:---------|
| Case of tracker URLs | Tracker lists are `TStringList` with the default `CaseSensitive = False`. `IndexOf` and `dupIgnore` treat URLs that differ only in case (for example a passkey in the path or query) as the same tracker, although only the scheme and host are case insensitive (RFC 3986). It is rare, and the console version never shows the duplicate end result. |
| Scheme case | `ValidTrackerURL` accepts only a **lower case** scheme (`udp://`, not `UDP://`). This is the current behaviour, not a decision. RFC 3986 allows any case. Change it only on purpose, together with `WebTorrentTrackerURL` and its test. |
| Tiers | `announce-list` tiers are flattened to one tracker per tier. Only the first tracker of a tier is read, and every tracker is written as its own tier. See [Torrent file format](torrent-format.html). |
| `remove_trackers.txt` | The lines are by design **not** validated as tracker URLs (unlike `add_trackers.txt`). A line that matches no tracker is skipped. An empty file (present) means: remove every tracker inside the torrents. |
| Private trackers | The trackers of private torrents are kept in `TTrackerList.TrackerFromPrivateTorrentsList` and are **never** sent to newTrackon (the URL can have a passkey). Always use `GetTrackersForOnlineSubmit`, also when the same URL is in a public torrent. |
| Saving | `TDecodeTorrent.SaveTorrent` writes a temp file and renames it. It keeps the owner and permissions, and on Unix follows a symlink, so the link stays a symlink. |
| Console failures | Console mode changes **no** torrent file when `add_trackers.txt` has an invalid URL or `remove_trackers.txt` cannot be read. The error is in `console_log.txt` and the exit code is 1. |
| Unknown parameters | Any other console parameter is an error, and the value after `-SOURCE` is never read as a parameter. |
| OpenSSL 3 | The DLLs `libssl-3-x64.dll` and `libcrypto-3-x64.dll` are required at run time on Windows for HTTPS. The `{$IFDEF VER3_2_2}` block in `fix_openssl.pas` stays until FPC detects OpenSSL 3 itself. |
| `trackereditor_cli` | Built for Windows and Linux only (no macOS job). No networking code, so `-TEST_SSL` is not supported. |

## Where the files are loaded and saved

See [Console mode](console-mode.html#where-the-text-files-are-determinetrackerlistfolder). The rule per packaging format is: Snap -> `$SNAP_USER_COMMON`, Flatpak -> `$XDG_DATA_HOME`, AppImage -> `$OWD`, macOS -> `~/.config/trackereditor/`, otherwise the folder of the executable.

## Commit messages

[Conventional Commits](https://www.conventionalcommits.org/):

```text
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

- Types: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `build`, `ci`, `chore`, `revert`.
- Scope is optional and names the area: `console`, `gui`, `decode`, `tests`, `pages`.
- The description is imperative, lower case, without a trailing period, about 72 characters or less.
- The body explains what was wrong and why, not how.
- Breaking change: `!` after the type or scope and/or a `BREAKING CHANGE:` footer.
- A fix always comes with its test in the same commit. Use `test` only for tests without a code change.
