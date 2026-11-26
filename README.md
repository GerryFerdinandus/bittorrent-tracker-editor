**bittorrent-tracker-editor** will add/remove bittorrent tracker from the torrent file(s).
This software works on Windows 7+, macOS and Linux.

---

## Software latest release: ##
[![GitHub Latest release](https://img.shields.io/github/release/GerryFerdinandus/bittorrent-tracker-editor/all.svg)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
[![GitHub commits since latest release (by SemVer including pre-releases)](https://img.shields.io/github/commits-since/gerryferdinandus/bittorrent-tracker-editor/latest)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/commits/main)
---

## Build Status: ##
Continuous integration|Status| Generate an executable file for the operating system| Download link
------------|---------|---------|----------
GitHub Actions |[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/gerryferdinandus/bittorrent-tracker-editor/cicd_ubuntu.yaml?label=Ubuntu)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/actions/workflows/cicd_ubuntu.yaml)[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/gerryferdinandus/bittorrent-tracker-editor/cicd_windows.yaml?label=Windows)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/actions/workflows/cicd_windows.yaml)[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/gerryferdinandus/bittorrent-tracker-editor/cicd_macos.yaml?label=macOS)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/actions/workflows/cicd_macos.yaml)|Linux(amd64), Windows(amd64) and macOS(Universal)|[![GitHub Latest release](https://img.shields.io/github/release/GerryFerdinandus/bittorrent-tracker-editor/all.svg)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
GitHub Actions (Ubuntu snap) |[![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/gerryferdinandus/bittorrent-tracker-editor/snap.yml)](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/actions/workflows/snap.yml)|Linux (amd64 and arm64)|[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-white.svg)](https://snapcraft.io/bittorrent-tracker-editor)
Flathub build server||Linux (amd64 and arm64)|<a href='https://flathub.org/apps/io.github.gerryferdinandus.bittorrent-tracker-editor'><img width='180' alt='Download on Flathub' src='https://flathub.org/assets/badges/flathub-badge-i-en.svg'/></a>
---

## Warning: ##
There is no backup function in this software. Use it at your own risk. Bittorrent works fine without this program. You probably don't need this software.

---

## Which program to use for add/remove bittorrent trackers? ##
  * **Edit one torrent file:** You can use http://torrenteditor.com/
  * **Edit multiple torrent files:** Use this program. It is made for changing multiple torrent files.

---

## Features: ##
  * Select one torrent file or a folder with torrent files.
  * Add one or more trackers at the same time.
  * Remove one or more trackers at the same time.
  * Remove all the trackers to create trackerless torrent. DHT torrent
  * Change public/private flag. Warning: This will change the torrent info HASH.
  * Preset add/remove tracker via add\_trackers.txt and remove\_trackers.txt files when present in the same folder as the executable file. (For linux snap version use: home/snap/bittorrent-tracker-editor/common/ )
  * Optional start as console program. (info: [console mode and parameters](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/console-mode.html), [tracker order modes](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/tracker-order-modes.html), also in [readme.txt](https://raw.githubusercontent.com/GerryFerdinandus/bittorrent-tracker-editor/refs/heads/main/enduser/readme.txt) inside the download)
  * Show torrent files content.
  * Download stable trackers from newTrackon or ngosang.

---

## Downloads: ##
  * [From GitHub: Executable file for Windows, macOS and Linux.](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases)
  * [From GitHub: Trackers list (from GitHub user: ngosang )](https://github.com/ngosang/trackerslist)
  * [From newtrackon.com: Trackers list (from GitHub user: CorralPeltzer )](https://newtrackon.com)

---

## Build from source: ##

Install **GNU Make** and **Lazarus with Free Pascal**, then:

```sh
git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git
cd bittorrent-tracker-editor
make help
make all
make test
```

Ensure `make --version` identifies GNU Make. Bare `make` shows help.
Use `make gui` or `make cli` to build just one application;
macOS supports GUI builds only. Linux defaults to Qt6; install its development
libraries and Qt Pascal bindings, or select `WIDGETSET=gtk2`, `gtk3`, or `qt5`.
`make test` builds all required applications before running the tests.
Executables are created in [enduser](enduser/).

See [SourceCodeBuild.txt](SourceCodeBuild.txt) for platform prerequisites,
toolkit examples, troubleshooting, and IDE/direct build alternatives.

---

## Documentation: ##
The documentation is on the **[GitHub Pages site](https://gerryferdinandus.github.io/bittorrent-tracker-editor/)**:
  * [User guide](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/): the [GUI](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/gui.html), the [console mode and its parameters](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/console-mode.html) and the [tracker order modes](https://gerryferdinandus.github.io/bittorrent-tracker-editor/user/tracker-order-modes.html).
  * [Software history](https://gerryferdinandus.github.io/bittorrent-tracker-editor/history.html): what is new in each version.
  * [Programmer guide](https://gerryferdinandus.github.io/bittorrent-tracker-editor/developer/): how the source code works, with diagrams.

The source of these pages is in the [docs](docs/) folder.

---

![](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases/download/V1.32.0/trackereditor_list_windows.png "Trackers List")

This screen shot show the program, after a folder is selected with torrent files inside. The normal procedure is to deselect the trackers that are no longer working. Optionally add your own trackers. And select the 'Update torrent' menu.

---

![](https://github.com/GerryFerdinandus/bittorrent-tracker-editor/releases/download/V1.32.0/trackereditor_info_windows.png "Files/Trackers/Info")

---

## License: ##
**bittorrent-tracker-editor** is released under [MIT License](http://www.opensource.org/licenses/MIT)

---

This program is developed using [Lazarus](http://lazarus.freepascal.org/) RAD and [Free Pascal](http://www.freepascal.org/) compiler.

---

This product includes software developed by the OpenSSL Project for use in the OpenSSL Toolkit (http://www.openssl.org/)
