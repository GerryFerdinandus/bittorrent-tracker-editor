# SPDX-License-Identifier: MIT
# GNU Make entry point. Run "make help".
# Embarcadero Make and Microsoft's nmake do not work with this Makefile.
# Check that "make --version" identifies GNU Make.
# Recipes use cmd.exe on Windows and /bin/sh on macOS/Linux; Bash is not required.

.DEFAULT_GOAL := help
.PHONY: help all gui cli test test-build test-runner-build test-run check-lazbuild check check-gui
# Lazarus can rebuild shared LCL packages/configuration. Do not race these builds,
# even when the caller uses -j. Each requested project is rebuilt only once.
.NOTPARALLEL:

LAZBUILD ?= lazbuild
LAZBUILD_FLAGS ?=
BUILD_MODE ?= Release
TEST_ARGS ?= -a --format=plain
TEST_RUNNER ?=

ifeq ($(OS),Windows_NT)
SHELL := cmd.exe
.SHELLFLAGS := /c
HOST_OS := Windows
WIDGETSET ?= win32
EXE := .exe
NOOP := rem
native-path = $(subst /,\,$(1))
else
SHELL := /bin/sh
.SHELLFLAGS := -c
HOST_OS := $(shell uname -s)
ifeq ($(HOST_OS),Darwin)
WIDGETSET ?= cocoa
else
WIDGETSET ?= qt6
endif
EXE :=
NOOP := :
native-path = $(1)
endif

GUI_PROJECT := $(call native-path,source/project/tracker_editor/trackereditor.lpi)
CLI_PROJECT := $(call native-path,source/project/tracker_editor/trackereditor_cli.lpi)
TEST_PROJECT := $(call native-path,source/project/unit_test/tracker_editor_test.lpi)
TEST_BINARY := $(call native-path,./enduser/test_trackereditor$(EXE))

ifeq ($(HOST_OS),Darwin)
APP_TARGETS := gui
VALID_WIDGETSETS := cocoa
else ifeq ($(HOST_OS),Windows)
APP_TARGETS := gui cli
VALID_WIDGETSETS := win32
else
APP_TARGETS := gui cli
VALID_WIDGETSETS := gtk2 gtk3 qt5 qt6
endif

# Help uses Make's own output so it works without Lazarus or shell utilities.
define HELP_TEXT
BitTorrent Tracker Editor - build from source with GNU Make

Usage (run in the repository root):
  make help       Show this help (also the default for bare make)
  make gui        Build the GUI application
  make cli        Build the headless CLI (Windows/Linux only)
  make all        Build GUI + CLI (macOS: GUI only)
  make test       Build applications + Debug test runner, then run all tests
  make test-build Build applications + Debug test runner without running tests
  make test-runner-build
                  Build only the Debug test runner (for CLI-only CI jobs)
  make test-run   Run existing test/application binaries without rebuilding

Windows: ensure make is GNU Make, not Embarcadero Make or Microsoft nmake.
macOS: uses Cocoa; CLI builds/tests are excluded, matching GitHub Actions.
Applications default to Release. Executables are written to enduser.

During tests, the screen may flicker or windows may briefly appear as the GUI
application is repeatedly started and closed. This is expected.

Linux GUI examples (default: qt6):
  make gui WIDGETSET=gtk2
  make gui WIDGETSET=gtk3
  make gui WIDGETSET=qt5
  make gui WIDGETSET=qt6
Use the same WIDGETSET with all or test. Install the toolkit development
libraries first: libgtk2.0-dev, libgtk-3-dev, libqt5pas-dev or libqt6pas-dev
(Debian/Ubuntu package names). Qt builds require the matching Qt Pascal bindings.
CLI-only builds do not need GUI toolkit libraries.

Overrides:
  BUILD_MODE=Debug          Application build mode (Release or Debug)
  WIDGETSET=qt6             GUI toolkit (Windows: win32; macOS: cocoa)
  LAZBUILD=lazbuild         Executable name or path, without embedded quotes
  LAZBUILD_FLAGS="..."      Extra lazbuild options, e.g. --lazarusdir=...
  TEST_ARGS="..."           Test arguments (default: -a --format=plain)
  TEST_RUNNER="..."         Optional launcher, e.g. xvfb-run --auto-servernum

Examples:
  make all BUILD_MODE=Debug
  make test WIDGETSET=qt6
  make test TEST_RUNNER="xvfb-run --auto-servernum"
  make gui "LAZBUILD=C:\Program Files\Lazarus\lazbuild.exe"

First clone: git clone --recursive https://github.com/GerryFerdinandus/bittorrent-tracker-editor.git
Existing clone: git submodule update --init --recursive
Install GNU Make and Lazarus with FPC, then put lazbuild on PATH or set LAZBUILD.
Windows HTTPS needs libssl-3-x64.dll and libcrypto-3-x64.dll in enduser.
See SourceCodeBuild.txt for installation details and direct/IDE build alternatives.
endef

help:
	$(info $(HELP_TEXT))
	@$(NOOP)

# Checks run only for builds/tests: help works even on a fresh machine.
# Invoke lazbuild, rather than only searching PATH, to also check explicit paths.
check-lazbuild:
ifeq ($(HOST_OS),Windows)
	@"$(LAZBUILD)" --version >NUL 2>&1 || (echo ERROR: Cannot run lazbuild: "$(LAZBUILD)" & echo Install Lazarus with Free Pascal from https://www.lazarus-ide.org/ & echo Add its installation folder to PATH or set LAZBUILD to the executable path. & echo Example: make gui "LAZBUILD=C:\lazarus\lazbuild.exe" & exit /b 1)
else
	@"$(LAZBUILD)" --version >/dev/null 2>&1 || { printf '%s\n' 'ERROR: Cannot run lazbuild: "$(LAZBUILD)"' 'Install Lazarus with Free Pascal from https://www.lazarus-ide.org/' 'Add its installation folder to PATH or set LAZBUILD to the executable path.' 'Example: make gui LAZBUILD=/path/to/lazarus/lazbuild' >&2; exit 1; }
endif

check: check-lazbuild
ifneq ($(words $(BUILD_MODE)),1)
	$(error BUILD_MODE must be a single value: Release or Debug)
endif
ifeq ($(filter Release Debug,$(BUILD_MODE)),)
	$(error Invalid BUILD_MODE "$(BUILD_MODE)". Use Release or Debug)
endif
ifeq ($(wildcard submodule/dcpcrypt/dcpcrypt2.pas),)
	$(error DCPcrypt submodule is missing. Run: git submodule update --init --recursive)
endif
ifeq ($(wildcard submodule/dcpcrypt/Hashes/dcpsha256.pas),)
	$(error DCPcrypt SHA256 source is missing. Run: git submodule update --init --recursive)
endif

# Toolkit validation does not affect a headless CLI-only build.
check-gui: check
ifneq ($(words $(WIDGETSET)),1)
	$(error WIDGETSET must be a single value. Choose: $(VALID_WIDGETSETS))
endif
ifeq ($(filter $(VALID_WIDGETSETS),$(WIDGETSET)),)
	$(error Invalid WIDGETSET "$(WIDGETSET)" for $(HOST_OS). Choose: $(VALID_WIDGETSETS))
endif

all: $(APP_TARGETS)

gui: check-gui
	"$(LAZBUILD)" --build-all --build-mode=$(BUILD_MODE) --widgetset=$(WIDGETSET) $(LAZBUILD_FLAGS) "$(GUI_PROJECT)"

ifeq ($(HOST_OS),Darwin)
cli:
	$(error CLI builds are supported on Windows/Linux only. On macOS use make gui, make all or make test)
else
cli: check
	"$(LAZBUILD)" --build-all --build-mode=$(BUILD_MODE) $(LAZBUILD_FLAGS) "$(CLI_PROJECT)"
endif

# Real applications are prerequisites: integration tests launch their binaries.
# Tests always use Debug (assertions/range checks), even for Release applications.
test-build: $(APP_TARGETS) test-runner-build

test-runner-build: check-gui
	"$(LAZBUILD)" --build-all --build-mode=Debug --widgetset=$(WIDGETSET) $(LAZBUILD_FLAGS) "$(TEST_PROJECT)"

# Separate execution preserves a packaged GUI binary substituted by AppImage CI.
test: test-build test-run

test-run:
	$(TEST_RUNNER) "$(TEST_BINARY)" $(TEST_ARGS)
