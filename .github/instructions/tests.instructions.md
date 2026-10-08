---
description: "Use when writing, modifying, or reviewing FPCUnit test files for the bittorrent-tracker-editor project. Covers test structure, naming, assertions, and registration patterns."
applyTo: "source/test/**/*.pas"
---
# Test File Guidelines

## File Structure

Every test unit follows this skeleton:

```pascal
// SPDX-License-Identifier: MIT
unit test_<name>;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, <unit_under_test>;

type
  TTest<Name> = class(TTestCase)
  private
    // Test fixtures
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Each published procedure is a test case
    procedure Test_<DescriptiveName>;
  end;

implementation

// Test implementations...

initialization
  RegisterTest(TTest<Name>);
end.
```

## Naming Conventions

- Unit name: `test_<module>.pas` (e.g., `test_newtrackon.pas`)
- Test class: `TTest<ModuleName>` (e.g., `TTestNewTrackon`, `TTestStartUpParameter`)
- Test methods: `Test_<What_Is_Being_Tested>` — use underscores (e.g., `Test_API_Download`, `Test_Parameter_U0`)
- Mark incomplete tests with `_TODO` suffix (e.g., `Test_Parameter_U5_TODO`)

## Assertions

Use FPCUnit assertion methods:
- `Check(condition, 'message')` — boolean check (preferred)
- `CheckEquals(expected, actual, 'message')` — equality
- `Fail('message')` — explicit failure

Always include a descriptive message string.

## Fixtures

- Create objects in `SetUp`, free them in `TearDown`
- Use `test_miscellaneous.pas` helpers: `GetProjectRootFolderWithPathDelimiter`, `LoadConsoleLog`, `VerifyTrackerResult`, `SetFileReadOnly`

## Running Tests

```bash
lazbuild --build-all --build-mode=Debug source/project/unit_test/tracker_editor_test.lpi
enduser/test_trackereditor -a --format=plain
```

**After adding or changing a test, always run it on Windows AND in WSL (Linux). Never skip the WSL run.** Windows alone does not compile `{$IFDEF UNIX}`/`{$IFDEF LINUX}` branches and hides Linux differences (execute permission, case sensitive file names, `/tmp` paths). See "Testing the Linux code path via WSL" in `.github/copilot-instructions.md` for the exact commands, the suites to run and the clean-up of the Linux build output.

## Pitfalls

- `Fail`/`Check` raise `EAssertionFailedError`, which is an `Exception`. In `try ... except on E: Exception do ...` it is swallowed and the test can never fail. Re-raise it first: `on E: EAssertionFailedError do raise; on E: Exception do Raised := True;` and then `Check(Raised, ...)`.
- A test that runs a copied executable must set the execute bit on Unix (`FpChmod(Path, &755)` in `{$IFDEF UNIX}`): `CopyFile` does not keep it and the start fails with error code 127.
- Tests must not leave files behind or depend on the order of other tests; use a temp folder and delete it in `finally`/`TearDown`.
- `for S in ['-U8', '-U10'] do` types the literal after its first element, so longer items are cut (`-U10` became `-U1`). Use a typed `const X: array[0..N] of string = (...)`.
- Arguments are evaluated before the call: `Check(Verify(X), 'msg ' + X.ErrorString)` builds the message before `Verify` has set it. Call first, then `Check(OK, ...)`.
- Quote every path in a command line (`QuoteParameter`), a path with a space is else several parameters. `ExecuteProcess(Path, string)` drops an empty argument on Unix: pass an array of arguments there, and quote every argument on Windows.
- A test sets its own pre-condition. The test torrents are not what an earlier test left: the V2 and hybrid torrents have no trackers.
- A test of an `Assert` only works with assertions on (the Debug test build): wrap it in `{$IFOPT C+}`.
- After writing a test, break the code under test on purpose (a mutation) and check that the test fails. Restore the code and rebuild before the final run. A mutation that no test catches can mean the code is redundant: check before you add a test for it.
- An unreadable file can be made with `TFileStream.Create(Name, fmOpenRead or fmShareExclusive)`: it works on Windows and Linux, and no chmod is needed (a chmod does not stop root). Free the stream in `finally`.
- Unix only tests (symlinks, permissions) go in `{$IFDEF UNIX}`, also the declaration in the class. They only run in WSL, so run it.
- Platform rules that read the environment: test a pure function with the values as parameters (`PackagedTrackerListFolder`), do not change the environment of the test process. Use `Ignore('reason')` when the machine itself decides (macOS, a snap or flatpak run).

## Registration

Every test class must be registered in the `initialization` section of its unit AND listed in the test project's `.lpr` file (`source/project/unit_test/tracker_editor_test.lpr`) and `.lpi` file (`<Units>`, update `Count`).
