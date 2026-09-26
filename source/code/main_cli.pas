// SPDX-License-Identifier: MIT
unit main_cli;

{
 Entry point for trackereditor_cli. Runs headless: no Forms, no widgetset, always terminates
 immediately after processing the command line parameters.
}

{$mode objfpc}{$H+}

interface

procedure RunConsoleApplication;

implementation

uses
  SysUtils, main_common;

procedure RunConsoleApplication;
var
  FolderForTrackerListLoadAndSave: string;
begin
  FolderForTrackerListLoadAndSave := main_common.DetermineTrackerListFolder(ParamStr(0));

  if not main_common.RunConsoleMode(FolderForTrackerListLoadAndSave) then
    System.ExitCode := 1;
end;

end.
