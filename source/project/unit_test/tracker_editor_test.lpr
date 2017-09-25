program tracker_editor_test;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,  fpcunit,
  fpcunitreport, test_newtrackon, test_ngosang_trackers_list, test_start_up_parameter,
torrent_miscellaneous, test_miscellaneous, ngosang_trackerslist;

type

  { TLazTestRunner }

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
    procedure DoTestRun(ATest: TTest); override;
  end;

var
  Application: TMyTestRunner;

{ TMyTestRunner }

procedure TMyTestRunner.DoTestRun(ATest: TTest);
var
  ResultsWriter: TCustomResultsWriter;
  TestResult: TTestResult;
begin
  ResultsWriter := GetResultsWriter;
  ResultsWriter.Filename := FileName;
  TestResult := TTestResult.Create;
  try
    TestResult.AddListener(ResultsWriter);
    ATest.Run(TestResult);
    ResultsWriter.WriteResult(TestResult);

    //if something failed then exit with error: 1
    if (TestResult.NumberOfErrors > 0) or (TestResult.NumberOfFailures > 0) then
    begin
      System.ExitCode := 1;
    end;

  finally
    TestResult.Free;
    ResultsWriter.Free;
  end;
end;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
