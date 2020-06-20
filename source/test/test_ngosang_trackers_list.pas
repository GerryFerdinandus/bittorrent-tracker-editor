// SPDX-License-Identifier: MIT
unit test_ngosang_trackers_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ngosang_trackerslist;  //testutils,

type

  TTestNgosangTrackersList = class(TTestCase)
  private
    FngosangTrackerList: TngosangTrackerList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_DownloadAPI;
  end;

implementation

procedure TTestNgosangTrackersList.Test_DownloadAPI;
begin
  Check(FngosangTrackerList.TrackerList_Blacklist.Count > 0,
    'TrackerList_Blacklist should never be empty');

  Check(FngosangTrackerList.TrackerList_All.Count > 0,
    'TrackerList_All should never be empty');

  Check(FngosangTrackerList.TrackerList_All_HTTP.Count > 0,
    'TrackerList_All_HTTP should never be empty');

{ //HTTPS is not popular. it may be empty
  Check(
    FngosangTrackerList.TrackerList_All_HTTPS.Count > 0,
    'TrackerList_All_HTTPS should never be empty');
}
  Check(
    FngosangTrackerList.TrackerList_All_IP.Count > 0,
    'TrackerList_All_IP should never be empty');

  Check(
    FngosangTrackerList.TrackerList_All_UDP.Count > 0,
    'TrackerList_All_UDP should never be empty');

  Check(
    FngosangTrackerList.TrackerList_All_WS.Count > 0,
    'TrackerList_All_WS should never be empty');

  Check(
    FngosangTrackerList.TrackerList_Best.Count > 0,
    'TrackerList_Best should never be empty');

  Check(
    FngosangTrackerList.TrackerList_Best_IP.Count > 0,
    'TrackerList_Best_IP should never be empty');
end;

procedure TTestNgosangTrackersList.SetUp;
begin
  WriteLn('TTestNgosangTrackersList.SetUp');
  FngosangTrackerList := TngosangTrackerList.Create;
end;

procedure TTestNgosangTrackersList.TearDown;
begin
  WriteLn('TTestNgosangTrackersList.TearDown');
  FngosangTrackerList.Free;
end;

initialization

  RegisterTest(TTestNgosangTrackersList);
end.
