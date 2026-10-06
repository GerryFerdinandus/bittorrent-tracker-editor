// SPDX-License-Identifier: MIT
unit test_newtrackon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, newtrackon; //testutils

type

  { TTestNewTrackon }

  TTestNewTrackon = class(TTestCase)
  private
    FNewTrackon: TNewTrackon;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_API_Download;
    procedure Test_API_Upload;
    procedure Test_Dead_List_Is_All_Without_Live;
    procedure Test_Dead_List_Without_Live_Trackers_Is_The_Whole_All_List;
    procedure Test_Dead_List_Without_All_Trackers_Is_Empty;
    procedure Test_Dead_List_Is_Replaced_Not_Added_To;
    procedure Test_Failed_Download_Is_Reported_And_Gives_No_Dead_List;
    procedure Test_Failed_Submit_Is_Reported;
  end;

implementation

procedure TTestNewTrackon.Test_API_Download;
begin
  //An outage of newtrackon.com is a network issue, not a code defect: skip instead of failing the build.
  if not FNewTrackon.DownloadEverything then
    Ignore('newtrackon.com is unreachable; skipping network-dependent test');

  Check(FNewTrackon.TrackerList_All.Count > 0,
    'TrackerList_All should never be empty');

  Check(FNewTrackon.TrackerList_Live.Count > 0,
    'TrackerList_Live should never be empty');

  Check(FNewTrackon.TrackerList_Stable.Count > 0,
    'TrackerList_Stable should never be empty');

  Check(FNewTrackon.TrackerList_Udp.Count > 0,
    'TrackerList_Udp should never be empty');

  Check(FNewTrackon.TrackerList_Http.Count > 0,
    'TrackerList_Http should never be empty');

  Check(FNewTrackon.TrackerList_Dead.Count > 0,
    'TrackerList_Dead should never be empty');
end;

procedure TTestNewTrackon.Test_API_Upload;
var
  TrackerList: TStringList;
  TrackersSendCount: integer;
begin
  TrackerList := TStringList.Create;

  //Add two trackers
  TrackerList.Add('udp://tracker.leechers-paradise.org:6969/announce');
  TrackerList.Add('udp://tracker.test.org:6969/announce');//dummy URL
  TrackerList.Add('wss://tracker.openwebtorrent.com');

  //Test if upload is OK
  try
    //An outage of newtrackon.com is a network issue, not a code defect: skip instead of failing the build.
    if not FNewTrackon.SubmitTrackers(TrackerList, TrackersSendCount) then
      Ignore('newtrackon.com is unreachable; skipping network-dependent test');

    Check(TrackersSendCount <= TrackerList.Count, 'TrackersSendCount have too high value');
  finally
    TrackerList.Free;
  end;

end;

procedure TTestNewTrackon.Test_Dead_List_Is_All_Without_Live;
begin
  FNewTrackon.TrackerList_All.Add('udp://a.test:6969/announce');
  FNewTrackon.TrackerList_All.Add('udp://b.test:6969/announce');
  FNewTrackon.TrackerList_All.Add('udp://c.test:6969/announce');
  FNewTrackon.TrackerList_All.Add('udp://d.test:6969/announce');
  FNewTrackon.TrackerList_Live.Add('udp://d.test:6969/announce');
  FNewTrackon.TrackerList_Live.Add('udp://b.test:6969/announce');
  //A live tracker that is not in the all list does not make a dead tracker
  FNewTrackon.TrackerList_Live.Add('udp://other.test:6969/announce');

  FNewTrackon.CreateTrackerList_Dead;

  CheckEquals(2, FNewTrackon.TrackerList_Dead.Count, 'Wrong dead tracker count');
  CheckEquals('udp://a.test:6969/announce', FNewTrackon.TrackerList_Dead[0],
    'Wrong first dead tracker');
  CheckEquals('udp://c.test:6969/announce', FNewTrackon.TrackerList_Dead[1],
    'Wrong second dead tracker');
  CheckEquals(4, FNewTrackon.TrackerList_All.Count, 'The all list must not change');
  CheckEquals(3, FNewTrackon.TrackerList_Live.Count, 'The live list must not change');
end;

procedure TTestNewTrackon.Test_Dead_List_Without_Live_Trackers_Is_The_Whole_All_List;
begin
  FNewTrackon.TrackerList_All.Add('udp://a.test:6969/announce');
  FNewTrackon.TrackerList_All.Add('udp://b.test:6969/announce');

  FNewTrackon.CreateTrackerList_Dead;

  CheckEquals(2, FNewTrackon.TrackerList_Dead.Count, 'Every tracker is dead');
end;

procedure TTestNewTrackon.Test_Dead_List_Without_All_Trackers_Is_Empty;
begin
  FNewTrackon.TrackerList_Live.Add('udp://a.test:6969/announce');

  FNewTrackon.CreateTrackerList_Dead;

  CheckEquals(0, FNewTrackon.TrackerList_Dead.Count, 'There can be no dead tracker');
end;

procedure TTestNewTrackon.Test_Dead_List_Is_Replaced_Not_Added_To;
begin
  FNewTrackon.TrackerList_All.Add('udp://a.test:6969/announce');
  FNewTrackon.TrackerList_All.Add('udp://b.test:6969/announce');
  FNewTrackon.CreateTrackerList_Dead;
  CheckEquals(2, FNewTrackon.TrackerList_Dead.Count, 'First dead list');

  //A new download: a is alive now
  FNewTrackon.TrackerList_Live.Add('udp://a.test:6969/announce');
  FNewTrackon.CreateTrackerList_Dead;

  CheckEquals(1, FNewTrackon.TrackerList_Dead.Count, 'The old dead list must be replaced');
  CheckEquals('udp://b.test:6969/announce', FNewTrackon.TrackerList_Dead[0],
    'Wrong dead tracker');
end;

procedure TTestNewTrackon.Test_Failed_Download_Is_Reported_And_Gives_No_Dead_List;
begin
  //An invalid IP address fails at once, without waiting for a connection timeout.
  FNewTrackon.BaseURL := 'http://999.999.999.999/';

  CheckFalse(FNewTrackon.DownloadEverything, 'DownloadEverything must fail');
  CheckFalse(FNewTrackon.Download_All_Live_Stable, 'Download_All_Live_Stable must fail');
  CheckEquals(0, FNewTrackon.TrackerList_Dead.Count, 'There must be no dead list');
end;

procedure TTestNewTrackon.Test_Failed_Submit_Is_Reported;
var
  TrackerList: TStringList;
  TrackersSendCount: integer;
begin
  FNewTrackon.BaseURL := 'http://999.999.999.999/';
  TrackerList := TStringList.Create;
  try
    TrackerList.Add('udp://a.test:6969/announce');

    CheckFalse(FNewTrackon.SubmitTrackers(TrackerList, TrackersSendCount),
      'A failed submit must be reported');
    CheckEquals(0, TrackersSendCount, 'Nothing was sent');
  finally
    TrackerList.Free;
  end;
end;

procedure TTestNewTrackon.SetUp;
begin
  WriteLn('TTestNewTrackon.SetUp');
  FNewTrackon := TNewTrackon.Create;
end;

procedure TTestNewTrackon.TearDown;
begin
  WriteLn('TTestNewTrackon.TearDown');
  FNewTrackon.Free;
end;

initialization

  RegisterTest(TTestNewTrackon);
end.
