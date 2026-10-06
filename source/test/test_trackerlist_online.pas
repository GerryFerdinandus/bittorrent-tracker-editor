// SPDX-License-Identifier: MIT
unit test_trackerlist_online;

{
  The classification of a tracker URL against the online tracker lists. No internet is needed.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, trackerlist_online;

type

  { TTestTrackerListOnline }

  TTestTrackerListOnline = class(TTestCase)
  private
    FTrackerListOnline: TTrackerListOnline;
    FLive, FStable, FDead: TStringList;

    procedure CheckStatus(Expected: TTrackerListOnlineStatus; const TrackerURL: UTF8String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Stable_Tracker;
    procedure Test_Live_Tracker_That_Is_Not_Stable_Is_Unstable;
    procedure Test_Dead_Tracker;
    procedure Test_Tracker_In_No_List_Is_Unknown;
    procedure Test_Stable_Has_Priority_Over_Live_And_Dead;
    procedure Test_Dead_Has_Priority_Over_Live;
    procedure Test_Empty_Lists_Give_Unknown;
    procedure Test_Missing_Lists_Give_Unknown_And_Do_Not_Crash;
    procedure Test_Only_A_Complete_URL_Matches;
    procedure Test_Status_To_String;
  end;

implementation

const
  URL_STABLE = 'udp://stable.test:6969/announce';
  URL_LIVE = 'udp://live.test:6969/announce';
  URL_DEAD = 'udp://dead.test:6969/announce';
  URL_UNKNOWN = 'udp://unknown.test:6969/announce';

procedure TTestTrackerListOnline.SetUp;
begin
  FLive := TStringList.Create;
  FStable := TStringList.Create;
  FDead := TStringList.Create;

  //The live list has all the trackers that are online, the stable ones too.
  FLive.Add(URL_STABLE);
  FLive.Add(URL_LIVE);
  FStable.Add(URL_STABLE);
  FDead.Add(URL_DEAD);

  FTrackerListOnline := TTrackerListOnline.Create;
  FTrackerListOnline.TrackerList_Live := FLive;
  FTrackerListOnline.TrackerList_Stable := FStable;
  FTrackerListOnline.TrackerList_Dead := FDead;
end;

procedure TTestTrackerListOnline.TearDown;
begin
  FTrackerListOnline.Free;
  FDead.Free;
  FStable.Free;
  FLive.Free;
end;

procedure TTestTrackerListOnline.CheckStatus(Expected: TTrackerListOnlineStatus;
  const TrackerURL: UTF8String);
begin
  CheckEquals(Ord(Expected), Ord(FTrackerListOnline.TrackerStatus(TrackerURL)),
    'Wrong status for ''' + TrackerURL + ''', expected ' +
    FTrackerListOnline.TrackerListOnlineStatusToString(Expected));
end;

procedure TTestTrackerListOnline.Test_Stable_Tracker;
begin
  CheckStatus(tos_stable, URL_STABLE);
end;

procedure TTestTrackerListOnline.Test_Live_Tracker_That_Is_Not_Stable_Is_Unstable;
begin
  CheckStatus(tos_live_but_unstable, URL_LIVE);
end;

procedure TTestTrackerListOnline.Test_Dead_Tracker;
begin
  CheckStatus(tos_dead, URL_DEAD);
end;

procedure TTestTrackerListOnline.Test_Tracker_In_No_List_Is_Unknown;
begin
  CheckStatus(tos_unknown, URL_UNKNOWN);
  CheckStatus(tos_unknown, '');
end;

procedure TTestTrackerListOnline.Test_Stable_Has_Priority_Over_Live_And_Dead;
begin
  //A stable tracker is also in the live list, and in this case also in the dead list
  FDead.Add(URL_STABLE);
  CheckStatus(tos_stable, URL_STABLE);
end;

procedure TTestTrackerListOnline.Test_Dead_Has_Priority_Over_Live;
begin
  FLive.Add(URL_DEAD);
  CheckStatus(tos_dead, URL_DEAD);
end;

procedure TTestTrackerListOnline.Test_Empty_Lists_Give_Unknown;
begin
  FLive.Clear;
  FStable.Clear;
  FDead.Clear;

  CheckStatus(tos_unknown, URL_STABLE);
  CheckStatus(tos_unknown, URL_LIVE);
  CheckStatus(tos_unknown, URL_DEAD);
end;

procedure TTestTrackerListOnline.Test_Missing_Lists_Give_Unknown_And_Do_Not_Crash;
begin
  //Before the first download the lists are not assigned
  FTrackerListOnline.TrackerList_Live := nil;
  FTrackerListOnline.TrackerList_Stable := nil;
  FTrackerListOnline.TrackerList_Dead := nil;
  CheckStatus(tos_unknown, URL_STABLE);

  //Only one list is assigned
  FTrackerListOnline.TrackerList_Dead := FDead;
  CheckStatus(tos_dead, URL_DEAD);
  CheckStatus(tos_unknown, URL_LIVE);
end;

procedure TTestTrackerListOnline.Test_Only_A_Complete_URL_Matches;
begin
  CheckStatus(tos_unknown, 'udp://stable.test:6969');
  CheckStatus(tos_unknown, URL_STABLE + '/');
  CheckStatus(tos_unknown, 'stable.test:6969/announce');
end;

procedure TTestTrackerListOnline.Test_Status_To_String;
begin
  CheckEquals('Stable', FTrackerListOnline.TrackerListOnlineStatusToString(tos_stable),
    'tos_stable');
  CheckEquals('Unstable',
    FTrackerListOnline.TrackerListOnlineStatusToString(tos_live_but_unstable),
    'tos_live_but_unstable');
  CheckEquals('Dead', FTrackerListOnline.TrackerListOnlineStatusToString(tos_dead),
    'tos_dead');
  CheckEquals('Unknown', FTrackerListOnline.TrackerListOnlineStatusToString(tos_unknown),
    'tos_unknown');
end;

initialization
  RegisterTest(TTestTrackerListOnline);
end.
