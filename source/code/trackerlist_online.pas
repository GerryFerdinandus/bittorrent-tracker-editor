// SPDX-License-Identifier: MIT
unit trackerlist_online;

{
  Get the status of the TrackerURL by comparing with the 'online trackers database'
  These data are comming from https://newtrackon.com/
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  //What is the status of this tracker
  TTrackerListOnlineStatus = (
    tos_stable,            //< Most of the time present online
    tos_live_but_unstable, //< Is present online but unstable
    tos_dead,              //< Was alive but now is dead
    tos_unknown            //< not in the database present
    );

  { TTrackerListOnline }

  TTrackerListOnline = class
  private
  public
    // currently active and responding trackers.
    TrackerList_Live: TStringList;

    // trackers that have an uptime of equal or more than 95%.
    TrackerList_Stable: TStringList;

    // trackers that no longer present in 'live' list
    TrackerList_Dead: TStringList;

    //return the status of the TrackerURL
    function TrackerStatus(const TrackerURL: UTF8String): TTrackerListOnlineStatus;
    function TrackerListOnlineStatusToString(Value: TTrackerListOnlineStatus): string;
  end;


implementation


{ TTrackerListOnline }

function TTrackerListOnline.TrackerStatus(
  const TrackerURL: UTF8String): TTrackerListOnlineStatus;
  //var
  //  index: integer;
begin
  //look for this tracker in all the posible string list

  //TrackerList_Stable
  if Assigned(TrackerList_Stable) and (TrackerList_Stable.IndexOf(TrackerURL) >= 0) then
  begin
    Result := tos_stable;
    exit;
  end;

  //TrackerList_Dead
  if Assigned(TrackerList_Dead) and (TrackerList_Dead.IndexOf(TrackerURL) >= 0) then
  begin
    Result := tos_dead;
    exit;
  end;

  //TrackerList_Live
  if Assigned(TrackerList_Live) and (TrackerList_Live.IndexOf(TrackerURL) >= 0) then
  begin
    //This tracker is online but not in present in the stable list.
    //It is then mark as unstable
    Result := tos_live_but_unstable;
    exit;
  end;

  Result := tos_unknown;

end;

function TTrackerListOnline.TrackerListOnlineStatusToString(
  Value: TTrackerListOnlineStatus): string;
begin
  case Value of
    tos_stable: Result := 'Stable';
    tos_live_but_unstable: Result := 'Unstable';
    tos_dead: Result := 'Dead';
    tos_unknown: Result := 'Unknown';
    else
      begin
        Result := '';
        assert(True, 'Unknown TTrackerListOnlineStatus')
      end;
  end;
end;

end.
