{ MIT licence
Copyright (c) Gerry Ferdinandus
}

unit newtrackon;

{
Use api from newtrackon to get tracker list that are working or not.
see: https://newtrackon.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type


  { TNewTrackon }

  TNewTrackon = class
  private
    FTrackerList_All, FTrackerList_Live, FTrackerList_Stable,
    FTrackerList_Dead: TStringList;

    procedure CreateTrackerList_Dead;

    //need to move to miscellaneous.pas
    procedure RemoveTrackersFromList(RemoveList, UpdatedList: TStringList);
    procedure SanatizeTrackerList(StringList: TStringList);

  public
    // all known trackers, dead or alive
    property TrackerList_All: TStringList read FTrackerList_All;

    // currently active and responding trackers.
    property TrackerList_Live: TStringList read FTrackerList_Live;

    // trackers that have an uptime of equal or more than 95%.
    property TrackerList_Stable: TStringList read FTrackerList_Stable;

    // trackers that no longer present in 'live' list
    property TrackerList_Dead: TStringList read FTrackerList_Dead;




    //Download all the trackers via API
    function DownloadTrackers: boolean;

    //create/destroy class object
    constructor Create;
    destructor Destroy; override;
  end;



implementation

uses fphttpclient, LazUTF8;

const
  URL_ALL: string = 'https://newtrackon.com/api/all';
  URL_LIVE: string = 'https://newtrackon.com/api/live';
  URL_STABLE: string = 'https://newtrackon.com/api/stable';

{ TNewTrackon }



procedure TNewTrackon.RemoveTrackersFromList(RemoveList, UpdatedList: TStringList);
var
  TrackerStr: string;
  i: integer;
begin
  //Remove the trackers that we do not want in the list
  for TrackerStr in RemoveList do
  begin
    //Find the tracker and remove it from the list.
    i := UpdatedList.IndexOf(UTF8Trim(TrackerStr));
    if i >= 0 then
      UpdatedList.Delete(i);
  end;
end;

procedure TNewTrackon.SanatizeTrackerList(StringList: TStringList);
var
  TrackerStr: UTF8String;
  i: integer;
  PositionSpace: PtrInt;
begin
  //remove all empty space and comment after the URL

  if StringList.Count > 0 then
  begin
    for i := 0 to StringList.Count - 1 do
    begin
      //process every line one by one
      TrackerStr := StringList[i];

      //remove empty spaces at the begin/end of line
      TrackerStr := UTF8Trim(TrackerStr);

      //find the first 'space' found in line
      PositionSpace := UTF8Pos(' ', TrackerStr);
      if PositionSpace > 0 then
      begin
        // There is a 'space' found
        // Remove everything after this 'space'
        TrackerStr := UTF8LeftStr(TrackerStr, PositionSpace - 1);
      end;

      //write the modified string back
      StringList[i] := TrackerStr;
    end;
  end;

end;


procedure TNewTrackon.CreateTrackerList_Dead;
begin
  //FTrackerList_Dead = FTrackerList_All - FTrackerList_Live;
  FTrackerList_Dead.Assign(FTrackerList_All);
  RemoveTrackersFromList(FTrackerList_Live, FTrackerList_Dead);
end;

function TNewTrackon.DownloadTrackers: boolean;
var
  str: UTF8String;
begin
  try
    //fill all the list one by one
    str := TFPCustomHTTPClient.SimpleGet(URL_ALL);
    FTrackerList_All.DelimitedText := str;

    str := TFPCustomHTTPClient.SimpleGet(URL_LIVE);
    FTrackerList_Live.DelimitedText := str;

    str := TFPCustomHTTPClient.SimpleGet(URL_STABLE);
    FTrackerList_Stable.DelimitedText := str;

    SanatizeTrackerList(FTrackerList_All);
    SanatizeTrackerList(FTrackerList_Live);
    SanatizeTrackerList(FTrackerList_Stable);

    CreateTrackerList_Dead;

    Result := True;
  except
    //No OpenSSL or web server is down
    Result := False;
  end;
end;

constructor TNewTrackon.Create;
begin

  FTrackerList_All := TStringList.Create;
  FTrackerList_Live := TStringList.Create;
  FTrackerList_Stable := TStringList.Create;
  FTrackerList_Dead := TStringList.Create;


  FTrackerList_All.Duplicates := dupIgnore;
  FTrackerList_Live.Duplicates := dupIgnore;
  FTrackerList_Stable.Duplicates := dupIgnore;
  FTrackerList_Dead.Duplicates := dupIgnore;

end;

destructor TNewTrackon.Destroy;
begin
  FTrackerList_All.Free;
  FTrackerList_Live.Free;
  FTrackerList_Stable.Free;
  FTrackerList_Dead.Free;

  inherited Destroy;
end;


end.
