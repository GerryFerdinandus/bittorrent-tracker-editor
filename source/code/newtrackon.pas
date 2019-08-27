// SPDX-License-Identifier: MIT
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

  //All the type of tracker list.
  TNewTrackon_List = (
    ntl_URL_All,//< Download from internet
    ntl_URL_Live,//< Download from internet
    ntl_URL_Stable,//< Download from internet
    ntl_URL_UDP,//< Download from internet
    ntl_URL_HTTP,//< Download from internet
    ntl_CREATE_DEAD//< ntl_CREATE_DEAD is NOT download but created by comparing betwean tracker list
    );

  TNewTrackon = class
  private
    FTRackerList: array [TNewTrackon_List] of TStringList;

    function DownloadTracker(NewTrackon_List: TNewTrackon_List): boolean;
    procedure CreateTrackerList_Dead;
  public
    // all known trackers, dead or alive
    property TrackerList_All: TStringList read FTRackerList[ntl_URL_All];

    // currently active and responding trackers.
    property TrackerList_Live: TStringList read FTrackerList[ntl_URL_Live];

    // trackers that have an uptime of equal or more than 95%.
    property TrackerList_Stable: TStringList read FTrackerList[ntl_URL_Stable];

    // stable UDP trackers.
    property TrackerList_Udp: TStringList read FTrackerList[ntl_URL_UDP];

    // stable HTTP/HTTPS trackers.
    property TrackerList_Http: TStringList read FTrackerList[ntl_URL_UDP];

    // trackers that no longer present in 'live' list
    property TrackerList_Dead: TStringList read FTrackerList[ntl_CREATE_DEAD];

    //Download all the types the trackers via API
    function DownloadEverything: boolean;

    //Download only three types of the trackers via API
    function Download_All_Live_Stable: boolean;

    //Submit tracker to newTrackon via http POST
    function SubmitTrackers(TrackerList: TStringList;
      out TrackersSendCount: integer): boolean;

    //create/destroy class object
    constructor Create;
    destructor Destroy; override;
  end;



implementation

uses fphttpclient, LazUTF8, torrent_miscellaneous, httpdefs;

const
  URL: array [TNewTrackon_List] of string =
    (//Warning: the URL strings must be in the same order as TNewTrackon_List
    'https://newtrackon.com/api/all',
    'https://newtrackon.com/api/live',
    'https://newtrackon.com/api/stable',
    'https://newtrackon.com/api/udp',
    'https://newtrackon.com/api/http',
    ''//there is no dead tracker list api
    );

{ TNewTrackon }


procedure TNewTrackon.CreateTrackerList_Dead;
begin
  //FTrackerList_Dead = FTrackerList_All - FTrackerList_Live;
  TrackerList_Dead.Assign(TrackerList_All);
  RemoveTrackersFromList(TrackerList_Live, TrackerList_Dead);
end;

function TNewTrackon.DownloadTracker(NewTrackon_List: TNewTrackon_List): boolean;
begin

  try
    //there is no Dead tracker list to be downloaded. so it can be skip
    if NewTrackon_List <> ntl_CREATE_DEAD then
    begin
      //download via URL and put the data in the TrackerList
      //will create exception if something is wrong
      FTRackerList[NewTrackon_List].DelimitedText :=
        TFPCustomHTTPClient.SimpleGet(URL[NewTrackon_List]);
    end;

    Result := True;
  except
    //No OpenSSL or web server is down
    Result := False;
  end;

  //Clean up the tracker list just downloaded
  SanatizeTrackerList(FTRackerList[NewTrackon_List]);
end;

function TNewTrackon.DownloadEverything: boolean;
var
  i: TNewTrackon_List;
begin
  //download all the list one by one
  for i in TNewTrackon_List do
  begin
    Result := DownloadTracker(i);
    if not Result then
      Exit;
  end;

  CreateTrackerList_Dead;
end;

function TNewTrackon.Download_All_Live_Stable: boolean;
begin
  Result := DownloadTracker(ntl_URL_All);
  if Result then
  begin
    Result := DownloadTracker(ntl_URL_Stable);
    if Result then
    begin
      Result := DownloadTracker(ntl_URL_Live);
    end;
  end;

  CreateTrackerList_Dead;
end;

function TNewTrackon.SubmitTrackers(TrackerList: TStringList;
  out TrackersSendCount: integer): boolean;
var
  TrackerListToBeSend: TStringList;
  FormData: string;
  Trackers: string;
  HTTPS: TFPHTTPClient;
  Seperator: string;

const
  URL_POST = 'https://newtrackon.com/api/add';
begin
  TrackersSendCount := 0;

  //Must always first download the ALL tracker list if not already downloaded.
  //To make sure it is always checking agains the most recent list
  Result := DownloadTracker(ntl_URL_All);

  if Result then
  begin
    try
      HTTPS := TFPHTTPClient.Create(nil);
      TrackerListToBeSend := TStringList.Create;
      TrackerListToBeSend.Assign(TrackerList);

      //remove all duplicate trackers before sending,
      RemoveTrackersFromList(TrackerList_All, TrackerListToBeSend);

      //Give information back about how many unique tracker URL is send.
      TrackersSendCount := TrackerListToBeSend.Count;

      if TrackersSendCount > 0 then
      begin

        //this is the 'key'
        FormData := 'new_trackers=';

        //This is the 'values' all seperated with one space '+'
        Seperator := '';
        for Trackers in TrackerListToBeSend do
        begin
          FormData := FormData + Seperator + HTTPEncode(Trackers);
          if Seperator = '' then
            Seperator := '+';
        end;

        try
          HTTPS.FormPost(URL_POST, FormData);

          //Check the response must be 204
          Result := HTTPS.ResponseStatusCode = 204;
        except
          //No OpenSSL or web server is down
          Result := False;
        end;
      end;

    finally
      TrackerListToBeSend.Free;
      HTTPS.Free;
    end;
  end;
end;

constructor TNewTrackon.Create;
var
  i: TNewTrackon_List;
begin
  //Create all the TStringList
  for i in TNewTrackon_List do
  begin
    FTrackerList[i] := TStringList.Create;
    FTrackerList[i].Duplicates := dupIgnore;
  end;
end;

destructor TNewTrackon.Destroy;
var
  i: TNewTrackon_List;
begin
  //Release all the TStringList
  for i in TNewTrackon_List do
  begin
    FTrackerList[i].Free;
  end;

  inherited Destroy;
end;


end.
