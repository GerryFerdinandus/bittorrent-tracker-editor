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
    FDownloadStatus: boolean;

    procedure DownloadTracker(NewTrackon_List: TNewTrackon_List);
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

    //Download all the trackers via API
    function DownloadTrackers: boolean;

    //create/destroy class object
    constructor Create;
    destructor Destroy; override;
  end;



implementation

uses fphttpclient, LazUTF8, torrent_miscellaneous;

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

procedure TNewTrackon.DownloadTracker(NewTrackon_List: TNewTrackon_List);
begin
  if NewTrackon_List = ntl_CREATE_DEAD then
    Exit; //there is no Dead tracker list to be downloaded

  //download via URL and put the data in the TrackerList
  //will create exception if something is wrong
  FTRackerList[NewTrackon_List].DelimitedText :=
    TFPCustomHTTPClient.SimpleGet(URL[NewTrackon_List]);

  //Clean up the tracker list
  SanatizeTrackerList(FTRackerList[NewTrackon_List]);
end;

function TNewTrackon.DownloadTrackers: boolean;
var
  i: TNewTrackon_List;
begin
  try
    //download all the list one by one
    for i in TNewTrackon_List do
    begin
      DownloadTracker(i);//< may create exception
    end;

    CreateTrackerList_Dead;

    FDownloadStatus := True;
  except
    //No OpenSSL or web server is down
    FDownloadStatus := False;
  end;

  Result := FDownloadStatus;
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
