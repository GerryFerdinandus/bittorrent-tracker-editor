unit ngosang_trackerslist;

{
Use api from ngosang to get tracker list that are working or not.
see: https://github.com/ngosang/trackerslist
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type


  { TNewTrackon }

  //All the type of tracker list.
  Tngosang_List = (
    ntl_URL_Blacklist,//< Download from internet
    ntl_URL_All,//< Download from internet
    ntl_URL_All_HTTP,//< Download from internet
    ntl_URL_All_HTTPS,//< Download from internet
    ntl_URL_All_IP,//< Download from internet
    ntl_URL_All_UDP,//< Download from internet
    ntl_URL_Best,//< Download from internet
    ntl_URL_Best_IP//< Download from internet
    );

  { TngosangTrackerList }

  TngosangTrackerList = class
  private
    FTRackerList: array [Tngosang_List] of TStringList;

    procedure DownloadTracker(ngosang_List: Tngosang_List);

  public

    property TrackerList_Blacklist: TStringList read FTRackerList[ntl_URL_Blacklist];

    property TrackerList_All: TStringList read FTrackerList[ntl_URL_All];

    property TrackerList_All_HTTP: TStringList read FTrackerList[ntl_URL_All_HTTP];

    property TrackerList_All_HTTPS: TStringList read FTrackerList[ntl_URL_All_HTTPS];

    property TrackerList_All_IP: TStringList read FTrackerList[ntl_URL_All_IP];

    property TrackerList_All_UDP: TStringList read FTrackerList[ntl_URL_All_UDP];

    property TrackerList_Best: TStringList read FTrackerList[ntl_URL_Best];

    property TrackerList_Best_IP: TStringList read FTrackerList[ntl_URL_Best_IP];

    //Download all the trackers via API
    function DownloadTrackers: boolean;

    //create/destroy class object
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses fphttpclient, LazUTF8, torrent_miscellaneous;

const
  URL: array [Tngosang_List] of string =
    (//Warning: the URL strings must be in the same order as Tngosang_List
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/blacklist.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_http.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_https.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_ip.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_udp.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best_ip.txt'
    );

{ TngosangTrackerList }
procedure TngosangTrackerList.DownloadTracker(ngosang_List: Tngosang_List);
begin
  //download via URL and put the data in the TrackerList
  FTRackerList[ngosang_List].DelimitedText :=
    TFPCustomHTTPClient.SimpleGet(URL[ngosang_List]);

  //Clean up the tracker list
  SanatizeTrackerList(FTRackerList[ngosang_List]);
end;


function TngosangTrackerList.DownloadTrackers: boolean;
var
  i: Tngosang_List;
begin
  try
    //download all the list one by one
    for i in Tngosang_List do
    begin
      DownloadTracker(i);
    end;

    Result := True;
  except
    //No OpenSSL or web server is down
    Result := False;
  end;
end;

constructor TngosangTrackerList.Create;
var
  i: Tngosang_List;
begin
  //Create all the TStringList
  for i in Tngosang_List do
  begin
    FTrackerList[i] := TStringList.Create;
    FTrackerList[i].Duplicates := dupIgnore;
  end;
end;

destructor TngosangTrackerList.Destroy;
var
  i: Tngosang_List;
begin
  //Release all the TStringList
  for i in Tngosang_List do
  begin
    FTrackerList[i].Free;
  end;

  inherited Destroy;
end;

end.

