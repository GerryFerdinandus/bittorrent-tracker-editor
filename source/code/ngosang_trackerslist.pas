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
    ntl_URL_All_WS,//< Download from internet
    ntl_URL_Best,//< Download from internet
    ntl_URL_Best_IP//< Download from internet
    );

  { TngosangTrackerList }

  TngosangTrackerList = class
  private
    FTRackerList: array [Tngosang_List] of TStringList;
    function DownloadTracker(ngosang_List: Tngosang_List): TStringList;
  public

    property TrackerList_Blacklist: TStringList index ntl_URL_Blacklist
      read DownloadTracker;

    property TrackerList_All: TStringList index ntl_URL_All read DownloadTracker;

    property TrackerList_All_HTTP: TStringList index ntl_URL_All_HTTP
      read DownloadTracker;

    property TrackerList_All_HTTPS: TStringList index ntl_URL_All_HTTPS
      read DownloadTracker;

    property TrackerList_All_IP: TStringList index ntl_URL_All_IP read DownloadTracker;

    property TrackerList_All_UDP: TStringList index ntl_URL_All_UDP read DownloadTracker;

    property TrackerList_All_WS: TStringList index ntl_URL_All_WS read DownloadTracker;

    property TrackerList_Best: TStringList index ntl_URL_Best read DownloadTracker;

    property TrackerList_Best_IP: TStringList index ntl_URL_Best_IP read DownloadTracker;

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
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_ws.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt',
    'https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best_ip.txt'
    );

{ TngosangTrackerList }
function TngosangTrackerList.DownloadTracker(ngosang_List: Tngosang_List): TStringList;
begin
  try
    //download via URL and put the data in the TrackerList
    FTRackerList[ngosang_List].DelimitedText :=
      TFPCustomHTTPClient.SimpleGet(URL[ngosang_List]);

    //Clean up the tracker list
    SanatizeTrackerList(FTRackerList[ngosang_List]);

  except
    //No OpenSSL or web server is down
  end;

  Result := FTrackerList[ngosang_List];
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
