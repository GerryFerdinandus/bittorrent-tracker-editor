// SPDX-License-Identifier: MIT
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

uses opensslsockets, OpenSSL, fphttpclient, LazUTF8, torrent_miscellaneous;

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

initialization
  // FPC 3.2.2 is missing support for the latest openSSL 3, will be fix in the future release.
  // Latest openssl.pas https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/openssl/src/openssl.pas?ref_type=heads
  // Copy this newer SSL detection into the older openssl code used by the present FPC 3.2.2
{$IFDEF VER3_2}
{$IFDEF WINDOWS}
  DLLSSLName3 := {$IFDEF WIN64}'libssl-3-x64.dll'{$ELSE}'libssl-3.dll'{$ENDIF};
  DLLUtilName2 := {$IFDEF WIN64}'libcrypto-3-x64.dll'{$ELSE}'libcrypto-3.dll'{$ENDIF};
{$ELSE WINDOWS}
{$IFDEF DARWIN}
  if High(OpenSSL.DLLVersions) >= 19 then
  begin
    // macOS version
    // LibreSSL
    OpenSSL.DLLVersions[1] := '.48';
    OpenSSL.DLLVersions[2] := '.47';
    OpenSSL.DLLVersions[3] := '.46';
    OpenSSL.DLLVersions[4] := '.45';
    OpenSSL.DLLVersions[5] := '.44';
    OpenSSL.DLLVersions[6] := '.43';
    OpenSSL.DLLVersions[7] := '.35';

    // OpenSSL
    OpenSSL.DLLVersions[8] := '.3';
    OpenSSL.DLLVersions[9] := '.1.1';
    OpenSSL.DLLVersions[10] := '.11';
    OpenSSL.DLLVersions[11] := '.10';
    OpenSSL.DLLVersions[12] := '.1.0.6';
    OpenSSL.DLLVersions[13] := '.1.0.5';
    OpenSSL.DLLVersions[14] := '.1.0.4';
    OpenSSL.DLLVersions[15] := '.1.0.3';
    OpenSSL.DLLVersions[16] := '.1.0.2';
    OpenSSL.DLLVersions[17] := '.1.0.1';
    OpenSSL.DLLVersions[18] := '.1.0.0';
    OpenSSL.DLLVersions[19] := '.0.9.8';
  end;
{$ElSE DARWIN}
  // Unix/Linux version of FPC need openSSL 3 in the detection list
  OpenSSL.DLLVersions[Length(OpenSSL.DLLVersions) - 1] := '.3';
{$ENDIF DARWIN}
{$ENDIF WINDOWS}
{$ENDIF VER3_2}

end.
