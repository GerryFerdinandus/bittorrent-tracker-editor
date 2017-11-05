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
  end;

implementation

procedure TTestNewTrackon.Test_API_Download;
begin
  Check(FNewTrackon.DownloadEverything, 'Download the newtrackon API');

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
    Check(FNewTrackon.SubmitTrackers(TrackerList, TrackersSendCount),
      'Upload the newtrackon API');
    Check(TrackersSendCount <= TrackerList.Count, 'TrackersSendCount have too high value');
  finally
    TrackerList.Free;
  end;

end;

procedure TTestNewTrackon.SetUp;
begin
  FNewTrackon := TNewTrackon.Create;
end;

procedure TTestNewTrackon.TearDown;
begin
  FNewTrackon.Free;
end;

initialization

  RegisterTest(TTestNewTrackon);
end.
