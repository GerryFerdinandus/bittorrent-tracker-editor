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
    procedure Test_DownloadAPI;
  end;

implementation

procedure TTestNewTrackon.Test_DownloadAPI;
begin
  AssertTrue('Download the newtrackon API', FNewTrackon.DownloadTrackers);

  AssertTrue('TrackerList_All should never be empty',
    FNewTrackon.TrackerList_All.Count > 0);

  AssertTrue('TrackerList_Live should never be empty',
    FNewTrackon.TrackerList_Live.Count > 0);

  AssertTrue('TrackerList_Stable should never be empty',
    FNewTrackon.TrackerList_Stable.Count > 0);

  AssertTrue('TrackerList_Dead should never be empty',
    FNewTrackon.TrackerList_Dead.Count > 0);
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
