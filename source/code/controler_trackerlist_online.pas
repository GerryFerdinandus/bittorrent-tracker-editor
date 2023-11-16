// SPDX-License-Identifier: MIT
unit controler_trackerlist_online;

{
  Show the present status of the trackerURL via TStringGrid
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, trackerlist_online, newtrackon;

type

  TDefaultChecked = function(const TrackerURL: utf8string): boolean of object;

  { TControlerTrackerListOnline }

  TControlerTrackerListOnline = class
  private
    FStringGridTorrentURL: TStringGrid;
    FNewTrackon: TNewTrackon;
    FTrackerListOnline: TTrackerListOnline;
    FTrackerList: TStringList;
    FDefaultChecked: TDefaultChecked;

    //The collumn must be in this design order.
    FSelect,               //< 0
    FTorrentURL,           //< 1
    FTorrentURL_Status     //< 2
    : TGridColumn;

    function GetChecked(index: integer): boolean;
    procedure SetChecked(index: integer; AValue: boolean);
    procedure ShowTrackerStatus(Visible: boolean);
    procedure AppendRow(Checked: boolean; Status: TTrackerListOnlineStatus;
      const TrackerURL: utf8string);
  public
    property Checked[index: integer]: boolean read GetChecked write SetChecked;
    function TrackerURL(index: integer): string;
    function TrackerStatus(index: integer): TTrackerListOnlineStatus;
    function Count: integer;
    function StableTrackers: TStringList;

    //must be called if the tracker list is updated
    procedure UpdateView;

    function DownloadTrackers_All_Live_Stable: boolean;

    //Submit tracker to newTrackon via http POST
    function SubmitTrackers(TrackerList: TStringList;
      out TrackersSendCount: integer): boolean;

    constructor Create(StringGridTorrentURL: TStringGrid; TrackerList: TStringList;
      DefaultChecked: TDefaultChecked);
    destructor Destroy; override;
  end;


implementation

uses Graphics;

{ TControlerTrackerListOnline }


function IsDarkTheme: boolean;
  // by "Hansaplast" & "Alextp" from Lazarus forum
  function _Level(C: TColor): double;
  begin
    Result := Red(C) * 0.3 + Green(C) * 0.59 + Blue(C) * 0.11;
  end;

begin
  Result := _Level(ColorToRGB(clWindow)) < _Level(ColorToRGB(clWindowText));
end;

function TControlerTrackerListOnline.DownloadTrackers_All_Live_Stable: boolean;
begin
  Result := FNewTrackon.Download_All_Live_Stable;
  UpdateView;
  ShowTrackerStatus(Result);
end;

function TControlerTrackerListOnline.SubmitTrackers(TrackerList: TStringList;
  out TrackersSendCount: integer): boolean;
begin
  Result := FNewTrackon.SubmitTrackers(TrackerList, TrackersSendCount);
end;

constructor TControlerTrackerListOnline.Create(StringGridTorrentURL: TStringGrid;
  TrackerList: TStringList; DefaultChecked: TDefaultChecked);
begin

  FTrackerList := TrackerList;
  FDefaultChecked := DefaultChecked;

  FStringGridTorrentURL := StringGridTorrentURL;
  FStringGridTorrentURL.RowCount := 1;
  FStringGridTorrentURL.FixedRows := 1;

  if not IsDarkTheme then
  begin // The dark theme for Linux and macOS cannot use AlternateColor. Text will be invisible.
    FStringGridTorrentURL.AlternateColor := clCream;
  end;

  FSelect := FStringGridTorrentURL.Columns.Add;
  FSelect.Title.Caption := 'Keep';
  FSelect.ButtonStyle := cbsCheckboxColumn;
  FSelect.ReadOnly := False;


  FTorrentURL_Status := FStringGridTorrentURL.Columns.Add;
  FTorrentURL_Status.Title.Caption := 'Online status';
  ShowTrackerStatus(False);
  FTorrentURL_Status.ReadOnly := True;

  FTorrentURL := FStringGridTorrentURL.Columns.Add;
  FTorrentURL.Title.Caption := 'Tracker URL';
  FTorrentURL.ReadOnly := True;

  //make sure all text are fit inside the columns
  FStringGridTorrentURL.AutoSizeColumns;

  FNewTrackon := TNewTrackon.Create;
  FTrackerListOnline := TTrackerListOnline.Create;

  //copy tracker list from TNewTrackon to TTrackerListOnline
  FTrackerListOnline.TrackerList_Live := FNewTrackon.TrackerList_Live;
  FTrackerListOnline.TrackerList_Dead := FNewTrackon.TrackerList_Dead;
  FTrackerListOnline.TrackerList_Stable := FNewTrackon.TrackerList_Stable;
end;

destructor TControlerTrackerListOnline.Destroy;
begin
  FTrackerListOnline.Free;
  FNewTrackon.Free;
  inherited Destroy;
end;

procedure TControlerTrackerListOnline.ShowTrackerStatus(Visible: boolean);
begin
  FTorrentURL_Status.Visible := Visible;
end;

function TControlerTrackerListOnline.GetChecked(index: integer): boolean;
begin
  //read the select checkbox. If '1' then it is True
  Result := FStringGridTorrentURL.Cells[0, index +
    FStringGridTorrentURL.FixedRows] = '1';
end;

procedure TControlerTrackerListOnline.SetChecked(index: integer; AValue: boolean);
begin
  FStringGridTorrentURL.Cells[0, index + FStringGridTorrentURL.FixedRows] :=
    BoolToStr(AValue, '1', '0');
end;


procedure TControlerTrackerListOnline.AppendRow(Checked: boolean;
  Status: TTrackerListOnlineStatus; const TrackerURL: utf8string);
var
  CheckedStr, StatusStr: string;
begin
  CheckedStr := BoolToStr(Checked, '1', '0');
  StatusStr := FTrackerListOnline.TrackerListOnlineStatusToString(Status);

  //append to the end of the view list
  FStringGridTorrentURL.InsertRowWithValues(FStringGridTorrentURL.RowCount,
    [CheckedStr, StatusStr, TrackerURL]);
end;

function TControlerTrackerListOnline.TrackerURL(index: integer): string;
begin
  Result := FStringGridTorrentURL.Cells[2, index + FStringGridTorrentURL.FixedRows];
end;

function TControlerTrackerListOnline.TrackerStatus(index: integer):
TTrackerListOnlineStatus;
begin
  Result := FTrackerListOnline.TrackerStatus(TrackerURL(index));
end;

function TControlerTrackerListOnline.Count: integer;
begin
  Result := FStringGridTorrentURL.RowCount - FStringGridTorrentURL.FixedRows;
end;

function TControlerTrackerListOnline.StableTrackers: TStringList;
begin
  Result := FNewTrackon.TrackerList_Stable;
end;

procedure TControlerTrackerListOnline.UpdateView;
var
  tracker: string;
begin
  //Clear all the previeus data in the view
  FStringGridTorrentURL.RowCount := FStringGridTorrentURL.FixedRows;

  FStringGridTorrentURL.BeginUpdate;

  //Show the TrackerList list in string grid view
  for tracker in FTrackerList do
  begin
    AppendRow(FDefaultChecked(tracker), FTrackerListOnline.TrackerStatus(
      tracker), tracker);
  end;

  //make sure all text are fit inside the columns
  FStringGridTorrentURL.AutoSizeColumns;

  FStringGridTorrentURL.EndUpdate;
end;

end.
