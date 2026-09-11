// SPDX-License-Identifier: MIT
unit controllergridtorrentdata;

{
The view string grid shows all the information of the torrent.
The grid column position order can be rearrange by the user.
The updating and reading of the column position must be 'dynamic'.
Must keep track of the position of the column even when the user rearrange it.

There are 10 column that must be 'track'
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type

  { TControllerGridTorrentData }

  TControllerGridTorrentData = class
  private
    //The view grid that must be controled.
    FStringGridTorrentData: TStringGrid;

    //The column must be in this design order.
    FTorrentFile,           //0
    FInfoFileName,          //1
    FTorrentVersion,        //2
    FPadding,               //3
    FInfoHash,              //4
    FCreatedOn,             //5
    FCreatedBy,             //6
    FComment,               //7
    FPrivateTorrent,        //8
    FInfoSource,            //9
    FPieceLength,           //10
    FTotalSize,              //11
    FIndexOrder             //12
    : TGridColumn;

    FRowIsMovedNeedUpdate: boolean;
    procedure StringGridTorrentDataColRowMoved(Sender: TObject;
      {%H-}IsColumn: boolean; {%H-}sIndex, {%H-}tIndex: integer);
    procedure AddColumn(var GridColumn: TGridColumn; index: integer);
    procedure UpdateColumnTag;
    procedure WriteCell(GridColumn: TGridColumn; const Str: UTF8String);
  public
    //All the string that can be written to grid.
    TorrentFile,           //0
    InfoFileName,          //1
    TorrentVersion,        //2
    Padding,               //3
    InfoHash,              //4
    CreatedOn,             //5
    CreatedBy,             //6
    Comment,               //7
    PrivateTorrent,        //8
    InfoSource,            //9
    PieceLength,           //10
    TotalSize,              //11
    IndexOrder             //12
    : UTF8String;

    procedure ClearAllImageIndex;
    procedure AppendRow;
    procedure ReorderGrid;
    function ReadComment(Rowindex: integer): UTF8String;
    constructor Create(StringGridTorrentData: TStringGrid);
    destructor Destroy; override;
  end;

implementation

{ TControllerGridTorrentData }
const
  COLUMN_COUNT = 13;

procedure TControllerGridTorrentData.StringGridTorrentDataColRowMoved(Sender: TObject;
  IsColumn: boolean; sIndex, tIndex: integer);
begin
  //This is called before the column is moved 'rearrange' by the user.
  FRowIsMovedNeedUpdate := True;
end;

procedure TControllerGridTorrentData.AddColumn(var GridColumn: TGridColumn;
  index: integer);
begin
  GridColumn := FStringGridTorrentData.Columns[index];
end;

procedure TControllerGridTorrentData.UpdateColumnTag;
var
  i: integer;
begin
  //fill the 'tag' value as the position of the column.
  //this method must be only called when the user change the column order.
  for i := 0 to FStringGridTorrentData.Columns.Count - 1 do
  begin
    FStringGridTorrentData.Columns[i].Tag :=
      FStringGridTorrentData.Columns.IndexOf(FStringGridTorrentData.Columns[i]);
  end;

  //The tag is now in sync column index. It is process.
  FRowIsMovedNeedUpdate := False;
end;

procedure TControllerGridTorrentData.ClearAllImageIndex;
var
  i: integer;
begin
  //The sort icon must be removed from the title bar
    for i := 0 to FStringGridTorrentData.Columns.Count - 1 do
  begin
    FStringGridTorrentData.Columns[i].Title.ImageIndex := -1;
  end;
end;

procedure TControllerGridTorrentData.WriteCell(GridColumn: TGridColumn;
  const Str: UTF8String);
begin
  FStringGridTorrentData.Cells[GridColumn.Tag,
    FStringGridTorrentData.RowCount - 1] := Str;
end;

procedure TControllerGridTorrentData.AppendRow;
begin
  //Add a new empty row, copy all the stings to this empty row.

  //Update Column.tag if row have change.
  if FRowIsMovedNeedUpdate then
    UpdateColumnTag;

  //Create a empty row to at the bottom.
  FStringGridTorrentData.InsertColRow(False, FStringGridTorrentData.RowCount);

  //write all the string to the cell.
  WriteCell(FTorrentFile, TorrentFile);
  WriteCell(FInfoFileName, InfoFileName);
  WriteCell(FTorrentVersion, TorrentVersion);
  WriteCell(FPadding, Padding);
  WriteCell(FInfoHash, InfoHash);
  WriteCell(FCreatedOn, CreatedOn);
  WriteCell(FCreatedBy, CreatedBy);
  WriteCell(FComment, Comment);
  WriteCell(FPrivateTorrent, PrivateTorrent);
  WriteCell(FInfoSource, InfoSource);
  WriteCell(FPieceLength, PieceLength);
  WriteCell(FTotalSize, TotalSize);
  WriteCell(FIndexOrder, IndexOrder);

end;

procedure TControllerGridTorrentData.ReorderGrid;
begin
  //Undo all possible sort column used by the user. Sort it back to 'begin state'
  //FIndexOrder is a non visible row use for this purpose. TGridColumnTitle
  FStringGridTorrentData.SortOrder := soAscending;
  FStringGridTorrentData.SortColRow(True,
    FStringGridTorrentData.Columns.IndexOf(FIndexOrder));

  //The order are no longer the same. Hide the ascending/decending icon.
  ClearAllImageIndex;
end;

function TControllerGridTorrentData.ReadComment(Rowindex: integer): UTF8String;
begin
  //Update Column.tag if row have change.
  if FRowIsMovedNeedUpdate then
    UpdateColumnTag;

  //Read the 'comment' grid cell.
  Result := FStringGridTorrentData.Cells[FComment.Tag, Rowindex];
end;

constructor TControllerGridTorrentData.Create(StringGridTorrentData: TStringGrid);
begin
  inherited Create;
  FStringGridTorrentData := StringGridTorrentData;

  //When user move the row, call StringGridTorrentDataColRowMoved.
  FStringGridTorrentData.OnColRowMoved := @StringGridTorrentDataColRowMoved;

  //The view and the controller part must have the same column count.
  Assert(FStringGridTorrentData.Columns.Count = COLUMN_COUNT, 'Wrong column count');

  //Track the column
  AddColumn(FTorrentFile, 0);
  AddColumn(FInfoFileName, 1);
  AddColumn(FTorrentVersion, 2);
  AddColumn(FPadding, 3);
  AddColumn(FInfoHash, 4);
  AddColumn(FCreatedOn, 5);
  AddColumn(FCreatedBy, 6);
  AddColumn(FComment, 7);
  AddColumn(FPrivateTorrent, 8);
  AddColumn(FInfoSource, 9);
  AddColumn(FPieceLength, 10);
  AddColumn(FTotalSize, 11);
  AddColumn(FIndexOrder, 12);

  //Fillin the tag value
  UpdateColumnTag;
end;

destructor TControllerGridTorrentData.Destroy;
begin
  inherited Destroy;
end;

end.
