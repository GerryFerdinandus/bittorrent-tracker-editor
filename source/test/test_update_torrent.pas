// SPDX-License-Identifier: MIT
unit test_update_torrent;

{
  Update torrent files without any user interface.
  Torrent files are created in a temporary folder, so no test fixture file and
  no internet connection is needed.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, decodetorrent, torrent_miscellaneous,
  update_torrent;

type

  { TTestUpdateTorrent }

  TTestUpdateTorrent = class(TTestCase)
  private
    FTrackerList: TTrackerList;
    FDecodeTorrent: TDecodeTorrent;
    FTempFolder: string;

    //Write a torrent file with one file inside and the given trackers
    function CreateTorrentFile(const Name: string; TrackerList: array of string): string;

    //Write a file that is not valid bencode, so DecodeTorrent must fail on it
    function CreateCorruptTorrentFile(const Name: string): string;

    //Every torrent file is public and has no comment
    function DefaultFileSettingList: TTorrentFileSettingArray;

    procedure CheckTrackerListInFile(const FileName: string;
      Expected: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Sort_Writes_Same_List_In_All_Files;
    procedure Test_Append_After_Keeps_Original_Tracker_Of_Each_File;
    procedure Test_No_Trackers_Removes_Announce;
    procedure Test_ReadOnly_File_Is_Reported_And_Not_Changed;
    procedure Test_Undecodable_File_Is_Reported_And_Not_Changed;
    procedure Test_Private_Flag_Comment_And_SourceTag;
  end;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

const
  PIECES = '20:AAAAAAAAAAAAAAAAAAAA';
  INFO_ONE_FILE =
    'd6:lengthi1024e4:name8:test.bin12:piece lengthi16384e6:pieces' + PIECES + 'e';

  TRACKER_A = 'udp://a.test/announce';
  TRACKER_B = 'udp://b.test/announce';
  TRACKER_C = 'udp://c.test/announce';

function BEncodeString(const Str: UTF8String): UTF8String;
begin
  Result := IntToStr(Length(Str)) + ':' + Str;
end;

{
 Make a file read only, or writable again.

 Windows has a read only file attribute, Unix has not. FileSetAttr always fails
 on Unix, so the file permission must be used there. FPC reports faReadOnly for
 a Unix file when the owner has no write permission, and that is what the code
 under test is looking at.
}
function SetFileReadOnly(const FileName: string; ReadOnly: boolean): boolean;
{$IFDEF UNIX}
const
  //This test creates the files itself, so the permission can simply be set.
  MODE_READ_ONLY = &444;
  MODE_READ_WRITE = &644;
begin
  if ReadOnly then
    Result := FpChmod(FileName, MODE_READ_ONLY) = 0
  else
    Result := FpChmod(FileName, MODE_READ_WRITE) = 0;
{$ELSE}
var
  Attributes: longint;
begin
  Attributes := FileGetAttr(FileName);
  Result := Attributes <> -1;
  if not Result then
    Exit;

  if ReadOnly then
    Attributes := Attributes or faReadOnly
  else
    Attributes := Attributes and not faReadOnly;

  Result := FileSetAttr(FileName, Attributes) = 0;
{$ENDIF}
end;

{ TTestUpdateTorrent }

function TTestUpdateTorrent.CreateTorrentFile(const Name: string;
  TrackerList: array of string): string;
var
  TorrentStr: UTF8String;
  Stream: TFileStream;
  i: integer;
begin
  //Dictionary keys must be in alphabetical order: 'announce', 'announce-list', 'info'
  TorrentStr := 'd';
  if Length(TrackerList) > 0 then
  begin
    TorrentStr := TorrentStr + '8:announce' + BEncodeString(TrackerList[0]);
    TorrentStr := TorrentStr + '13:announce-listl';
    for i := Low(TrackerList) to High(TrackerList) do
      TorrentStr := TorrentStr + 'l' + BEncodeString(TrackerList[i]) + 'e';
    TorrentStr := TorrentStr + 'e';
  end;
  TorrentStr := TorrentStr + '4:info' + INFO_ONE_FILE + 'e';

  Result := FTempFolder + Name;

  //A read only file left behind by a crashed run would block fmCreate.
  if FileExists(Result) then
  begin
    SetFileReadOnly(Result, False);
    DeleteFile(Result);
  end;

  Stream := TFileStream.Create(Result, fmCreate);
  try
    Stream.Write(TorrentStr[1], Length(TorrentStr));
  finally
    Stream.Free;
  end;
end;

function TTestUpdateTorrent.CreateCorruptTorrentFile(const Name: string): string;
const
  //Not valid bencode: a dictionary must start with 'd' and end with 'e'
  CORRUPT_CONTENT = 'this is not bencode';
var
  Stream: TFileStream;
begin
  Result := FTempFolder + Name;

  if FileExists(Result) then
  begin
    SetFileReadOnly(Result, False);
    DeleteFile(Result);
  end;

  Stream := TFileStream.Create(Result, fmCreate);
  try
    Stream.Write(CORRUPT_CONTENT[1], Length(CORRUPT_CONTENT));
  finally
    Stream.Free;
  end;
end;

function TTestUpdateTorrent.DefaultFileSettingList: TTorrentFileSettingArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, FTrackerList.TorrentFileNameList.Count);
  for i := 0 to High(Result) do
  begin
    Result[i].PublicTorrent := True;
    Result[i].Comment := '';
  end;
end;

procedure TTestUpdateTorrent.CheckTrackerListInFile(const FileName: string;
  Expected: array of string);
var
  i: integer;
begin
  Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode ' + FileName);
  CheckEquals(Length(Expected), FDecodeTorrent.TrackerList.Count,
    'Wrong tracker count in ' + FileName);
  for i := Low(Expected) to High(Expected) do
  begin
    CheckEquals(Expected[i], FDecodeTorrent.TrackerList[i],
      Format('Wrong tracker at index %d in %s', [i, FileName]));
  end;
end;

procedure TTestUpdateTorrent.SetUp;
begin
  FTempFolder := IncludeTrailingPathDelimiter(GetTempDir) +
    'test_update_torrent' + PathDelim;
  ForceDirectories(FTempFolder);

  FDecodeTorrent := TDecodeTorrent.Create;

  FTrackerList.TorrentFileNameList := TStringList.Create;
  FTrackerList.TrackerFinalList := TStringList.Create;
  FTrackerList.TrackerAddedByUserList := TStringList.Create;
  FTrackerList.TrackerBanByUserList := TStringList.Create;
  FTrackerList.TrackerFromInsideTorrentFilesList := TStringList.Create;
  FTrackerList.TrackerManuallyDeselectedByUserList := TStringList.Create;
  FTrackerList.LogStringList := TStringList.Create;

  FTrackerList.SkipAnnounceCheck := False;
  FTrackerList.SourceTag := '';
  FTrackerList.RemoveAllSourceTag := False;
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
end;

procedure TTestUpdateTorrent.TearDown;
var
  FileName: string;
begin
  //Windows can not delete a read only file, so clear the flag first.
  for FileName in FTrackerList.TorrentFileNameList do
  begin
    SetFileReadOnly(FileName, False);
    DeleteFile(FileName);
  end;
  RemoveDir(FTempFolder);

  FTrackerList.TorrentFileNameList.Free;
  FTrackerList.TrackerFinalList.Free;
  FTrackerList.TrackerAddedByUserList.Free;
  FTrackerList.TrackerBanByUserList.Free;
  FTrackerList.TrackerFromInsideTorrentFilesList.Free;
  FTrackerList.TrackerManuallyDeselectedByUserList.Free;
  FTrackerList.LogStringList.Free;
  FDecodeTorrent.Free;
end;

procedure TTestUpdateTorrent.Test_Sort_Writes_Same_List_In_All_Files;
var
  UpdateResult: TUpdateTorrentResult;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent', [TRACKER_C]));
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', []));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_B);
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;

  //The caller must always combine with tloSort first
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckEquals(2, UpdateResult.FilesUpdated, 'Both torrent files must be updated');
  CheckEquals(2, UpdateResult.TrackerCount, 'Wrong tracker count');
  CheckFalse(UpdateResult.SomeFilesAreReadOnly, 'No file is read only');
  CheckFalse(UpdateResult.SomeFilesCannotBeWritten, 'Every file must be written');

  //tloSort ignores the trackers already inside the torrent file
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0], [TRACKER_A, TRACKER_B]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_A, TRACKER_B]);
end;

procedure TTestUpdateTorrent.Test_Append_After_Keeps_Original_Tracker_Of_Each_File;
var
  UpdateResult: TUpdateTorrentResult;
begin
  //Every torrent file has its own original tracker list
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent', [TRACKER_B]));
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_C]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing;

  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckEquals(2, UpdateResult.FilesUpdated, 'Both torrent files must be updated');

  //The original tracker of each file stays first, the new one is appended
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0], [TRACKER_B, TRACKER_A]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_C, TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_No_Trackers_Removes_Announce;
var
  UpdateResult: TUpdateTorrentResult;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent',
    [TRACKER_A, TRACKER_B]));

  //No tracker is added and nothing is kept from inside the torrent
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckEquals(0, UpdateResult.TrackerCount, 'There must be no tracker left');
  CheckEquals(1, UpdateResult.FilesUpdated, 'The torrent file must be updated');

  //'announce' and 'announce-list' must both be gone
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0], []);
end;

procedure TTestUpdateTorrent.Test_ReadOnly_File_Is_Reported_And_Not_Changed;
var
  UpdateResult: TUpdateTorrentResult;
  ReadOnlyFile: string;
begin
  ReadOnlyFile := CreateTorrentFile('readonly.torrent', [TRACKER_C]);
  FTrackerList.TorrentFileNameList.Add(ReadOnlyFile);
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_C]));

  CheckTrue(SetFileReadOnly(ReadOnlyFile, True),
    'Can not make the torrent file read only');

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckTrue(UpdateResult.SomeFilesAreReadOnly, 'Read only file must be reported');
  CheckEquals(1, UpdateResult.FilesUpdated, 'Only one file can be updated');

  //The read only file must still have its original tracker
  CheckTrackerListInFile(ReadOnlyFile, [TRACKER_C]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_Undecodable_File_Is_Reported_And_Not_Changed;
var
  UpdateResult: TUpdateTorrentResult;
  CorruptFile: string;
begin
  CorruptFile := CreateCorruptTorrentFile('corrupt.torrent');
  FTrackerList.TorrentFileNameList.Add(CorruptFile);
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_C]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckTrue(UpdateResult.SomeFilesCanNotBeDecoded, 'Undecodable file must be reported');
  CheckEquals(1, UpdateResult.FilesUpdated, 'Only the valid file can be updated');

  //The other, valid, file must still be updated normally
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_Private_Flag_Comment_And_SourceTag;
var
  FileSettingList: TTorrentFileSettingArray;
  UpdateResult: TUpdateTorrentResult;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent', [TRACKER_A]));
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_A]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.SourceTag := 'SOURCE_TAG';
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  FileSettingList := DefaultFileSettingList;
  FileSettingList[0].PublicTorrent := False;
  FileSettingList[0].Comment := 'a private torrent';
  FileSettingList[1].PublicTorrent := True;
  FileSettingList[1].Comment := 'a public torrent';

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent, FileSettingList);
  CheckEquals(2, UpdateResult.FilesUpdated, 'Both torrent files must be updated');

  Check(FDecodeTorrent.DecodeTorrent(FTrackerList.TorrentFileNameList[0]),
    'Can not decode the private torrent');
  CheckTrue(FDecodeTorrent.PrivateTorrent, 'Torrent must be private');
  CheckEquals('a private torrent', FDecodeTorrent.Comment, 'Wrong comment');
  CheckEquals('SOURCE_TAG', FDecodeTorrent.InfoSource, 'Wrong info source');

  Check(FDecodeTorrent.DecodeTorrent(FTrackerList.TorrentFileNameList[1]),
    'Can not decode the public torrent');
  CheckFalse(FDecodeTorrent.PrivateTorrent, 'Torrent must be public');
  CheckEquals('a public torrent', FDecodeTorrent.Comment, 'Wrong comment');
  CheckEquals('SOURCE_TAG', FDecodeTorrent.InfoSource, 'Wrong info source');
end;

initialization
  RegisterTest(TTestUpdateTorrent);
end.