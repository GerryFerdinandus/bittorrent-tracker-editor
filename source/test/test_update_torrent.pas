// SPDX-License-Identifier: MIT
unit test_update_torrent;

{
  Update torrent files without any user interface.
  Torrent files are created in a temporary folder, so no test fixture file and
  no internet connection is needed.
}

{$mode objfpc}{$H+}
//Needed because this unit has non-Latin string literals in source; without it FPC tags
//them with the default AnsiString codepage and double-encodes them on assignment to
//UTF8String. Not needed in trackereditor/trackereditor_cli: their non-Latin text is always
//runtime data (file bytes, ParamStr, LCL widgets), never a compiled string literal.
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, decodetorrent, torrent_miscellaneous,
  update_torrent, main_common;

type

  { TTestUpdateTorrent }

  TTestUpdateTorrent = class(TTestCase)
  private
    FTrackerList: TTrackerList;
    FDecodeTorrent: TDecodeTorrent;
    FTempFolder: string;

    //Write a torrent file with one file inside and the given trackers
    function CreateTorrentFile(const Name: string; TrackerList: array of string): string;

    //Write a torrent file at an explicit full path, so it can be placed in any folder
    function CreateTorrentFileAt(const FullPath: string; TrackerList: array of string): string;

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
    procedure Test_NonLatin_Torrent_FileName;
    procedure Test_NonLatin_Folder_Path;
    procedure Test_RemoveNothing_Keeps_Ban_Lists_Intact;
    procedure Test_Folder_Name_With_Dot_Is_A_Folder;
    procedure Test_Missing_File_Is_Reported_As_Undecodable_Not_ReadOnly;
    procedure Test_SanitizeTrackerList_Removes_Comments_And_Spaces;
    procedure Test_InvalidTrackerURLMessage_Lists_All_Valid_Prefixes;
    procedure Test_ValidateNewTrackerLines_Cleans_And_Ignores_Duplicates;
    procedure Test_ValidateNewTrackerLines_Rejects_Unknown_Scheme;
    procedure Test_ValidateNewTrackerLines_Announce_Check;
    procedure Test_ValidateNewTrackerLines_Replaces_Previous_Result;
    procedure Test_ReadAddTrackersFile_Unreadable_Keeps_Lines;
    procedure Test_LoadAddTrackersRaw_Without_File_Uses_Recommended_Trackers;
    procedure Test_LoadRemoveTrackers_Present_And_Missing;
    procedure Test_LoadTorrentViaDir_Uppercase_Extension_And_Skips_Folders;
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
begin
  Result := CreateTorrentFileAt(FTempFolder + Name, TrackerList);
end;

function TTestUpdateTorrent.CreateTorrentFileAt(const FullPath: string;
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

  Result := FullPath;

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

procedure TTestUpdateTorrent.Test_NonLatin_Torrent_FileName;
var
  UpdateResult: TUpdateTorrentResult;
begin
  //CJK + Cyrillic torrent file name, to verify Unicode file names are read/written correctly.
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('测试_трекер.torrent', [TRACKER_C]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckEquals(1, UpdateResult.FilesUpdated, 'The torrent file must be updated');
  CheckFalse(UpdateResult.SomeFilesCannotBeWritten, 'File must be written');

  //The original tracker stays first, the new one is appended
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0], [TRACKER_C, TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_NonLatin_Folder_Path;
var
  NonLatinFolder, TorrentFileName: string;
  FoundFiles: TStringList;
begin
  //CJK + Cyrillic folder name, to verify a torrent inside it is found and decoded.
  //Cleaned up locally, this folder is not part of SetUp/TearDown.
  NonLatinFolder := FTempFolder + '文件夹_папка' + PathDelim;
  ForceDirectories(NonLatinFolder);
  TorrentFileName := CreateTorrentFileAt(NonLatinFolder + 'one.torrent', [TRACKER_A]);
  FoundFiles := TStringList.Create;
  try
    Check(LoadTorrentViaDir(NonLatinFolder, FoundFiles),
      'Can not find torrent files inside the non-Latin folder');
    CheckEquals(1, FoundFiles.Count, 'Wrong torrent file count in non-Latin folder');

    Check(FDecodeTorrent.DecodeTorrent(FoundFiles[0]),
      'Can not decode torrent found inside a non-Latin folder');
  finally
    FoundFiles.Free;
    DeleteFile(TorrentFileName);
    RemoveDir(NonLatinFolder);
  end;
end;

procedure TTestUpdateTorrent.Test_RemoveNothing_Keeps_Ban_Lists_Intact;
var
  Present: TStringList;
begin
  Present := TStringList.Create;
  try
    Present.Add(TRACKER_A);
    Present.Add(TRACKER_B);
    FTrackerList.TrackerAddedByUserList.Add(TRACKER_C);
    FTrackerList.TrackerBanByUserList.Add(TRACKER_A);
    FTrackerList.TrackerManuallyDeselectedByUserList.Add(TRACKER_B);

    CombineFiveTrackerListToOne(
      tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing, FTrackerList, Present);
    CheckEquals(3, FTrackerList.TrackerFinalList.Count, 'Nothing may be removed');
    CheckEquals(1, FTrackerList.TrackerBanByUserList.Count,
      'The ban list of the caller must not be cleared');
    CheckEquals(1, FTrackerList.TrackerManuallyDeselectedByUserList.Count,
      'The deselected list of the caller must not be cleared');

    //The same lists must still remove trackers in a mode that removes
    CombineFiveTrackerListToOne(tloAppendNewAfterAndKeepNewIntact, FTrackerList, Present);
    CheckEquals(1, FTrackerList.TrackerFinalList.Count, 'Banned trackers must be removed');
    CheckEquals(TRACKER_C, FTrackerList.TrackerFinalList[0], 'Wrong remaining tracker');
  finally
    Present.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_Folder_Name_With_Dot_Is_A_Folder;
var
  DottedFolder: string;
begin
  DottedFolder := FTempFolder + 'My.Torrents';
  ForceDirectories(DottedFolder);
  try
    CheckTrue(PathIsTorrentFolder(DottedFolder), 'Existing folder with a dot is a folder');
    CheckTrue(PathIsTorrentFolder(FTempFolder + 'no_such_folder'),
      'A path without extension is treated as a folder');
    CheckFalse(PathIsTorrentFolder(FTempFolder + 'missing.torrent'),
      'A missing .torrent path is a file');
  finally
    RemoveDir(DottedFolder);
  end;

  CheckTrue(PathIsTorrentFile('a.torrent'), 'Wrong .torrent detection');
  CheckTrue(PathIsTorrentFile('A.TORRENT'), '.torrent must be case insensitive');
  CheckFalse(PathIsTorrentFile('a.txt'), 'A .txt is not a torrent');
  CheckFalse(PathIsTorrentFile('a.torrent.bak'), 'A .bak is not a torrent');
end;

procedure TTestUpdateTorrent.Test_LoadTorrentViaDir_Uppercase_Extension_And_Skips_Folders;
var
  Folder, UpperFile, LowerFile, OtherFile, SubFolder: string;
  FoundFiles: TStringList;
begin
  Folder := FTempFolder + 'scan' + PathDelim;
  ForceDirectories(Folder);
  SubFolder := Folder + 'folder.torrent';
  ForceDirectories(SubFolder);
  UpperFile := CreateTorrentFileAt(Folder + 'UPPER.TORRENT', [TRACKER_A]);
  LowerFile := CreateTorrentFileAt(Folder + 'lower.torrent', [TRACKER_A]);
  OtherFile := CreateTorrentFileAt(Folder + 'other.txt', [TRACKER_A]);
  FoundFiles := TStringList.Create;
  try
    Check(LoadTorrentViaDir(Folder, FoundFiles), 'Can not find the torrent files');
    CheckEquals(2, FoundFiles.Count,
      'Must find the upper and lower case torrent file, not the folder or the .txt');
  finally
    FoundFiles.Free;
    DeleteFile(UpperFile);
    DeleteFile(LowerFile);
    DeleteFile(OtherFile);
    RemoveDir(SubFolder);
    RemoveDir(Folder);
  end;
end;

procedure TTestUpdateTorrent.Test_Missing_File_Is_Reported_As_Undecodable_Not_ReadOnly;
var
  UpdateResult: TUpdateTorrentResult;
begin
  //The file was deleted after it was loaded. FileGetAttr returns -1, that is not 'read only'.
  FTrackerList.TorrentFileNameList.Add(FTempFolder + 'missing.torrent');
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_C]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodeTorrent,
    DefaultFileSettingList);

  CheckFalse(UpdateResult.SomeFilesAreReadOnly, 'A missing file is not read only');
  CheckTrue(UpdateResult.SomeFilesCanNotBeDecoded, 'A missing file must be reported');
  CheckEquals(1, UpdateResult.FilesUpdated, 'Only the existing file can be updated');
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_SanitizeTrackerList_Removes_Comments_And_Spaces;
var
  Lines: TStringList;
begin
  //A tracker list file may have a comment after the URL. It must not reach the URL validation.
  Lines := TStringList.Create;
  try
    Lines.Add('  ' + TRACKER_A + '  # comment');
    Lines.Add(TRACKER_B);
    Lines.Add('');

    SanitizeTrackerList(Lines);

    CheckEquals(3, Lines.Count, 'Line count must not change');
    CheckEquals(TRACKER_A, Lines[0], 'Comment and spaces must be removed');
    CheckEquals(TRACKER_B, Lines[1], 'A clean URL must stay unchanged');
    CheckEquals('', Lines[2], 'An empty line must stay empty');
  finally
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_InvalidTrackerURLMessage_Lists_All_Valid_Prefixes;
var
  Prefix: UTF8String;
begin
  for Prefix in VALID_TRACKERS_URL do
    Check(Pos(Prefix, InvalidTrackerURLMessage) > 0,
      'Error message must mention ' + Prefix);
  CheckEquals('ERROR: Tracker URL must begin with udp://, http://, https://, ws:// or wss://',
    InvalidTrackerURLMessage, 'Wrong error message');
end;

procedure TTestUpdateTorrent.Test_ValidateNewTrackerLines_Cleans_And_Ignores_Duplicates;
var
  Lines, Added: TStringList;
  ErrorStr, FailedTracker: UTF8String;
begin
  Lines := TStringList.Create;
  Added := TStringList.Create;
  try
    Lines.Add('  ' + TRACKER_A + '  ');
    Lines.Add('');
    Lines.Add(TRACKER_B);
    Lines.Add(TRACKER_A);

    CheckTrue(ValidateNewTrackerLines(Lines, False, Added, ErrorStr, FailedTracker),
      'Valid lines must be accepted');

    CheckEquals(2, Added.Count, 'Duplicates and empty lines must be dropped');
    CheckEquals(TRACKER_A, Added[0], 'Spaces must be removed, the order must be kept');
    CheckEquals(TRACKER_B, Added[1], 'Wrong second tracker');
    CheckEquals('', ErrorStr, 'No error expected');
  finally
    Added.Free;
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_ValidateNewTrackerLines_Rejects_Unknown_Scheme;
var
  Lines, Added: TStringList;
  ErrorStr, FailedTracker: UTF8String;
begin
  Lines := TStringList.Create;
  Added := TStringList.Create;
  try
    Lines.Add(TRACKER_A);
    Lines.Add('ftp://c.test/announce');
    Lines.Add(TRACKER_B);

    CheckFalse(ValidateNewTrackerLines(Lines, False, Added, ErrorStr, FailedTracker),
      'An unknown scheme must be rejected');

    CheckEquals(InvalidTrackerURLMessage, ErrorStr, 'Wrong error message');
    CheckEquals('ftp://c.test/announce', FailedTracker, 'Wrong rejected tracker');
  finally
    Added.Free;
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_ValidateNewTrackerLines_Announce_Check;
var
  Lines, Added: TStringList;
  ErrorStr, FailedTracker: UTF8String;
begin
  Lines := TStringList.Create;
  Added := TStringList.Create;
  try
    Lines.Add('udp://a.test:6969');

    CheckFalse(ValidateNewTrackerLines(Lines, False, Added, ErrorStr, FailedTracker),
      'A tracker without /announce must be rejected');
    CheckEquals('ERROR: Tracker URL must end with /announce or /announce.php',
      ErrorStr, 'Wrong error message');
    CheckEquals('udp://a.test:6969', FailedTracker, 'Wrong rejected tracker');

    CheckTrue(ValidateNewTrackerLines(Lines, True, Added, ErrorStr, FailedTracker),
      'SkipAnnounceCheck must accept it');

    //WebTorrent trackers never have /announce
    Lines.Clear;
    Lines.Add('wss://tracker.test');
    CheckTrue(ValidateNewTrackerLines(Lines, False, Added, ErrorStr, FailedTracker),
      'A WebTorrent tracker needs no /announce');
  finally
    Added.Free;
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_ValidateNewTrackerLines_Replaces_Previous_Result;
var
  Lines, Added: TStringList;
  ErrorStr, FailedTracker: UTF8String;
begin
  Lines := TStringList.Create;
  Added := TStringList.Create;
  try
    Added.Add(TRACKER_C);
    Lines.Add(TRACKER_A);

    CheckTrue(ValidateNewTrackerLines(Lines, False, Added, ErrorStr, FailedTracker),
      'Valid lines must be accepted');

    CheckEquals(1, Added.Count, 'The previous result must be replaced');
    CheckEquals(TRACKER_A, Added[0], 'Wrong tracker');
  finally
    Added.Free;
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_ReadAddTrackersFile_Unreadable_Keeps_Lines;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(TRACKER_A);

    CheckFalse(ReadAddTrackersFile(FTempFolder + 'no_such_file.txt', Lines),
      'A missing file must be reported');
    CheckEquals(1, Lines.Count, 'The lines must stay unchanged');
    CheckEquals(TRACKER_A, Lines[0], 'The lines must stay unchanged');
  finally
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_LoadAddTrackersRaw_Without_File_Uses_Recommended_Trackers;
var
  Lines: TStringList;
  TrackerFile: TStringList;
begin
  Lines := TStringList.Create;
  TrackerFile := TStringList.Create;
  try
    LoadAddTrackersRaw(FTempFolder, Lines);
    CheckTrue(Lines.Count > 0, 'Without a file the recommended trackers must be used');

    TrackerFile.Add(TRACKER_B + ' # comment');
    TrackerFile.SaveToFile(FTempFolder + FILE_NAME_ADD_TRACKERS);

    LoadAddTrackersRaw(FTempFolder, Lines);
    CheckEquals(1, Lines.Count, 'The file must replace the recommended trackers');
    CheckEquals(TRACKER_B, Lines[0], 'The comment must be removed');
  finally
    DeleteFile(FTempFolder + FILE_NAME_ADD_TRACKERS);
    TrackerFile.Free;
    Lines.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_LoadRemoveTrackers_Present_And_Missing;
var
  TrackerList: TTrackerList;
  TrackerFile: TStringList;
  FilePresent: boolean;
begin
  CreateTrackerList(TrackerList);
  TrackerFile := TStringList.Create;
  try
    LoadRemoveTrackers(FTempFolder, TrackerList, FilePresent);
    CheckFalse(FilePresent, 'No file present');
    CheckEquals(0, TrackerList.TrackerBanByUserList.Count, 'No ban list without a file');

    TrackerFile.Add(TRACKER_A);
    TrackerFile.SaveToFile(FTempFolder + FILE_NAME_REMOVE_TRACKERS);

    LoadRemoveTrackers(FTempFolder, TrackerList, FilePresent);
    CheckTrue(FilePresent, 'File must be detected');
    CheckEquals(1, TrackerList.TrackerBanByUserList.Count, 'Wrong ban list count');
    CheckEquals(TRACKER_A, TrackerList.TrackerBanByUserList[0], 'Wrong ban list item');
  finally
    DeleteFile(FTempFolder + FILE_NAME_REMOVE_TRACKERS);
    TrackerFile.Free;
    FreeTrackerList(TrackerList);
  end;
end;

initialization
  RegisterTest(TTestUpdateTorrent);
end.