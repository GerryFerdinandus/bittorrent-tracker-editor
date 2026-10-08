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
  update_torrent, main_common, BEncode, test_miscellaneous;

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

    //Two torrent files with their own trackers, trackers to add and trackers to keep.
    //File 1 has B, C, D. File 2 has D, C. Added: A, C. Kept from all files: B, D, E
    procedure PrepareTwoFilesForModes;

    //Combine like the program does for the sort, then update all the files in the given order
    function UpdateWithOrder(Order: TTrackerListOrder): TUpdateTorrentResult;

    //Read a torrent file as bencode. The caller must free the result.
    function ReadBEncodedFile(const FileName: string): TBEncoded;
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
    procedure Test_LoadRemoveTrackers_Unreadable_Is_Reported;
    procedure Test_PackagedTrackerListFolder_Rules;
    procedure Test_DetermineTrackerListFolder_Default_Is_Folder_Of_Program;
    procedure Test_TrySaveTrackerFinalListToFile_Writes_One_Group_Per_Tracker;
    procedure Test_TrySaveTrackerFinalListToFile_Failure_Is_Reported_Not_Raised;
    procedure Test_LoadTorrentViaDir_Uppercase_Extension_And_Skips_Folders;
    procedure Test_Order_U0_Insert_Before_Keep_New_Intact;
    procedure Test_Order_U1_Insert_Before_Keep_Original_Intact;
    procedure Test_Order_U2_Append_After_Keep_New_Intact;
    procedure Test_Order_U3_Append_After_Keep_Original_Intact;
    procedure Test_Order_U5_Insert_Before_Remove_Nothing;
    procedure Test_Order_U6_Append_After_Remove_Nothing;
    procedure Test_Order_U7_Randomize_Writes_The_Same_Trackers;
    procedure Test_Ban_And_Deselected_Lists_Remove_Trackers_From_Every_File;
    procedure Test_Source_Tag_Is_Kept_Replaced_Or_Removed;
    procedure Test_One_Tracker_Writes_Announce_Without_AnnounceList;
    procedure Test_Several_Trackers_Write_Announce_And_AnnounceList;
    procedure Test_Private_Torrent_With_Same_Settings_Stays_Byte_Identical;
    procedure Test_Mismatched_FileSettingList_Length_Is_Refused;
  end;

implementation

const
  PIECES = '20:AAAAAAAAAAAAAAAAAAAA';
  INFO_ONE_FILE =
    'd6:lengthi1024e4:name8:test.bin12:piece lengthi16384e6:pieces' + PIECES + 'e';

  TRACKER_A = 'udp://a.test/announce';
  TRACKER_B = 'udp://b.test/announce';
  TRACKER_C = 'udp://c.test/announce';
  TRACKER_D = 'udp://d.test/announce';
  TRACKER_E = 'udp://e.test/announce';

function BEncodeString(const Str: UTF8String): UTF8String;
begin
  Result := IntToStr(Length(Str)) + ':' + Str;
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

procedure TTestUpdateTorrent.PrepareTwoFilesForModes;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent',
    [TRACKER_B, TRACKER_C, TRACKER_D]));
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent',
    [TRACKER_D, TRACKER_C]));

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_C);

  FTrackerList.TrackerFromInsideTorrentFilesList.Add(TRACKER_B);
  FTrackerList.TrackerFromInsideTorrentFilesList.Add(TRACKER_D);
  FTrackerList.TrackerFromInsideTorrentFilesList.Add(TRACKER_E);
end;

function TTestUpdateTorrent.UpdateWithOrder(Order: TTrackerListOrder): TUpdateTorrentResult;
begin
  FTrackerList.TrackerListOrderForUpdatedTorrent := Order;
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  Result := UpdateTorrentFileList(FTrackerList, FDecodeTorrent, DefaultFileSettingList);
  CheckEquals(FTrackerList.TorrentFileNameList.Count, Result.FilesUpdated,
    'Every torrent file must be updated');
end;

function TTestUpdateTorrent.ReadBEncodedFile(const FileName: string): TBEncoded;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := TBEncoded.Create(Stream);
  finally
    Stream.Free;
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

  CheckTrue(PathIsTrackerListFile('a.txt'), 'Wrong .txt detection');
  CheckTrue(PathIsTrackerListFile('A.TXT'), '.txt must be case insensitive');
  CheckFalse(PathIsTrackerListFile('a.torrent'), 'A .torrent is not a tracker list');
  CheckFalse(PathIsTrackerListFile('a.txt.bak'), 'A .bak is not a tracker list');
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

procedure TTestUpdateTorrent.
Test_TrySaveTrackerFinalListToFile_Writes_One_Group_Per_Tracker;
var
  Trackers, Saved: TStringList;
begin
  Trackers := TStringList.Create;
  Saved := TStringList.Create;
  try
    Trackers.Add(TRACKER_A);
    Trackers.Add(TRACKER_B);

    CheckTrue(TrySaveTrackerFinalListToFile(FTempFolder, Trackers),
      'The export file must be written');

    Saved.LoadFromFile(FTempFolder + FILE_NAME_EXPORT_TRACKERS);
    //Every tracker is a separate tracker group, one empty line between each
    CheckEquals(4, Saved.Count, 'Wrong line count');
    CheckEquals(TRACKER_A, Saved[0], 'Wrong first tracker');
    CheckEquals('', Saved[1], 'Wrong first separator');
    CheckEquals(TRACKER_B, Saved[2], 'Wrong second tracker');
    CheckEquals('', Saved[3], 'Wrong second separator');
  finally
    DeleteFile(FTempFolder + FILE_NAME_EXPORT_TRACKERS);
    Saved.Free;
    Trackers.Free;
  end;
end;

procedure TTestUpdateTorrent.
Test_TrySaveTrackerFinalListToFile_Failure_Is_Reported_Not_Raised;
var
  Trackers: TStringList;
begin
  Trackers := TStringList.Create;
  try
    Trackers.Add(TRACKER_A);

    //A folder with the name of the export file: it can not be written.
    //The GUI must still reload the torrent files and show the result after this.
    ForceDirectories(FTempFolder + FILE_NAME_EXPORT_TRACKERS);
    CheckFalse(TrySaveTrackerFinalListToFile(FTempFolder, Trackers),
      'An export file that can not be written must be reported');
  finally
    RemoveDir(FTempFolder + FILE_NAME_EXPORT_TRACKERS);
    Trackers.Free;
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

procedure TTestUpdateTorrent.Test_PackagedTrackerListFolder_Rules;
const
  SNAP = '/snap/common';
  FLATPAK_DATA = '/flatpak/data';
  OWD = '/original/work/dir';
begin
  CheckEquals('', PackagedTrackerListFolder('', '', '', '', ''), 'Not packaged');
  CheckEquals('', PackagedTrackerListFolder('', '', FLATPAK_DATA, '', OWD),
    'The other variables are not used without snap, flatpak or AppImage');

  CheckEquals(SNAP, PackagedTrackerListFolder(SNAP, '', '', '', ''), 'Snap');

  CheckEquals(FLATPAK_DATA, PackagedTrackerListFolder('', 'flatpak', FLATPAK_DATA, '', ''),
    'Flatpak');
  CheckEquals('', PackagedTrackerListFolder('', 'podman', FLATPAK_DATA, '', ''),
    'Only the container "flatpak" uses XDG_DATA_HOME');
  CheckEquals('', PackagedTrackerListFolder(SNAP, 'flatpak', '', '', ''),
    'Flatpak without XDG_DATA_HOME gives the folder of the program');

  CheckEquals(OWD, PackagedTrackerListFolder('', '', '', '/tmp/.mount/app.AppImage', OWD),
    'AppImage uses the original working directory');
  CheckEquals('', PackagedTrackerListFolder('', '', '', '/tmp/.mount/app.AppImage', ''),
    'AppImage without OWD gives the folder of the program');

  CheckEquals(FLATPAK_DATA, PackagedTrackerListFolder(SNAP, 'flatpak', FLATPAK_DATA, '', ''),
    'Flatpak is stronger than snap');
  CheckEquals(OWD, PackagedTrackerListFolder(SNAP, 'flatpak', FLATPAK_DATA,
    '/tmp/.mount/app.AppImage', OWD), 'AppImage is stronger than flatpak and snap');
end;

procedure TTestUpdateTorrent.Test_DetermineTrackerListFolder_Default_Is_Folder_Of_Program;
var
  Folder: string;
begin
  {$IFDEF DARWIN}
  //macOS always uses ~/.config/trackereditor/ and creates it.
  Ignore('The folder is ~/.config/trackereditor/ on macOS');
  {$ELSE}
  {$IFDEF LINUX}
  if (GetEnvironmentVariable('SNAP_USER_COMMON') <> '') or
    (GetEnvironmentVariable('container') = 'flatpak') or
    (GetEnvironmentVariable('APPIMAGE') <> '') then
  begin
    Ignore('The test runs inside a snap, flatpak or AppImage');
    Exit;
  end;
  {$ENDIF}
  Folder := DetermineTrackerListFolder(FTempFolder + 'program');
  CheckEquals(FTempFolder, Folder, 'The folder of the program, with a path delimiter');
  {$ENDIF}
end;

procedure TTestUpdateTorrent.Test_LoadRemoveTrackers_Unreadable_Is_Reported;
var
  TrackerList: TTrackerList;
  TrackerFile: TStringList;
  LockedFile: TFileStream;
  FilePresent: boolean;
begin
  CreateTrackerList(TrackerList);
  TrackerFile := TStringList.Create;
  try
    TrackerFile.Add(TRACKER_A);
    TrackerFile.SaveToFile(FTempFolder + FILE_NAME_REMOVE_TRACKERS);

    //An exclusive lock makes the file unreadable
    LockedFile := TFileStream.Create(FTempFolder + FILE_NAME_REMOVE_TRACKERS,
      fmOpenRead or fmShareExclusive);
    try
      CheckFalse(LoadRemoveTrackers(FTempFolder, TrackerList, FilePresent),
        'An unreadable file must be reported');
    finally
      LockedFile.Free;
    end;
    CheckFalse(FilePresent, 'An unreadable file is treated as not present');
    CheckEquals(0, TrackerList.TrackerBanByUserList.Count, 'No ban list from an unreadable file');

    CheckTrue(LoadRemoveTrackers(FTempFolder, TrackerList, FilePresent),
      'The same file must load when it is not locked');
    CheckEquals(1, TrackerList.TrackerBanByUserList.Count, 'Wrong ban list count');
  finally
    DeleteFile(FTempFolder + FILE_NAME_REMOVE_TRACKERS);
    TrackerFile.Free;
    FreeTrackerList(TrackerList);
  end;
end;

procedure TTestUpdateTorrent.Test_Order_U0_Insert_Before_Keep_New_Intact;
var
  UpdateResult: TUpdateTorrentResult;
begin
  PrepareTwoFilesForModes;
  UpdateResult := UpdateWithOrder(tloInsertNewBeforeAndKeepNewIntact);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_A, TRACKER_C, TRACKER_B, TRACKER_D, TRACKER_E]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_A, TRACKER_C, TRACKER_D, TRACKER_B, TRACKER_E]);
  CheckEquals(5, UpdateResult.TrackerCount, 'Wrong tracker count');
end;

procedure TTestUpdateTorrent.Test_Order_U1_Insert_Before_Keep_Original_Intact;
begin
  PrepareTwoFilesForModes;
  UpdateWithOrder(tloInsertNewBeforeAndKeepOriginalIntact);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_A, TRACKER_B, TRACKER_C, TRACKER_D, TRACKER_E]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_A, TRACKER_D, TRACKER_C, TRACKER_B, TRACKER_E]);
end;

procedure TTestUpdateTorrent.Test_Order_U2_Append_After_Keep_New_Intact;
begin
  PrepareTwoFilesForModes;
  UpdateWithOrder(tloAppendNewAfterAndKeepNewIntact);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_B, TRACKER_D, TRACKER_A, TRACKER_C, TRACKER_E]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_D, TRACKER_A, TRACKER_C, TRACKER_B, TRACKER_E]);
end;

procedure TTestUpdateTorrent.Test_Order_U3_Append_After_Keep_Original_Intact;
begin
  PrepareTwoFilesForModes;
  UpdateWithOrder(tloAppendNewAfterAndKeepOriginalIntact);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_B, TRACKER_C, TRACKER_D, TRACKER_A, TRACKER_E]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_D, TRACKER_C, TRACKER_A, TRACKER_B, TRACKER_E]);
end;

procedure TTestUpdateTorrent.Test_Order_U5_Insert_Before_Remove_Nothing;
var
  UpdateResult: TUpdateTorrentResult;
begin
  PrepareTwoFilesForModes;
  UpdateResult := UpdateWithOrder(tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing);

  //The trackers kept from the other files are not added
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_A, TRACKER_B, TRACKER_C, TRACKER_D]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_A, TRACKER_D, TRACKER_C]);
  CheckEquals(3, UpdateResult.TrackerCount, 'The count is of the last file');
end;

procedure TTestUpdateTorrent.Test_Order_U6_Append_After_Remove_Nothing;
begin
  PrepareTwoFilesForModes;
  UpdateWithOrder(tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_B, TRACKER_C, TRACKER_D, TRACKER_A]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_D, TRACKER_C, TRACKER_A]);
end;

procedure TTestUpdateTorrent.Test_Order_U7_Randomize_Writes_The_Same_Trackers;
var
  Sorted: TStringList;
  FileName: string;
begin
  PrepareTwoFilesForModes;
  UpdateWithOrder(tloRandomize);

  Sorted := TStringList.Create;
  try
    //Added: A, C. Kept from all files: B, D, E. The order is random.
    for FileName in FTrackerList.TorrentFileNameList do
    begin
      Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode ' + FileName);
      Sorted.Assign(FDecodeTorrent.TrackerList);
      Sorted.Sort;
      CheckEquals(5, Sorted.Count, 'Wrong tracker count in ' + FileName);
      CheckEquals(TRACKER_A, Sorted[0], 'Tracker A is lost in ' + FileName);
      CheckEquals(TRACKER_B, Sorted[1], 'Tracker B is lost in ' + FileName);
      CheckEquals(TRACKER_C, Sorted[2], 'Tracker C is lost in ' + FileName);
      CheckEquals(TRACKER_D, Sorted[3], 'Tracker D is lost in ' + FileName);
      CheckEquals(TRACKER_E, Sorted[4], 'Tracker E is lost in ' + FileName);
    end;
  finally
    Sorted.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_Ban_And_Deselected_Lists_Remove_Trackers_From_Every_File;
begin
  PrepareTwoFilesForModes;
  //B is banned. E is deselected. A is deselected too, but the user added it, so it stays.
  FTrackerList.TrackerBanByUserList.Add(TRACKER_B);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(TRACKER_E);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(TRACKER_A);

  UpdateWithOrder(tloInsertNewBeforeAndKeepNewIntact);

  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0],
    [TRACKER_A, TRACKER_C, TRACKER_D]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1],
    [TRACKER_A, TRACKER_C, TRACKER_D]);

  //The lists of the caller are not changed by the update
  CheckEquals(1, FTrackerList.TrackerBanByUserList.Count, 'Ban list');
  CheckEquals(2, FTrackerList.TrackerManuallyDeselectedByUserList.Count, 'Deselected list');
end;

procedure TTestUpdateTorrent.Test_Source_Tag_Is_Kept_Replaced_Or_Removed;
var
  FileName: string;
begin
  FileName := CreateTorrentFile('one.torrent', [TRACKER_A]);
  FTrackerList.TorrentFileNameList.Add(FileName);

  //The torrent has a source
  Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode the torrent');
  Check(FDecodeTorrent.InfoSourceAdd('OLD'), 'Can not add the source');
  Check(FDecodeTorrent.SaveTorrent(FileName), 'Can not save the torrent');

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);

  //An empty source tag does not change anything
  FTrackerList.SourceTag := '';
  FTrackerList.RemoveAllSourceTag := False;
  UpdateWithOrder(tloSort);
  Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode the torrent');
  CheckEquals('OLD', FDecodeTorrent.InfoSource, 'An empty source tag must keep the source');

  FTrackerList.SourceTag := 'NEW';
  UpdateWithOrder(tloSort);
  Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode the torrent');
  CheckEquals('NEW', FDecodeTorrent.InfoSource, 'The source must be replaced');

  //Remove all source tags wins over the source tag
  FTrackerList.RemoveAllSourceTag := True;
  UpdateWithOrder(tloSort);
  Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode the torrent');
  CheckEquals('', FDecodeTorrent.InfoSource, 'The source must be removed');
end;

procedure TTestUpdateTorrent.Test_One_Tracker_Writes_Announce_Without_AnnounceList;
var
  Root: TBEncoded;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent',
    [TRACKER_B, TRACKER_C]));
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);

  UpdateWithOrder(tloSort);

  Root := ReadBEncodedFile(FTrackerList.TorrentFileNameList[0]);
  try
    CheckEquals(TRACKER_A, Root.ListData.FindElement('announce').StringData,
      'Wrong announce');
    CheckNull(Root.ListData.FindElement('announce-list'),
      'One tracker must not have an announce-list');
  finally
    Root.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_Several_Trackers_Write_Announce_And_AnnounceList;
var
  Root: TBEncoded;
begin
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent', [TRACKER_C]));
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_B);
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);

  UpdateWithOrder(tloSort);

  Root := ReadBEncodedFile(FTrackerList.TorrentFileNameList[0]);
  try
    CheckEquals(TRACKER_A, Root.ListData.FindElement('announce').StringData,
      'The announce must be the first tracker');
    CheckEquals(2, Root.ListData.FindElement('announce-list').ListData.Count,
      'Wrong tier count');
  finally
    Root.Free;
  end;
end;

procedure TTestUpdateTorrent.Test_Private_Torrent_With_Same_Settings_Stays_Byte_Identical;
var
  FileName: string;
  Original, After: UTF8String;
  Stream: TFileStream;
  Settings: TTorrentFileSettingArray;
begin
  //The 'info' keys are deliberately not in order. Writing them again would change the info hash.
  Original := 'd8:announce' + BEncodeString(TRACKER_A) + '7:comment3:abc4:info' +
    'd7:privatei1e6:source3:abc6:lengthi1024e4:name8:test.bin' +
    '12:piece lengthi16384e6:pieces' + PIECES + 'ee';

  FileName := FTempFolder + 'private.torrent';
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Original[1], Length(Original));
  finally
    Stream.Free;
  end;
  FTrackerList.TorrentFileNameList.Add(FileName);

  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  FTrackerList.SourceTag := 'abc';
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  //Private, same comment, same source and the same tracker as the torrent has
  Settings := DefaultFileSettingList;
  Settings[0].PublicTorrent := False;
  Settings[0].Comment := 'abc';
  CheckEquals(1, UpdateTorrentFileList(FTrackerList, FDecodeTorrent, Settings).FilesUpdated,
    'The file must be updated');

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(After, Stream.Size);
    Stream.ReadBuffer(After[1], Length(After));
  finally
    Stream.Free;
  end;
  Check(Original = After, 'The torrent must stay byte identical');
end;

procedure TTestUpdateTorrent.Test_Mismatched_FileSettingList_Length_Is_Refused;
var
  Settings: TTorrentFileSettingArray;
  Raised: boolean;
begin
{$IFOPT C+}
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('one.torrent', [TRACKER_B]));
  FTrackerList.TorrentFileNameList.Add(CreateTorrentFile('two.torrent', [TRACKER_B]));
  FTrackerList.TrackerAddedByUserList.Add(TRACKER_A);
  CombineFiveTrackerListToOne(tloSort, FTrackerList, FDecodeTorrent.TrackerList);

  //One setting for two files
  Settings := DefaultFileSettingList;
  SetLength(Settings, 1);

  Raised := False;
  try
    UpdateTorrentFileList(FTrackerList, FDecodeTorrent, Settings);
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'A wrong number of settings must be refused');

  //No file may be changed
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[0], [TRACKER_B]);
  CheckTrackerListInFile(FTrackerList.TorrentFileNameList[1], [TRACKER_B]);
{$ELSE}
  Settings := nil;
  Raised := False;
  Ignore('Assertions are off, the length of the settings is only checked by an assert');
{$ENDIF}
end;

initialization
  RegisterTest(TTestUpdateTorrent);
end.