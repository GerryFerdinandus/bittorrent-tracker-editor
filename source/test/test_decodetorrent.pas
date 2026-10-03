// SPDX-License-Identifier: MIT
unit test_decodetorrent;

{
  Decode torrent files that are build in memory.
  No torrent file on disk and no internet connection is needed for these tests.
}

{$mode objfpc}{$H+}
//Needed because this unit has non-Latin string literals in source; without it FPC tags
//them with the default AnsiString codepage and double-encodes them on assignment to
//UTF8String. Not needed in trackereditor/trackereditor_cli: their non-Latin text is always
//runtime data (file bytes, ParamStr, LCL widgets), never a compiled string literal.
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, decodetorrent;

type

  { TTestDecodeTorrent }

  TTestDecodeTorrent = class(TTestCase)
  private
    FDecodeTorrent: TDecodeTorrent;

    //Decode a torrent that is present as a bencoded string
    function DecodeTorrentString(const TorrentStr: UTF8String): boolean;

    //Save FDecodeTorrent to a temp file and return the exact bytes that were written
    function SaveAndReadBack: UTF8String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Single_File_V1_InfoHash;
    procedure Test_Single_File_V1_FileList;
    procedure Test_Multi_File_V1_InfoHash;
    procedure Test_Multi_File_V1_Entry_Missing_Length_Is_Rejected;
    procedure Test_Multi_File_V1_Entry_Missing_Path_Is_Rejected;
    procedure Test_Hybrid_Malformed_V2_FileTree_Fails_Decode;
    procedure Test_Torrent_Without_Pieces_Has_No_InfoHash;
    procedure Test_Comment_Remove_Then_Add_Again;
    procedure Test_NonLatin_FileName_And_Comment_Inside_Torrent;
    procedure Test_SaveTorrent_Overwrites_Existing_File_Without_Temp_Leftover;
    procedure Test_SaveTorrent_Failure_Leaves_No_Temp_File;
    procedure Test_Comment_Added_Keeps_Root_Keys_Sorted;
    procedure Test_Root_Keys_Are_Sorted_By_Byte_Order;
    procedure Test_Private_And_Source_Already_Present_Keep_Info_Unchanged;
    procedure Test_Private_Flag_With_Value_Zero_Is_Replaced_Not_Duplicated;
    procedure Test_Empty_AnnounceList_Tier_Is_Skipped;
    procedure Test_Failed_Decode_Clears_Previous_Torrent_State;    {$IFDEF UNIX}
    procedure Test_SaveTorrent_Keeps_Permissions_Of_Original;
    {$ENDIF}
  end;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}
const
  //20 bytes of 'pieces', this is one SHA1 piece hash.
  PIECES = '20:AAAAAAAAAAAAAAAAAAAA';

  //Torrent with one file has 'info.length' and no 'info.files'
  INFO_SINGLE_FILE =
    'd6:lengthi1024e4:name8:test.bin12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Torrent with more then one file has 'info.files' and no 'info.length'
  INFO_MULTI_FILE =
    'd5:filesld6:lengthi100e4:pathl5:a.txteed6:lengthi200e4:pathl3:dir5:b.txteee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Second file entry has no 'length', it must not inherit the first entry's 100
  INFO_MULTI_FILE_MISSING_LENGTH =
    'd5:filesld6:lengthi100e4:pathl5:a.txteed4:pathl5:b.txteee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Second file entry has no 'path', it must not inherit the first entry's name
  INFO_MULTI_FILE_MISSING_PATH =
    'd5:filesld6:lengthi100e4:pathl5:a.txteed6:lengthi200eee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Both 'info.pieces' (V1) and 'info.file tree' (V2) are present, but the
  //V2 tree is empty, so GetFileList_V2 finds no files (a malformed hybrid torrent).
  INFO_HYBRID_MALFORMED_V2 =
    'd9:file treede5:filesld6:lengthi100e4:pathl5:a.txteee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Neither V1 'info.pieces' nor V2 'info.file tree' is present
  INFO_WITHOUT_PIECES = 'd6:lengthi1024e4:name8:test.bine';

  ANNOUNCE = '8:announce27:udp://tracker.test/announce';

  //SHA1 of INFO_SINGLE_FILE
  INFO_HASH_SINGLE_FILE = '7DBDCBCFBDD306C058848B78BC2048822BB4CBA6';

  //SHA1 of INFO_MULTI_FILE
  INFO_HASH_MULTI_FILE = '2575ADB45B1E904ADF75726BFD5C27FD76897C2B';

  NO_INFO_HASH = 'N/A';

function BEncodeString(const Str: UTF8String): UTF8String;
begin
  Result := IntToStr(Length(Str)) + ':' + Str;
end;

function BuildTorrent(const InfoStr: UTF8String): UTF8String;
begin
  //Dictionary keys must be in alphabetical order: 'announce' before 'info'
  Result := 'd' + ANNOUNCE + '4:info' + InfoStr + 'e';
end;

{ TTestDecodeTorrent }

procedure TTestDecodeTorrent.SetUp;
begin
  FDecodeTorrent := TDecodeTorrent.Create;
end;

procedure TTestDecodeTorrent.TearDown;
begin
  FDecodeTorrent.Free;
end;

function TTestDecodeTorrent.DecodeTorrentString(const TorrentStr: UTF8String): boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(TorrentStr[1], Length(TorrentStr));
    Result := FDecodeTorrent.DecodeTorrent(Stream);
  finally
    Stream.Free;
  end;
end;

function TTestDecodeTorrent.SaveAndReadBack: UTF8String;
var
  TempFileName: string;
  Stream: TFileStream;
begin
  TempFileName := GetTempDir + 'test_decodetorrent_saveandreadback.torrent';
  Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not save torrent');
  try
    Stream := TFileStream.Create(TempFileName, fmOpenRead);
    try
      SetLength(Result, Stream.Size);
      Stream.ReadBuffer(Result[1], Length(Result));
    finally
      Stream.Free;
    end;
  finally
    DeleteFile(TempFileName);
  end;
end;

procedure TTestDecodeTorrent.Test_Single_File_V1_InfoHash;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  CheckEquals(Ord(tv_V1), Ord(FDecodeTorrent.TorrentVersion),
    'Torrent with one file must be detected as V1');

  CheckEquals(INFO_HASH_SINGLE_FILE, FDecodeTorrent.InfoHash_V1,
    'Torrent with one file must have a V1 info hash');

  CheckEquals(NO_INFO_HASH, FDecodeTorrent.InfoHash_V2,
    'A V1 torrent must not have a V2 info hash');
end;

procedure TTestDecodeTorrent.Test_Single_File_V1_FileList;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  CheckEquals(1, FDecodeTorrent.InfoFilesCount, 'Wrong file count');
  CheckEquals('test.bin', FDecodeTorrent.InfoFilesNameIndex(0), 'Wrong file name');
  CheckEquals(1024, FDecodeTorrent.InfoFilesLengthIndex(0), 'Wrong file length');
  CheckEquals(1024, FDecodeTorrent.TotalFileSize, 'Wrong total file size');

  CheckEquals(1, FDecodeTorrent.TrackerList.Count, 'Wrong tracker count');
  CheckEquals('udp://tracker.test/announce', FDecodeTorrent.TrackerList[0],
    'Wrong tracker URL');
end;

procedure TTestDecodeTorrent.Test_Multi_File_V1_InfoHash;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_MULTI_FILE)),
    'Can not decode a torrent with more then one file');

  CheckEquals(Ord(tv_V1), Ord(FDecodeTorrent.TorrentVersion),
    'Torrent with more then one file must be detected as V1');

  CheckEquals(INFO_HASH_MULTI_FILE, FDecodeTorrent.InfoHash_V1,
    'Torrent with more then one file must have a V1 info hash');

  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'Wrong file count');
  CheckEquals(300, FDecodeTorrent.TotalFileSize, 'Wrong total file size');
end;

procedure TTestDecodeTorrent.Test_Multi_File_V1_Entry_Missing_Length_Is_Rejected;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_MULTI_FILE_MISSING_LENGTH)),
    'Can not decode a torrent with a malformed file entry');

  CheckEquals(1, FDecodeTorrent.InfoFilesCount,
    'The entry without ''length'' must be rejected, not added with a stale length');
  CheckEquals(DirectorySeparator + 'a.txt', FDecodeTorrent.InfoFilesNameIndex(0),
    'Wrong file name');
  CheckEquals(100, FDecodeTorrent.TotalFileSize,
    'Total size must not include the rejected entry');
end;

procedure TTestDecodeTorrent.Test_Multi_File_V1_Entry_Missing_Path_Is_Rejected;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_MULTI_FILE_MISSING_PATH)),
    'Can not decode a torrent with a malformed file entry');

  CheckEquals(1, FDecodeTorrent.InfoFilesCount,
    'The entry without ''path'' must be rejected, not added with a stale name');
  CheckEquals(DirectorySeparator + 'a.txt', FDecodeTorrent.InfoFilesNameIndex(0),
    'Wrong file name');
  CheckEquals(100, FDecodeTorrent.TotalFileSize,
    'Total size must not include the rejected entry');
end;

procedure TTestDecodeTorrent.Test_Hybrid_Malformed_V2_FileTree_Fails_Decode;
begin
  //V1 succeeds ('files' has one entry), but the V2 'file tree' is empty.
  //DecodeTorrent must not report success while the V2 file model is empty.
  Check(not DecodeTorrentString(BuildTorrent(INFO_HYBRID_MALFORMED_V2)),
    'Decoding must fail when the V2 file tree has no files');
end;

procedure TTestDecodeTorrent.Test_Torrent_Without_Pieces_Has_No_InfoHash;
begin
  //Decoding must not crash on a torrent that has no V1 and no V2 marker.
  Check(DecodeTorrentString(BuildTorrent(INFO_WITHOUT_PIECES)),
    'Can not decode a torrent without pieces');

  CheckEquals(NO_INFO_HASH, FDecodeTorrent.InfoHash_V1, 'There must be no V1 info hash');
  CheckEquals(NO_INFO_HASH, FDecodeTorrent.InfoHash_V2, 'There must be no V2 info hash');
end;

procedure TTestDecodeTorrent.Test_Comment_Remove_Then_Add_Again;
var
  TempFileName: string;
  ReloadedTorrent: TDecodeTorrent;
begin
  //Removing the comment frees its bencode element. Setting a new comment
  //afterwards must not write through the now-dangling FBEncoded_Comment pointer.
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  FDecodeTorrent.Comment := 'first comment';
  FDecodeTorrent.Comment := '';
  FDecodeTorrent.Comment := 'second comment';

  TempFileName := GetTempDir + 'test_decodetorrent_comment.torrent';
  Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not save torrent');

  ReloadedTorrent := TDecodeTorrent.Create;
  try
    Check(ReloadedTorrent.DecodeTorrent(TempFileName),
      'Can not decode the saved torrent');
    CheckEquals('second comment', ReloadedTorrent.Comment,
      'Comment must be the last value set after remove and re-add');
  finally
    ReloadedTorrent.Free;
    DeleteFile(TempFileName);
  end;
end;

procedure TTestDecodeTorrent.Test_NonLatin_FileName_And_Comment_Inside_Torrent;
const
  //CJK + Cyrillic mix
  NON_LATIN_NAME = '文件_трекер.bin';
  NON_LATIN_COMMENT = '注释_комментарий';
var
  InfoStr: UTF8String;
  TempFileName: string;
  ReloadedTorrent: TDecodeTorrent;
begin
  InfoStr := 'd5:filesld6:lengthi100e4:pathl' + BEncodeString('a.txt') + 'eed' +
    '6:lengthi200e4:pathl' + BEncodeString(NON_LATIN_NAME) + 'eee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  Check(DecodeTorrentString(BuildTorrent(InfoStr)),
    'Can not decode a torrent with a non-Latin file name');

  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'Wrong file count');
  CheckEquals(DirectorySeparator + NON_LATIN_NAME, FDecodeTorrent.InfoFilesNameIndex(1),
    'Non-Latin file name must survive decode unchanged');

  //Comment round-trip: set, save to disk, reload, and compare.
  FDecodeTorrent.Comment := NON_LATIN_COMMENT;

  TempFileName := GetTempDir + 'test_decodetorrent_nonlatin_comment.torrent';
  Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not save torrent');

  ReloadedTorrent := TDecodeTorrent.Create;
  try
    Check(ReloadedTorrent.DecodeTorrent(TempFileName),
      'Can not decode the saved torrent');
    CheckEquals(NON_LATIN_COMMENT, ReloadedTorrent.Comment,
      'Non-Latin comment must survive save/reload unchanged');
  finally
    ReloadedTorrent.Free;
    DeleteFile(TempFileName);
  end;
end;

procedure TTestDecodeTorrent.Test_SaveTorrent_Overwrites_Existing_File_Without_Temp_Leftover;
var
  TempFileName: string;
  ReloadedTorrent: TDecodeTorrent;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  TempFileName := GetTempDir + 'test_decodetorrent_overwrite.torrent';
  Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not save torrent the first time');

  FDecodeTorrent.Comment := 'changed';
  Check(FDecodeTorrent.SaveTorrent(TempFileName),
    'Can not overwrite the existing torrent file');
  Check(not FileExists(TempFileName + '.tmp'), 'Temp file must not be left behind');

  ReloadedTorrent := TDecodeTorrent.Create;
  try
    Check(ReloadedTorrent.DecodeTorrent(TempFileName),
      'Can not decode the overwritten torrent');
    CheckEquals('changed', ReloadedTorrent.Comment, 'Overwrite must contain the new data');
  finally
    ReloadedTorrent.Free;
    DeleteFile(TempFileName);
  end;
end;

procedure TTestDecodeTorrent.Test_SaveTorrent_Failure_Leaves_No_Temp_File;
var
  TargetFolder: string;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  //A file can not replace an existing folder, so the final rename must fail.
  TargetFolder := GetTempDir + 'test_decodetorrent_save_fail';
  ForceDirectories(TargetFolder);
  try
    Check(not FDecodeTorrent.SaveTorrent(TargetFolder),
      'Saving over a folder must fail');
    Check(not FileExists(TargetFolder + '.tmp'),
      'Temp file must be removed after a failed save');
    Check(DirectoryExists(TargetFolder), 'The existing target must be untouched');
  finally
    RemoveDir(TargetFolder);
  end;
end;

procedure TTestDecodeTorrent.Test_Comment_Added_Keeps_Root_Keys_Sorted;
var
  Saved: UTF8String;
begin
  //The torrent has no 'comment', so a new element is created and must not be appended after 'info'.
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  FDecodeTorrent.Comment := 'new comment';
  Saved := SaveAndReadBack;

  Check(Pos('7:comment', Saved) > 0, 'Comment must be saved');
  Check(Pos('7:comment', Saved) < Pos('4:info', Saved),
    'Key ''comment'' must come before ''info'' in the saved file');
end;

procedure TTestDecodeTorrent.Test_Root_Keys_Are_Sorted_By_Byte_Order;
var
  Saved: UTF8String;
begin
  //Upper case 'Z' is sorted before lower case 'a' in byte order. A case-insensitive sort moves it to the end.
  Check(DecodeTorrentString('d6:Zextra3:foo' + ANNOUNCE + '4:info' + INFO_SINGLE_FILE + 'e'),
    'Can not decode a torrent with an upper case key');

  //Setting a comment sorts the root dictionary
  FDecodeTorrent.Comment := 'new comment';
  Saved := SaveAndReadBack;

  Check(Pos('6:Zextra', Saved) < Pos('8:announce', Saved),
    'Key ''Zextra'' must come before ''announce'' in byte order');
  Check(Pos('7:comment', Saved) < Pos('4:info', Saved),
    'Key ''comment'' must come before ''info''');
end;

procedure TTestDecodeTorrent.Test_Private_And_Source_Already_Present_Keep_Info_Unchanged;
var
  Before, After: UTF8String;
begin
  //'info' is deliberately not sorted. Rewriting it would change the info hash.
  Check(DecodeTorrentString(BuildTorrent('d7:privatei1e6:source3:abc6:lengthi1024e' +
    '4:name8:test.bin12:piece lengthi16384e6:pieces' + PIECES + 'e')),
    'Can not decode a private torrent');
  Before := SaveAndReadBack;

  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');
  Check(FDecodeTorrent.InfoSourceAdd('abc'), 'Can not add the source');
  After := SaveAndReadBack;

  CheckTrue(FDecodeTorrent.PrivateTorrent, 'Torrent must stay private');
  CheckEquals(Before, After,
    'Adding a flag and source that are already present must not change the torrent');
end;

procedure TTestDecodeTorrent.Test_Private_Flag_With_Value_Zero_Is_Replaced_Not_Duplicated;
var
  Saved: UTF8String;
begin
  //'private' is present but not 1, so the torrent is public and the flag must be replaced.
  Check(DecodeTorrentString(BuildTorrent('d7:privatei0e6:lengthi1024e4:name8:test.bin' +
    '12:piece lengthi16384e6:pieces' + PIECES + 'e')),
    'Can not decode a torrent with private flag 0');
  CheckFalse(FDecodeTorrent.PrivateTorrent, 'Torrent with private flag 0 must be public');

  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');
  CheckTrue(FDecodeTorrent.PrivateTorrent, 'Torrent must be private');

  Saved := SaveAndReadBack;
  CheckEquals(Length(Saved) - Length('7:private'),
    Length(StringReplace(Saved, '7:private', '', [rfReplaceAll])),
    'There must be exactly one ''private'' key');
  Check(Pos('7:privatei1e', Saved) > 0, 'The private flag must have value 1');
end;

procedure TTestDecodeTorrent.Test_Empty_AnnounceList_Tier_Is_Skipped;
const
  TRACKER_B = 'udp://b.test/announce';
begin
  //The first tier is empty ('le'). It must be skipped, not make the whole torrent fail.
  Check(DecodeTorrentString('d' + ANNOUNCE + '13:announce-listlle' + 'l' +
    BEncodeString(TRACKER_B) + 'ee4:info' + INFO_MULTI_FILE + 'e'),
    'An empty tier in announce-list must not make decoding fail');

  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'Wrong file count');
  CheckEquals(2, FDecodeTorrent.TrackerList.Count, 'Wrong tracker count');
  CheckEquals('udp://tracker.test/announce', FDecodeTorrent.TrackerList[0],
    'Wrong first tracker');
  CheckEquals(TRACKER_B, FDecodeTorrent.TrackerList[1], 'Wrong second tracker');
end;

procedure TTestDecodeTorrent.Test_Failed_Decode_Clears_Previous_Torrent_State;
const
  //Root is not a dictionary / root has no 'info' / not bencode at all
  BAD_TORRENTS: array[0..2] of UTF8String = ('i42e', 'd3:cow3:mooe', 'not bencode');
var
  i: integer;
begin
  for i := Low(BAD_TORRENTS) to High(BAD_TORRENTS) do
  begin
    //A valid torrent with metadata is decoded first
    Check(DecodeTorrentString('d' + ANNOUNCE + '7:comment3:old10:created by2:me4:info' +
      'd6:lengthi1024e4:name8:test.bin12:piece lengthi16384e6:pieces' + PIECES +
      '7:private' + 'i1e6:source3:abce' + 'e'),
      'Can not decode the valid torrent');
    CheckEquals('old', FDecodeTorrent.Comment, 'Wrong comment');
    CheckEquals('me', FDecodeTorrent.CreatedBy, 'Wrong created by');
    CheckEquals('abc', FDecodeTorrent.InfoSource, 'Wrong source');
    CheckTrue(FDecodeTorrent.PrivateTorrent, 'Torrent must be private');

    CheckFalse(DecodeTorrentString(BAD_TORRENTS[i]), 'Bad torrent must fail: ' +
      BAD_TORRENTS[i]);

    //Nothing of the previous torrent may be left
    CheckEquals(Ord(tv_unknown), Ord(FDecodeTorrent.TorrentVersion), 'Version');
    CheckEquals('', FDecodeTorrent.InfoHash_V1, 'InfoHash_V1');
    CheckEquals('', FDecodeTorrent.InfoHash_V2, 'InfoHash_V2');
    CheckEquals('', FDecodeTorrent.Name, 'Name');
    CheckEquals('', FDecodeTorrent.Comment, 'Comment');
    CheckEquals('', FDecodeTorrent.CreatedBy, 'CreatedBy');
    CheckEquals('', FDecodeTorrent.InfoSource, 'InfoSource');
    CheckEquals(0, FDecodeTorrent.PieceLength, 'PieceLength');
    CheckEquals(0, FDecodeTorrent.TotalFileSize, 'TotalFileSize');
    CheckEquals(0, FDecodeTorrent.InfoFilesCount, 'InfoFilesCount');
    CheckEquals(0, FDecodeTorrent.TrackerList.Count, 'TrackerList');
    CheckFalse(FDecodeTorrent.PrivateTorrent, 'PrivateTorrent');

    //Changing a torrent that is not decoded must fail, not use freed memory
    CheckFalse(FDecodeTorrent.AddPrivateTorrentFlag, 'AddPrivateTorrentFlag');
    CheckFalse(FDecodeTorrent.InfoSourceAdd('x'), 'InfoSourceAdd');
    FDecodeTorrent.Comment := 'new';
    CheckEquals('', FDecodeTorrent.Comment, 'A comment can not be set without a torrent');
    CheckFalse(FDecodeTorrent.ChangeAnnounce('udp://x.test/announce'), 'ChangeAnnounce');
    CheckFalse(FDecodeTorrent.SaveTorrent(GetTempDir + 'test_decodetorrent_not_saved.torrent'),
      'SaveTorrent');
    CheckFalse(FileExists(GetTempDir + 'test_decodetorrent_not_saved.torrent'),
      'No file may be written');
  end;
end;

{$IFDEF UNIX}
procedure TTestDecodeTorrent.Test_SaveTorrent_Keeps_Permissions_Of_Original;
const
  MODE_OTHER_THAN_DEFAULT = &640;
var
  TempFileName: string;
  Info: Stat;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');

  TempFileName := GetTempDir + 'test_decodetorrent_permissions.torrent';
  Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not save torrent the first time');
  try
    Check(FpChmod(TempFileName, MODE_OTHER_THAN_DEFAULT) = 0, 'Can not change permissions');

    Check(FDecodeTorrent.SaveTorrent(TempFileName), 'Can not overwrite the torrent');

    Check(FpStat(TempFileName, Info) = 0, 'Can not read file permissions');
    CheckEquals(MODE_OTHER_THAN_DEFAULT, Info.st_mode and &777,
      'Saving must keep the permissions of the original');
  finally
    DeleteFile(TempFileName);
  end;
end;
{$ENDIF}

initialization
  RegisterTest(TTestDecodeTorrent);
end.
