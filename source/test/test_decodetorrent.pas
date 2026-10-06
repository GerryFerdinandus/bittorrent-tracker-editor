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
  Classes, SysUtils, dateutils, fpcunit, testregistry, decodetorrent, BEncode,
  test_miscellaneous;

type

  { TTestDecodeTorrent }

  TTestDecodeTorrent = class(TTestCase)
  private
    FDecodeTorrent: TDecodeTorrent;

    //Decode a torrent that is present as a bencoded string
    function DecodeTorrentString(const TorrentStr: UTF8String): boolean;

    //Save FDecodeTorrent to a temp file and return the exact bytes that were written
    function SaveAndReadBack: UTF8String;

    function ReadWholeFile(const FileName: string): UTF8String;

    //Parse bencoded text. The caller must free the result.
    function ParseBEncoded(const Str: UTF8String): TBEncoded;

    //The keys of a dictionary must be in raw byte order.
    procedure CheckKeysSorted(Dict: TBEncoded; const Msg: string);
    function CountKeys(Dict: TBEncoded; const Key: UTF8String): integer;
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
    procedure Test_Failed_Decode_Clears_Previous_Torrent_State;
    procedure Test_New_Object_Has_Unknown_Version;
    procedure Test_RoundTrip_Real_Torrent_Files_Are_Identical;
    procedure Test_RoundTrip_V2_And_Hybrid_Are_Identical;
    procedure Test_V2_Version_Hashes_And_Metadata;
    procedure Test_V2_File_Tree_And_Padding;
    procedure Test_Hybrid_Uses_V2_Files_And_Detects_V1_Padding;
    procedure Test_V1_Padding_File_Is_Not_Counted;
    procedure Test_AnnounceList_Multiple_Tiers;
    procedure Test_AnnounceList_Url_Also_In_Announce_Is_Not_Duplicated;
    procedure Test_AnnounceList_Without_Announce;
    procedure Test_AnnounceList_Tier_With_Several_Trackers_Reads_First_Only;
    procedure Test_No_Announce_And_No_AnnounceList;
    procedure Test_ChangeAnnounce_Replaces_And_Keeps_Keys_Sorted;
    procedure Test_ChangeAnnounceList_One_Tracker_Per_Tier_And_Keys_Sorted;
    procedure Test_ChangeAnnounceList_Empty_Removes_The_List;
    procedure Test_RemoveAnnounce_And_RemoveAnnounceList;
    procedure Test_Private_Flag_Add_Remove_Keeps_Info_Keys_Sorted;
    procedure Test_InfoSource_Add_Change_Remove_Keeps_Info_Keys_Sorted;
    procedure Test_Empty_Input_Fails;
    procedure Test_Info_That_Is_Not_A_Dictionary_Fails;
    procedure Test_Every_Truncated_Torrent_Fails_And_Object_Can_Be_Reused;
    procedure Test_Truncated_And_Missing_File_Fail;
    procedure Test_CreatedBy_CreatedDate_Name_And_PieceLength;
    procedure Test_Missing_CreatedBy_And_CreatedDate_Are_Empty;
    procedure Test_InfoHash_Is_Recalculated_After_Private_Flag_And_Source_Change;
    procedure Test_InfoHash_V1_And_V2_Are_Recalculated_For_Hybrid;    {$IFDEF UNIX}
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

  TRACKER_2 = 'udp://b.test/announce';
  TRACKER_3 = 'udp://c.test/announce';

  //V2 file tree: a.txt (100), dir/b.bin (2000) and zpad (50, a padding file)
  TREE_V2 =
    'd5:a.txtd0:d6:lengthi100eee3:dird5:b.bind0:d6:lengthi2000eeee' +
    '4:zpadd0:d4:attr1:p6:lengthi50eeee';

  //V2 only: 'file tree' and no 'pieces'
  INFO_V2 = 'd9:file tree' + TREE_V2 + '12:meta versioni2e4:name4:root' +
    '12:piece lengthi16384ee';
  //Calculated with an other program (Python hashlib)
  INFO_HASH_V2_SHA256 =
    'ED179DC8F0BE69EA2CE8624D450D37D711CF85590E3B29E8B465551C1EC9CC66';

  //Hybrid: the V2 tree has 2 files, the V1 list has a third padding file
  TREE_HYBRID = 'd5:a.txtd0:d6:lengthi100eee3:dird5:b.bind0:d6:lengthi2000eeeee';
  FILES_HYBRID = 'ld6:lengthi100e4:pathl5:a.txteed6:lengthi2000e4:pathl3:dir5:b.bineed' +
    '4:attr1:p6:lengthi50e4:pathl4:.pad2:50eee';
  INFO_HYBRID_PADDING = 'd9:file tree' + TREE_HYBRID + '5:files' + FILES_HYBRID +
    '12:meta versioni2e4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';
  INFO_HASH_HYBRID_SHA1 = '3AB309F4309D70831FFC02CDD9FFF8B77DE6C052';
  INFO_HASH_HYBRID_SHA256 =
    '8C262A68A7309035CE5F9D5E238709F5B9F6240F0462416C5365343FE0E64BEA';

  //V1 torrent where the second file is a padding file
  INFO_V1_PADDING =
    'd5:filesld6:lengthi100e4:pathl5:a.txteed4:attr1:p6:lengthi28e4:pathl4:.pad2:28eee' +
    '4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';

  //Keys after 'private' and after 'source': adding them must put them in the middle
  INFO_TRAILING_KEYS = 'd6:lengthi1024e4:name8:test.bin12:piece lengthi16384e6:pieces' +
    PIECES + '9:publisher3:abc7:x-after1:ye';

  //The torrent files that are used by the other tests, in the folder test_torrent
  REAL_TORRENT_FILES: array[0..4] of string = (
    'Sintel.2010.2K.SURROUND.x264-VODO.torrent',
    'Sintel.2010.2K.Theora.Ogv-VODO.torrent',
    'Sintel.2010.720p.SURROUND.x264-VODO.torrent',
    'bittorrent-v2-test.torrent',
    'bittorrent-v2-hybrid-test.torrent');

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

function TTestDecodeTorrent.ReadWholeFile(const FileName: string): UTF8String;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function TTestDecodeTorrent.ParseBEncoded(const Str: UTF8String): TBEncoded;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Str[1], Length(Str));
    Stream.Position := 0;
    Result := TBEncoded.Create(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTestDecodeTorrent.CheckKeysSorted(Dict: TBEncoded; const Msg: string);
var
  i: integer;
begin
  for i := 1 to Dict.ListData.Count - 1 do
    Check(CompareStr(Dict.ListData[i - 1].Header, Dict.ListData[i].Header) < 0,
      Msg + ': key ''' + Dict.ListData[i].Header + ''' is not in byte order');
end;

function TTestDecodeTorrent.CountKeys(Dict: TBEncoded; const Key: UTF8String): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Dict.ListData.Count - 1 do
    if Dict.ListData[i].Header = Key then
      Inc(Result);
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

procedure TTestDecodeTorrent.Test_InfoHash_Is_Recalculated_After_Private_Flag_And_Source_Change;
var
  Original, Private_, WithSource: UTF8String;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode a torrent with one file');
  Original := FDecodeTorrent.InfoHash_V1;

  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');
  Private_ := FDecodeTorrent.InfoHash_V1;
  CheckNotEquals(Original, Private_, 'The hash must change with the private flag');

  Check(FDecodeTorrent.InfoSourceAdd('SRC'), 'Can not add the source');
  WithSource := FDecodeTorrent.InfoHash_V1;
  CheckNotEquals(Private_, WithSource, 'The hash must change with the source');

  //The shown hash must be the hash that a client calculates from the saved file
  Check(DecodeTorrentString(SaveAndReadBack), 'Can not decode the saved torrent');
  CheckEquals(WithSource, FDecodeTorrent.InfoHash_V1, 'Hash must match the saved file');

  //Removing both gives the original info dictionary and the original hash
  FDecodeTorrent.InfoSourceRemove;
  FDecodeTorrent.RemovePrivateTorrentFlag;
  CheckEquals(Original, FDecodeTorrent.InfoHash_V1, 'Hash must be the original again');
end;

procedure TTestDecodeTorrent.Test_InfoHash_V1_And_V2_Are_Recalculated_For_Hybrid;
const
  //V1 'files' and V2 'file tree' are both present
  INFO_HYBRID = 'd9:file treed5:a.txtd0:d6:lengthi100eeee5:filesld6:lengthi100e' +
    '4:pathl5:a.txteee4:name4:root12:piece lengthi16384e6:pieces' + PIECES + 'e';
var
  OriginalV1, OriginalV2, ChangedV1, ChangedV2: UTF8String;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_HYBRID)), 'Can not decode a hybrid torrent');
  CheckEquals(Ord(tv_Hybrid), Ord(FDecodeTorrent.TorrentVersion), 'Must be hybrid');
  OriginalV1 := FDecodeTorrent.InfoHash_V1;
  OriginalV2 := FDecodeTorrent.InfoHash_V2;

  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');
  ChangedV1 := FDecodeTorrent.InfoHash_V1;
  ChangedV2 := FDecodeTorrent.InfoHash_V2;
  CheckNotEquals(OriginalV1, ChangedV1, 'V1 hash must change');
  CheckNotEquals(OriginalV2, ChangedV2, 'V2 hash must change');

  //The shown hashes must be the hashes of the saved file
  Check(DecodeTorrentString(SaveAndReadBack), 'Can not decode the saved torrent');
  CheckEquals(ChangedV1, FDecodeTorrent.InfoHash_V1, 'Saved V1 hash must match');
  CheckEquals(ChangedV2, FDecodeTorrent.InfoHash_V2, 'Saved V2 hash must match');
end;

procedure TTestDecodeTorrent.Test_New_Object_Has_Unknown_Version;
begin
  CheckEquals(Ord(tv_unknown), Ord(FDecodeTorrent.TorrentVersion), 'Version');
  CheckEquals('unknown', FDecodeTorrent.TorrentVersionToString, 'Version text');
  CheckEquals('N/A', FDecodeTorrent.PaddingToString, 'Padding text');
  CheckEquals(0, FDecodeTorrent.InfoFilesCount, 'File count');
end;

procedure TTestDecodeTorrent.Test_RoundTrip_Real_Torrent_Files_Are_Identical;
var
  Name: string;
  FileName: string;
  Original, Saved: UTF8String;
  HashV1, HashV2: UTF8String;
begin
  //Decode and save without any change must not change a single byte.
  //Else the info hash changes and the torrent becomes a different torrent.
  for Name in REAL_TORRENT_FILES do
  begin
    FileName := GetProjectRootFolderWithPathDelimiter + 'test_torrent' + PathDelim + Name;
    Check(FileExists(FileName), 'Missing test torrent ' + FileName);
    Original := ReadWholeFile(FileName);

    Check(FDecodeTorrent.DecodeTorrent(FileName), 'Can not decode ' + FileName);
    HashV1 := FDecodeTorrent.InfoHash_V1;
    HashV2 := FDecodeTorrent.InfoHash_V2;
    Saved := SaveAndReadBack;

    CheckEquals(Length(Original), Length(Saved), 'Saved size differs: ' + FileName);
    Check(Original = Saved, 'Saved bytes differ: ' + FileName);

    //The hashes of the saved file are the hashes of the original
    Check(DecodeTorrentString(Saved), 'Can not decode the saved ' + FileName);
    CheckEquals(HashV1, FDecodeTorrent.InfoHash_V1, 'V1 hash differs: ' + FileName);
    CheckEquals(HashV2, FDecodeTorrent.InfoHash_V2, 'V2 hash differs: ' + FileName);
  end;
end;

procedure TTestDecodeTorrent.Test_RoundTrip_V2_And_Hybrid_Are_Identical;
const
  INFOS: array[0..1] of UTF8String = (INFO_V2, INFO_HYBRID_PADDING);
var
  Torrent: UTF8String;
  Info: UTF8String;
begin
  for Info in INFOS do
  begin
    Torrent := BuildTorrent(Info);
    Check(DecodeTorrentString(Torrent), 'Can not decode the torrent');
    Check(Torrent = SaveAndReadBack, 'Saved bytes differ');
  end;
end;

procedure TTestDecodeTorrent.Test_V2_Version_Hashes_And_Metadata;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_V2)), 'Can not decode a V2 torrent');

  CheckEquals(Ord(tv_V2), Ord(FDecodeTorrent.TorrentVersion), 'Version');
  CheckEquals('V2', FDecodeTorrent.TorrentVersionToString, 'Version text');
  CheckEquals(2, FDecodeTorrent.MetaVersion, 'Meta version');
  CheckEquals(INFO_HASH_V2_SHA256, FDecodeTorrent.InfoHash_V2, 'V2 info hash');
  CheckEquals(NO_INFO_HASH, FDecodeTorrent.InfoHash_V1, 'A V2 torrent has no V1 hash');
  CheckEquals('root', FDecodeTorrent.Name, 'Name');
  CheckEquals(16384, FDecodeTorrent.PieceLength, 'Piece length');
end;

procedure TTestDecodeTorrent.Test_V2_File_Tree_And_Padding;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_V2)), 'Can not decode a V2 torrent');

  CheckEquals(2, FDecodeTorrent.InfoFilesVersion, 'Files version');
  CheckEquals(3, FDecodeTorrent.InfoFilesCount, 'File count');
  CheckEquals(DirectorySeparator + 'a.txt', FDecodeTorrent.InfoFilesNameIndex(0), 'File 0');
  CheckEquals(100, FDecodeTorrent.InfoFilesLengthIndex(0), 'Length 0');
  CheckEquals(DirectorySeparator + 'dir' + DirectorySeparator + 'b.bin',
    FDecodeTorrent.InfoFilesNameIndex(1), 'File 1');
  CheckEquals(2000, FDecodeTorrent.InfoFilesLengthIndex(1), 'Length 1');
  CheckEquals(DirectorySeparator + 'zpad', FDecodeTorrent.InfoFilesNameIndex(2), 'File 2');
  CheckEquals(50, FDecodeTorrent.InfoFilesLengthIndex(2), 'Length 2');

  //The padding file is listed, but not counted
  CheckEquals(2100, FDecodeTorrent.TotalFileSize, 'Total size');
  CheckTrue(FDecodeTorrent.PaddingPresent_V2, 'Padding V2');
  CheckFalse(FDecodeTorrent.PaddingPresent_V1, 'Padding V1');
  CheckEquals('Yes', FDecodeTorrent.PaddingToString, 'Padding text');
end;

procedure TTestDecodeTorrent.Test_Hybrid_Uses_V2_Files_And_Detects_V1_Padding;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_HYBRID_PADDING)),
    'Can not decode a hybrid torrent');

  CheckEquals(Ord(tv_Hybrid), Ord(FDecodeTorrent.TorrentVersion), 'Version');
  CheckEquals('Hybrid (V1&V2)', FDecodeTorrent.TorrentVersionToString, 'Version text');
  CheckEquals(INFO_HASH_HYBRID_SHA1, FDecodeTorrent.InfoHash_V1, 'V1 info hash');
  CheckEquals(INFO_HASH_HYBRID_SHA256, FDecodeTorrent.InfoHash_V2, 'V2 info hash');
  CheckEquals(2, FDecodeTorrent.MetaVersion, 'Meta version');

  //Only the V2 files are used, the V1 list is only read for the padding
  CheckEquals(2, FDecodeTorrent.InfoFilesVersion, 'Files version');
  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'File count');
  CheckEquals(2100, FDecodeTorrent.TotalFileSize, 'Total size');
  CheckTrue(FDecodeTorrent.PaddingPresent_V1, 'Padding V1');
  CheckFalse(FDecodeTorrent.PaddingPresent_V2, 'Padding V2');
  CheckEquals('V1:Yes V2:No', FDecodeTorrent.PaddingToString, 'Padding text');
end;

procedure TTestDecodeTorrent.Test_V1_Padding_File_Is_Not_Counted;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_V1_PADDING)), 'Can not decode a V1 torrent');

  CheckEquals('V1', FDecodeTorrent.TorrentVersionToString, 'Version text');
  CheckEquals(0, FDecodeTorrent.MetaVersion, 'A V1 torrent has no meta version');
  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'The padding file is listed');
  CheckEquals(100, FDecodeTorrent.TotalFileSize, 'The padding file is not counted');
  CheckTrue(FDecodeTorrent.PaddingPresent_V1, 'Padding V1');
  CheckEquals('Yes', FDecodeTorrent.PaddingToString, 'Padding text');

  //No padding
  Check(DecodeTorrentString(BuildTorrent(INFO_MULTI_FILE)), 'Can not decode a V1 torrent');
  CheckFalse(FDecodeTorrent.PaddingPresent_V1, 'No padding V1');
  CheckEquals('No', FDecodeTorrent.PaddingToString, 'No padding text');
end;

procedure TTestDecodeTorrent.Test_AnnounceList_Multiple_Tiers;
begin
  Check(DecodeTorrentString('d' + ANNOUNCE + '13:announce-listl' +
    'l' + BEncodeString(TRACKER_2) + 'e' + 'l' + BEncodeString(TRACKER_3) + 'ee' +
    '4:info' + INFO_SINGLE_FILE + 'e'), 'Can not decode the torrent');

  CheckEquals(3, FDecodeTorrent.TrackerList.Count, 'Tracker count');
  CheckEquals('udp://tracker.test/announce', FDecodeTorrent.TrackerList[0], 'Tracker 0');
  CheckEquals(TRACKER_2, FDecodeTorrent.TrackerList[1], 'Tracker 1');
  CheckEquals(TRACKER_3, FDecodeTorrent.TrackerList[2], 'Tracker 2');
end;

procedure TTestDecodeTorrent.Test_AnnounceList_Url_Also_In_Announce_Is_Not_Duplicated;
begin
  Check(DecodeTorrentString('d' + ANNOUNCE + '13:announce-listl' +
    'l' + BEncodeString('udp://tracker.test/announce') + 'e' +
    'l' + BEncodeString(TRACKER_2) + 'ee' +
    '4:info' + INFO_SINGLE_FILE + 'e'), 'Can not decode the torrent');

  CheckEquals(2, FDecodeTorrent.TrackerList.Count, 'Tracker count');
  CheckEquals('udp://tracker.test/announce', FDecodeTorrent.TrackerList[0], 'Tracker 0');
  CheckEquals(TRACKER_2, FDecodeTorrent.TrackerList[1], 'Tracker 1');
end;

procedure TTestDecodeTorrent.Test_AnnounceList_Without_Announce;
begin
  Check(DecodeTorrentString('d13:announce-listl' +
    'l' + BEncodeString(TRACKER_2) + 'e' + 'l' + BEncodeString(TRACKER_3) + 'ee' +
    '4:info' + INFO_SINGLE_FILE + 'e'), 'Can not decode the torrent');

  CheckEquals(2, FDecodeTorrent.TrackerList.Count, 'Tracker count');
  CheckEquals(TRACKER_2, FDecodeTorrent.TrackerList[0], 'Tracker 0');
  CheckEquals(TRACKER_3, FDecodeTorrent.TrackerList[1], 'Tracker 1');
end;

procedure TTestDecodeTorrent.Test_AnnounceList_Tier_With_Several_Trackers_Reads_First_Only;
begin
  //Design choice, see the header of decodetorrent.pas: one tracker per tier.
  Check(DecodeTorrentString('d13:announce-listl' +
    'l' + BEncodeString(TRACKER_2) + BEncodeString(TRACKER_3) + 'ee' +
    '4:info' + INFO_SINGLE_FILE + 'e'), 'Can not decode the torrent');

  CheckEquals(1, FDecodeTorrent.TrackerList.Count, 'Tracker count');
  CheckEquals(TRACKER_2, FDecodeTorrent.TrackerList[0], 'Tracker 0');
end;

procedure TTestDecodeTorrent.Test_No_Announce_And_No_AnnounceList;
begin
  Check(DecodeTorrentString('d4:info' + INFO_SINGLE_FILE + 'e'),
    'A torrent without trackers must decode');
  CheckEquals(0, FDecodeTorrent.TrackerList.Count, 'Tracker count');
end;

procedure TTestDecodeTorrent.Test_ChangeAnnounce_Replaces_And_Keeps_Keys_Sorted;
var
  Root: TBEncoded;
begin
  //Without 'announce' the new key must be sorted in front of 'comment'
  Check(DecodeTorrentString('d7:comment3:abc10:created by2:me4:info' + INFO_SINGLE_FILE +
    'e'), 'Can not decode the torrent');

  Check(FDecodeTorrent.ChangeAnnounce(TRACKER_2), 'Can not add the announce');
  Check(FDecodeTorrent.ChangeAnnounce(TRACKER_3), 'Can not replace the announce');

  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckKeysSorted(Root, 'Root');
    CheckEquals(1, CountKeys(Root, 'announce'), 'There must be one announce');
    CheckEquals(TRACKER_3, Root.ListData.FindElement('announce').StringData,
      'Wrong announce');
    CheckEquals('abc', Root.ListData.FindElement('comment').StringData,
      'The comment must stay');
  finally
    Root.Free;
  end;
end;

procedure TTestDecodeTorrent.Test_ChangeAnnounceList_One_Tracker_Per_Tier_And_Keys_Sorted;
var
  Root, AnnounceList: TBEncoded;
  Trackers: TStringList;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)),
    'Can not decode the torrent');

  Trackers := TStringList.Create;
  try
    Trackers.Add(TRACKER_2);
    Trackers.Add(TRACKER_3);
    Check(FDecodeTorrent.ChangeAnnounceList(Trackers), 'Can not change the announce-list');
    //Again, the old list must be replaced, not added
    Check(FDecodeTorrent.ChangeAnnounceList(Trackers), 'Can not change it again');
  finally
    Trackers.Free;
  end;

  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckKeysSorted(Root, 'Root');
    CheckEquals(1, CountKeys(Root, 'announce-list'), 'There must be one announce-list');
    AnnounceList := Root.ListData.FindElement('announce-list');
    CheckEquals(2, AnnounceList.ListData.Count, 'Tier count');
    CheckEquals(1, AnnounceList.ListData[0].Data.ListData.Count, 'Size of tier 0');
    CheckEquals(TRACKER_2, AnnounceList.ListData[0].Data.ListData.First.Data.StringData,
      'Tier 0');
    CheckEquals(TRACKER_3, AnnounceList.ListData[1].Data.ListData.First.Data.StringData,
      'Tier 1');
  finally
    Root.Free;
  end;
end;

procedure TTestDecodeTorrent.Test_ChangeAnnounceList_Empty_Removes_The_List;
var
  Trackers: TStringList;
  Root: TBEncoded;
begin
  Check(DecodeTorrentString('d' + ANNOUNCE + '13:announce-listl' + 'l' +
    BEncodeString(TRACKER_2) + 'ee4:info' + INFO_SINGLE_FILE + 'e'),
    'Can not decode the torrent');

  Trackers := TStringList.Create;
  try
    Check(FDecodeTorrent.ChangeAnnounceList(Trackers), 'An empty list must be accepted');
  finally
    Trackers.Free;
  end;

  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckEquals(0, CountKeys(Root, 'announce-list'), 'The announce-list must be removed');
    CheckEquals(1, CountKeys(Root, 'announce'), 'The announce must stay');
  finally
    Root.Free;
  end;
end;

procedure TTestDecodeTorrent.Test_RemoveAnnounce_And_RemoveAnnounceList;
var
  Root: TBEncoded;
begin
  Check(DecodeTorrentString('d' + ANNOUNCE + '13:announce-listl' + 'l' +
    BEncodeString(TRACKER_2) + 'ee4:info' + INFO_SINGLE_FILE + 'e'),
    'Can not decode the torrent');

  Check(FDecodeTorrent.RemoveAnnounce, 'Can not remove the announce');
  Check(FDecodeTorrent.RemoveAnnounceList, 'Can not remove the announce-list');
  //Nothing left to remove is not an error
  Check(FDecodeTorrent.RemoveAnnounce, 'Remove of a missing announce');
  Check(FDecodeTorrent.RemoveAnnounceList, 'Remove of a missing announce-list');

  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckEquals(0, CountKeys(Root, 'announce'), 'The announce must be removed');
    CheckEquals(0, CountKeys(Root, 'announce-list'), 'The announce-list must be removed');
    CheckEquals(1, CountKeys(Root, 'info'), 'The info must stay');
  finally
    Root.Free;
  end;
end;

procedure TTestDecodeTorrent.Test_Private_Flag_Add_Remove_Keeps_Info_Keys_Sorted;
var
  Root: TBEncoded;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_TRAILING_KEYS)),
    'Can not decode the torrent');

  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');
  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckKeysSorted(Root.ListData.FindElement('info'), 'Info');
    CheckEquals(1, CountKeys(Root.ListData.FindElement('info'), 'private'),
      'There must be one private key');
  finally
    Root.Free;
  end;

  //Remove it again: the original torrent, byte for byte
  Check(FDecodeTorrent.RemovePrivateTorrentFlag, 'Can not remove the private flag');
  CheckFalse(FDecodeTorrent.PrivateTorrent, 'Torrent must be public');
  Check(BuildTorrent(INFO_TRAILING_KEYS) = SaveAndReadBack, 'Must be the original torrent');
end;

procedure TTestDecodeTorrent.Test_InfoSource_Add_Change_Remove_Keeps_Info_Keys_Sorted;
var
  Root: TBEncoded;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_TRAILING_KEYS)),
    'Can not decode the torrent');
  Check(FDecodeTorrent.AddPrivateTorrentFlag, 'Can not add the private flag');

  Check(FDecodeTorrent.InfoSourceAdd('abc'), 'Can not add the source');
  CheckEquals('abc', FDecodeTorrent.InfoSource, 'Source');
  Check(FDecodeTorrent.InfoSourceAdd('xyz'), 'Can not change the source');
  CheckEquals('xyz', FDecodeTorrent.InfoSource, 'Changed source');

  Root := ParseBEncoded(SaveAndReadBack);
  try
    CheckKeysSorted(Root.ListData.FindElement('info'), 'Info');
    CheckEquals(1, CountKeys(Root.ListData.FindElement('info'), 'source'),
      'There must be one source key');
    CheckEquals('xyz', Root.ListData.FindElement('info').ListData.FindElement('source').StringData,
      'Wrong source');
  finally
    Root.Free;
  end;

  Check(FDecodeTorrent.InfoSourceRemove, 'Can not remove the source');
  CheckEquals('', FDecodeTorrent.InfoSource, 'Source must be empty');
  Check(FDecodeTorrent.RemovePrivateTorrentFlag, 'Can not remove the private flag');
  Check(BuildTorrent(INFO_TRAILING_KEYS) = SaveAndReadBack, 'Must be the original torrent');
end;

procedure TTestDecodeTorrent.Test_Empty_Input_Fails;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    CheckFalse(FDecodeTorrent.DecodeTorrent(Stream), 'An empty stream must fail');
  finally
    Stream.Free;
  end;
  CheckEquals(Ord(tv_unknown), Ord(FDecodeTorrent.TorrentVersion), 'Version');
end;

procedure TTestDecodeTorrent.Test_Info_That_Is_Not_A_Dictionary_Fails;
begin
  CheckFalse(DecodeTorrentString('d4:infoi5ee'), 'An integer as info must fail');
  CheckFalse(DecodeTorrentString('d4:info3:abce'), 'A string as info must fail');
  CheckFalse(DecodeTorrentString('d4:infolee'), 'A list as info must fail');
end;

procedure TTestDecodeTorrent.Test_Every_Truncated_Torrent_Fails_And_Object_Can_Be_Reused;
var
  Torrent: UTF8String;
  Len: integer;
begin
  Torrent := BuildTorrent(INFO_MULTI_FILE);

  for Len := 1 to Length(Torrent) - 1 do
    CheckFalse(DecodeTorrentString(Copy(Torrent, 1, Len)),
      'A torrent cut at ' + IntToStr(Len) + ' bytes must fail');

  //The object still works after all the failures
  Check(DecodeTorrentString(Torrent), 'The complete torrent must decode');
  CheckEquals(2, FDecodeTorrent.InfoFilesCount, 'File count');
  CheckEquals(1, FDecodeTorrent.TrackerList.Count, 'Tracker count');
end;

procedure TTestDecodeTorrent.Test_Truncated_And_Missing_File_Fail;
var
  FileName: string;
  Torrent: UTF8String;
  Stream: TFileStream;
begin
  FileName := GetTempDir + 'test_decodetorrent_truncated.torrent';
  Torrent := BuildTorrent(INFO_MULTI_FILE);
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    //Only the first half of the torrent
    Stream.WriteBuffer(Torrent[1], Length(Torrent) div 2);
  finally
    Stream.Free;
  end;

  try
    CheckFalse(FDecodeTorrent.DecodeTorrent(FileName), 'A truncated file must fail');
  finally
    DeleteFile(FileName);
  end;

  CheckFalse(FDecodeTorrent.DecodeTorrent(FileName), 'A missing file must fail');
  CheckEquals(Ord(tv_unknown), Ord(FDecodeTorrent.TorrentVersion), 'Version');
  CheckEquals('', FDecodeTorrent.FilenameTorrent, 'No file name after a failure');
end;

procedure TTestDecodeTorrent.Test_CreatedBy_CreatedDate_Name_And_PieceLength;
begin
  Check(DecodeTorrentString('d10:created by6:tester13:creation datei1700000000e4:info' +
    INFO_SINGLE_FILE + 'e'), 'Can not decode the torrent');

  CheckEquals('tester', FDecodeTorrent.CreatedBy, 'Created by');
  //1700000000 is 2023-11-14 22:13:20 UTC
  CheckEquals(EncodeDate(2023, 11, 14) + EncodeTime(22, 13, 20, 0),
    FDecodeTorrent.CreatedDate, 1 / MSecsPerDay, 'Creation date');
  CheckEquals('test.bin', FDecodeTorrent.Name, 'Name');
  CheckEquals(16384, FDecodeTorrent.PieceLength, 'Piece length');
end;

procedure TTestDecodeTorrent.Test_Missing_CreatedBy_And_CreatedDate_Are_Empty;
begin
  Check(DecodeTorrentString(BuildTorrent(INFO_SINGLE_FILE)), 'Can not decode the torrent');

  CheckEquals('', FDecodeTorrent.CreatedBy, 'Created by');
  CheckEquals(0, FDecodeTorrent.CreatedDate, 1 / MSecsPerDay, 'Creation date');
  CheckEquals('', FDecodeTorrent.Comment, 'Comment');
  CheckEquals(0, FDecodeTorrent.MetaVersion, 'Meta version');
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
