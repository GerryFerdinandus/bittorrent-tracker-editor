// SPDX-License-Identifier: MIT
unit test_decodetorrent;

{
  Decode torrent files that are build in memory.
  No torrent file on disk and no internet connection is needed for these tests.
}

{$mode objfpc}{$H+}

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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Single_File_V1_InfoHash;
    procedure Test_Single_File_V1_FileList;
    procedure Test_Multi_File_V1_InfoHash;
    procedure Test_Multi_File_V1_Entry_Missing_Length_Is_Rejected;
    procedure Test_Multi_File_V1_Entry_Missing_Path_Is_Rejected;
    procedure Test_Torrent_Without_Pieces_Has_No_InfoHash;
    procedure Test_Comment_Remove_Then_Add_Again;
  end;

implementation

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

  //Neither V1 'info.pieces' nor V2 'info.file tree' is present
  INFO_WITHOUT_PIECES = 'd6:lengthi1024e4:name8:test.bine';

  ANNOUNCE = '8:announce27:udp://tracker.test/announce';

  //SHA1 of INFO_SINGLE_FILE
  INFO_HASH_SINGLE_FILE = '7DBDCBCFBDD306C058848B78BC2048822BB4CBA6';

  //SHA1 of INFO_MULTI_FILE
  INFO_HASH_MULTI_FILE = '2575ADB45B1E904ADF75726BFD5C27FD76897C2B';

  NO_INFO_HASH = 'N/A';

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

initialization
  RegisterTest(TTestDecodeTorrent);
end.
