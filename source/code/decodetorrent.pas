// SPDX-License-Identifier: MIT
unit DecodeTorrent;

{$mode objfpc}{$H+}
{
 Get all the tracker list inside the torrent file.
 And place it in TrackerList: TStringList

 Modify public/private flag

 Modify announce List


 You can use bencode editor and utorrent to verify the output of this program.
 https://sites.google.com/site/ultimasites/bencode-editor
  }
interface

uses
  Classes, SysUtils, contnrs, BEncode;

type

  TTorrentVersion = (
    tv_V1,            //< V1 version
    tv_V2,            //< V2 version
    tv_Hybrid,        //< V1 + V2 hybrid
    tv_unknown        //< Can not decode it
    );

  TDecodeTorrentFileNameAndLength = class
    Filename: utf8string;
    FileLength: int64;
  end;


  { TDecodeTorrent }
  TDecodeTorrent = class
  private
    FFilenameTorrent: utf8string;
    FMemoryStream: TMemoryStream;
    FBEncoded: TBEncoded;
    FObjectListFileNameAndLength: TObjectList;
    FTotalFileSize: int64;
    FTorrentVersion: TTorrentVersion;

    //Torrent file must have 'info' item.
    FBEncoded_Info: TBEncoded;
    FBEncoded_Comment: TBEncoded;

    FInfoHash_V1: utf8string;
    FInfoHash_V2: utf8string;
    FCreatedBy: utf8string;
    FCreatedDate: TDateTime;
    FComment: utf8string;
    FInfoSource: utf8string;
    FName: utf8string;
    FPieceLenght: int64;
    FMetaVersion: int64;
    FPrivateTorrent: boolean;
    FPaddingPresent_V1: boolean;
    FPaddingPresent_V2: boolean;
    FInfoFilesVersion: integer;

    function DecodeTorrent: boolean; overload;

    function isPresentInfoFiles_V1: boolean;
    function isPresentInfoFileTree_V2: boolean;

    function GetSha256(const Source: utf8string): utf8string;
    function GetMetaVersion: int64;
    function GetAnnounceList: boolean;
    function GetFileList_V1: boolean;
    function GetFileList_V2: boolean;
    function GetOneFileTorrent: boolean;
    function GetCreatedBy: utf8string;
    function GetCreatedDate: TDateTime;
    function GetComment: utf8string;
    function GetName: utf8string;
    function GetPieceLenght: int64;
    function GetPrivateTorrent: boolean;
    function GetInfoSource: utf8string;
    procedure SetComment(const AValue: utf8string);
  public
    //All the trackers inside this torrent file
    TrackerList: TStringList;

    property MetaVersion: int64 read FMetaVersion;
    property TorrentVersion: TTorrentVersion read FTorrentVersion;
    property PaddingPresent_V1: boolean read FPaddingPresent_V1;
    property PaddingPresent_V2: boolean read FPaddingPresent_V2;

    property FilenameTorrent: utf8string read FFilenameTorrent;

    property TotalFileSize: int64 read FTotalFileSize;

    //Info hash
    property InfoHash_V1: utf8string read FInfoHash_V1;
    property InfoHash_V2: utf8string read FInfoHash_V2;

    //Created by
    property CreatedBy: utf8string read FCreatedBy;

    //Create Date
    property CreatedDate: TDateTime read FCreatedDate;

    //Comment
    property Comment: utf8string read FComment write SetComment;

    //info.name
    property Name: utf8string read FName;

    //info.piecelength
    property PieceLenght: int64 read FPieceLenght;

    //public/private flag
    property PrivateTorrent: boolean read FPrivateTorrent;
    function RemovePrivateTorrentFlag: boolean;
    function AddPrivateTorrentFlag: boolean;

    //info.source
    property InfoSource: utf8string read FInfoSource;
    function InfoSourceRemove: boolean;
    function InfoSourceAdd(const Value: utf8string): boolean;

    //info.files and info. file tree
    property InfoFilesVersion: integer read FInfoFilesVersion;
    function InfoFilesCount: integer;
    function InfoFilesNameIndex(index: integer): utf8string;
    function InfoFilesLengthIndex(index: integer): int64;

    //Announce list
    function RemoveAnnounce: boolean;
    function RemoveAnnounceList: boolean;
    function ChangeAnnounce(const TrackerURL: utf8string): boolean;
    function ChangeAnnounceList(StringList: TStringList): boolean;

    function TorrentVersionToString: utf8string;
    function PaddingToString: utf8string;

    //Load torrent file
    function DecodeTorrent(const Filename: utf8string): boolean; overload;
    function DecodeTorrent(Stream: TStream): boolean; overload;

    //save torrent file
    function SaveTorrent(const Filename: utf8string): boolean;

    //create/destroy class object
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses dateutils, SHA1, DCPsha256, FileUtil, LazUTF8;

function SortFileName(Item1, Item2: Pointer): integer;
begin
  Result := UTF8CompareText(TDecodeTorrentFileNameAndLength(Item1).Filename,
    TDecodeTorrentFileNameAndLength(Item2).Filename);
end;


function Sort_(Item1, Item2: Pointer): integer;
begin
  Result := UTF8CompareText(TBEncodedData(Item1).Header, TBEncodedData(Item2).Header);
end;

{ TDecodeTorrent }
constructor TDecodeTorrent.Create;
begin
  inherited;
  //Every torrent have file list
  FObjectListFileNameAndLength := TObjectList.Create;

  //List for all the trackers.
  TrackerList := TStringList.Create;
  TrackerList.Duplicates := dupIgnore;
  //Must keep the original order.
  TrackerList.Sorted := False;

  FMemoryStream := TMemoryStream.Create;

end;

destructor TDecodeTorrent.Destroy;
begin
  //Free all the memory we have create.
  FObjectListFileNameAndLength.Free;
  TrackerList.Free;
  FMemoryStream.Free;

  //FBEncoded may or may not be created.
  if assigned(FBEncoded) then
    FBEncoded.Free;
  inherited;
end;

function TDecodeTorrent.DecodeTorrent(const Filename: utf8string): boolean;
var
  S: TFileStream;
begin
  FFilenameTorrent := Filename;
  //Load torrent file in FMemoryStream. This will be process by DecodeTorrent();
  try
    //Support FilenameTorrent with unicode.
    S := TFileStream.Create(FilenameTorrent, fmOpenRead or fmShareDenyWrite);
    try
      FMemoryStream.LoadFromStream(S);
    finally
      S.Free;
    end;
    Result := DecodeTorrent();
  except
    Result := False;
  end;
end;

function TDecodeTorrent.DecodeTorrent(Stream: TStream): boolean;
begin
  //copy Stream -> FMemoryStream
  try
    FMemoryStream.Clear;
    Stream.Seek(0, soBeginning);
    FMemoryStream.LoadFromStream(Stream);
    Result := DecodeTorrent();
  except;
    Result := False;
  end;
end;

function TDecodeTorrent.GetAnnounceList: boolean;
var
  TempBEncoded: TBEncoded;
  i, Count: integer;
  TrackerStr: utf8string;
begin
  //return false, if crash at decoding. Announce is optional in torrent file.
  TrackerList.Clear;
  Result := True;
  try
    {find 'announce-list' and copy the list content to TrackerList}
    //process 'announce'
    TempBEncoded := FBEncoded.ListData.FindElement('announce');
    if assigned(TempBEncoded) then
    begin
      TrackerList.Add(TempBEncoded.StringData);
    end;

    //process 'announce-list'
    TempBEncoded := FBEncoded.ListData.FindElement('announce-list');
    if assigned(TempBEncoded) then
    begin
      Count := TempBEncoded.ListData.Count;
      if Count > 0 then
      begin
        for i := 0 to Count - 1 do
        begin
          //there is a list in side a list!
          TrackerStr := TempBEncoded.ListData.Items[
            i].Data.ListData.First.Data.StringData;

          // TrackerList is not sorted. Must use IndexOf to ignore duplicated enteries.
          if TrackerList.IndexOf(TrackerStr) < 0 then
          begin
            TrackerList.Add(TrackerStr);
          end;

        end;
      end;
    end;

  except
    Result := False;
  end;
end;

function TDecodeTorrent.GetFileList_V1: boolean;
var
  P_File: Pointer;
  P_Item: Pointer;
  P_Path: Pointer;
  FileLength: int64;
  InfoFiles: TBEncoded;
  NodeData: TBEncodedData;
  ThisIsFileWithPadding: boolean;
  FilenameWithPathStr: utf8string;
  DecodeTorrentFileName: TDecodeTorrentFileNameAndLength;
begin
  // return false if there is no file at all. This must not be posible.
  // info/files/path -> all the files names
  FInfoFilesVersion := 1;
  FObjectListFileNameAndLength.Clear;
  FTotalFileSize := 0;

  try
    {find 'info.files' }
    InfoFiles := FBEncoded_Info.ListData.FindElement('files');

    if assigned(InfoFiles) then
    begin //'info.files' found

      for P_File in InfoFiles.ListData do
      begin // Every file need to be process one by one

        ThisIsFileWithPadding := False;
        for P_Item in TBEncodedData(P_File).Data.ListData do
        begin // Every item inside the file must be process one by one
          NodeData := TBEncodedData(P_Item);

          // Get the file name with path
          if NodeData.Header = 'path' then
          begin
            FilenameWithPathStr := '';
            for P_Path in NodeData.Data.ListData do
            begin
              FilenameWithPathStr :=
                FilenameWithPathStr + DirectorySeparator + TBEncodedData(
                P_Path).Data.StringData;
            end;
            Continue;
          end;

          // Get the file length
          if NodeData.Header = 'length' then
          begin
            FileLength := NodeData.Data.IntegerData;
            Continue;
          end;

          // check if this is padding
          if NodeData.Header = 'attr' then
          begin
            ThisIsFileWithPadding := UTF8Pos('p', NodeData.Data.StringData) > 0;
            if ThisIsFileWithPadding and not FPaddingPresent_V1 then
            begin
              FPaddingPresent_V1 := True;
            end;
          end;
        end; // Every item inside the file

        // one file is decoded. Now add it to the list
        DecodeTorrentFileName := TDecodeTorrentFileNameAndLength.Create;
        DecodeTorrentFileName.Filename := FilenameWithPathStr;
        DecodeTorrentFileName.FileLength := FileLength;
        FObjectListFileNameAndLength.Add(DecodeTorrentFileName);

        //add FileLength to the total sum of all files inside the torrent.
        if not ThisIsFileWithPadding then
        begin
          Inc(FTotalFileSize, FileLength);
        end;

      end; // Every file need to be process one by one
    end; //'info.files' found

    //There is file found inside the torrent?
    Result := FObjectListFileNameAndLength.Count > 0;

  except
    //Can not found items that should be present.
    Result := False;
  end;

end;

function TDecodeTorrent.GetFileList_V2: boolean;
var
  BEncodedFileTree: TBEncoded;

  procedure ProcessPathOrFile(const Node: TBEncoded; const path: string);
  var
    P_Item: Pointer;
    FileLength: int64;
    NodeData: TBEncodedData;
    ThisIsFileWithPadding: boolean;
    DecodeTorrentFileName: TDecodeTorrentFileNameAndLength;
  begin
    // Everyting in the tree is befDictionary
    ThisIsFileWithPadding := False;
    FileLength := -1; // -1 is no file length found yet.


    for P_Item in node.ListData do
    begin // read all the befDictionary list items one by one
      NodeData := TBEncodedData(P_Item);

      // Found a new path node
      if NodeData.Data.Format = befDictionary then
      begin
        ProcessPathOrFile(NodeData.Data, path + DirectorySeparator + NodeData.Header);
        Continue;
      end;

      // check if this present dictionary is a padding node
      if (NodeData.Data.Format = befString) and (NodeData.Header = 'attr') then
      begin
        ThisIsFileWithPadding := (UTF8Pos('p', NodeData.Data.StringData) > 0);
        if ThisIsFileWithPadding and not FPaddingPresent_V2 then
        begin
          FPaddingPresent_V2 := True;
        end;
        Continue;
      end;

      // Found a file node? This is at the end of the tree node.
      if (NodeData.Data.Format = befInteger) and (NodeData.Header = 'length') then
      begin
        FileLength := NodeData.Data.IntegerData;
      end;

    end; // read all the befDictionary list items one by one

    // One dictionary list have been process
    // Is this a dictionary list with a file inside?
    if FileLength >= 0 then
    begin
      DecodeTorrentFileName := TDecodeTorrentFileNameAndLength.Create;
      DecodeTorrentFileName.Filename := ExtractFileDir(path);
      DecodeTorrentFileName.FileLength := FileLength;
      FObjectListFileNameAndLength.Add(DecodeTorrentFileName);

      //add FileLength to the total sum of all files inside the torrent.
      if not ThisIsFileWithPadding then
      begin
        Inc(FTotalFileSize, DecodeTorrentFileName.FileLength);
      end;
    end;

  end;

begin
  Result := False;
  FInfoFilesVersion := 2;
  FObjectListFileNameAndLength.Clear;
  FTotalFileSize := 0;
  try
    {find 'info.file tree' }
    BEncodedFileTree := FBEncoded_Info.ListData.FindElement('file tree');

    if assigned(BEncodedFileTree) then
    begin //'info.file tree' found
      ProcessPathOrFile(BEncodedFileTree, '');
    end;

    //There is file found inside the torrent?
    Result := FObjectListFileNameAndLength.Count > 0;

  except
    //Can not found items that should be present.
    Result := False;
  end;

end;

function TDecodeTorrent.GetOneFileTorrent: boolean;
var
  Filename: utf8string;
  DecodeTorrentFileName: TDecodeTorrentFileNameAndLength;
  TempBEncoded: TBEncoded;
begin
  try
    // This is a torrent without file list/tree
    TempBEncoded := FBEncoded_Info.ListData.FindElement('length');
    Result := assigned(TempBEncoded);
    if Result then
    begin
      FInfoFilesVersion := 1;
      FObjectListFileNameAndLength.Clear;
      Filename := FBEncoded_Info.ListData.FindElement('name').StringData;
      DecodeTorrentFileName := TDecodeTorrentFileNameAndLength.Create;
      DecodeTorrentFileName.Filename := Filename;
      DecodeTorrentFileName.FileLength := TempBEncoded.IntegerData;
      FObjectListFileNameAndLength.Add(DecodeTorrentFileName);
      FTotalFileSize := TempBEncoded.IntegerData;
    end;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.DecodeTorrent: boolean;
begin
  Result := False;
  try
    //Free the old one before creating a new one
    if assigned(FBEncoded) then
    begin
      FreeAndNil(FBEncoded);
    end;

    // Clear List that will be filed later.
    TrackerList.Clear;
    FObjectListFileNameAndLength.Clear;

    // Reset to default value
    FTotalFileSize := 0;
    FTorrentVersion := tv_unknown;
    FPaddingPresent_V1 := False;
    FPaddingPresent_V2 := False;

    //the torrent file inside FMemoryStream -> BEnencode it
    FMemoryStream.Position := 0;
    FBEncoded := TBEncoded.Create(FMemoryStream);

    //torrent file MUST begin with befDictionary.
    if FBEncoded.Format <> befDictionary then
      exit; //error

    //torrent MUST have 'info'
    FBEncoded_Info := FBEncoded.ListData.FindElement('info');
    if not assigned(FBEncoded_Info) then
      exit; //error

    // Is this V1,V2 or hybrid torrent type?
    if isPresentInfoFiles_V1 then FTorrentVersion := tv_V1;
    if isPresentInfoFileTree_V2 then
    begin
      if FTorrentVersion = tv_V1 then
        FTorrentVersion := tv_Hybrid
      else
        FTorrentVersion := tv_V2;
    end;

    //Accept torrent only when there is no issue in reading AnnounceList and file list
    if GetAnnounceList then
    begin
      case FTorrentVersion of
        tv_V1: Result := GetFileList_V1;
        tv_V2: Result := GetFileList_V2;
        tv_Hybrid:
        begin // Only V2 is actualy used. V1 need to be read to look for padding.
          Result := GetFileList_V1;
          if Result then GetFileList_V2;
        end;
        else
          Assert(False, 'Missing torrent version');
      end;
    end;

    if not Result then
    begin // There is nothing found in tree list. Maybe this is a Torrent with one file?
      Result := GetOneFileTorrent;
      if Result then FTorrentVersion := tv_V1;
    end;

    //    FInfoHash_V1 := GetInfoHash;
    FCreatedBy := GetCreatedBy;
    FCreatedDate := GetCreatedDate;
    FComment := GetComment;
    FName := GetName;
    FPieceLenght := GetPieceLenght;
    FPrivateTorrent := GetPrivateTorrent;
    FInfoSource := GetInfoSource;
    FMetaVersion := GetMetaVersion;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.isPresentInfoFiles_V1: boolean;
var
  Info: TBEncoded;
  str: utf8string;
begin
  try
    {find 'info.files' }
    Info := FBEncoded_Info.ListData.FindElement('files');
    Result := assigned(info);
    if Result then
    begin
      str := '';
      TBEncoded.Encode(FBEncoded_Info, str);
      FInfoHash_V1 := UpperCase(SHA1Print(SHA1String(str)));
    end
    else
    begin
      FInfoHash_V1 := 'N/A';
    end;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.isPresentInfoFileTree_V2: boolean;
var
  Info: TBEncoded;
  str: utf8string;
begin
  try
    {find 'info.file tree' }
    Info := FBEncoded_Info.ListData.FindElement('file tree');
    Result := assigned(info);
    if Result then
    begin
      str := '';
      TBEncoded.Encode(FBEncoded_Info, str);
      FInfoHash_V2 := UpperCase(GetSha256(str));
    end
    else
    begin
      FInfoHash_V2 := 'N/A';
    end;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.GetSha256(const Source: utf8string): utf8string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
  i: integer;
begin
  Digest[0] := 0; // suppres compiler warning.
  Hash := TDCP_sha256.Create(nil);
  Hash.Init;
  Hash.UpdateStr(Source);
  Hash.Final(Digest);
  Result := '';
  for i := Low(Digest) to High(Digest) do
  begin
    Result := Result + IntToHex(Digest[i], 2);
  end;
  Hash.Free;
end;

function TDecodeTorrent.GetMetaVersion: int64;
var
  TempBEncoded: TBEncoded;
begin
  Result := 0;
  try
    {find 'meta version' }
    TempBEncoded := FBEncoded_Info.ListData.FindElement('meta version');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.IntegerData;
  except
    Result := 0;
  end;
end;

function TDecodeTorrent.InfoFilesCount: integer;
begin
  Result := FObjectListFileNameAndLength.Count;
end;

function TDecodeTorrent.InfoFilesNameIndex(index: integer): utf8string;
begin
  Result := TDecodeTorrentFileNameAndLength(
    FObjectListFileNameAndLength[index]).Filename;
end;

function TDecodeTorrent.InfoFilesLengthIndex(index: integer): int64;
begin
  Result := TDecodeTorrentFileNameAndLength(FObjectListFileNameAndLength[index]).FileLength;
end;

function TDecodeTorrent.GetPrivateTorrent: boolean;
var
  TempBEncoded: TBEncoded;
begin
  Result := False;
  try
    {find 'private' }
    TempBEncoded := FBEncoded_Info.ListData.FindElement('private');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.IntegerData = 1;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.GetInfoSource: utf8string;
var
  TempBEncoded: TBEncoded;
begin
  Result := '';
  try
    {find 'source' }
    TempBEncoded := FBEncoded_Info.ListData.FindElement('source');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.StringData;
  except
    Result := '';
  end;
end;

procedure TDecodeTorrent.SetComment(const AValue: utf8string);
var
  Data: TBEncodedData;
begin
  if FComment = AValue then
    Exit;
  FComment := AValue;
  try
    //if empty comment then remove the element.
    if FComment = '' then
    begin
      FBEncoded.ListData.RemoveElement('comment');
      exit;
    end;

    //if there is no comment element, then make new one
    if not assigned(FBEncoded_Comment) then
    begin
      FBEncoded_Comment := TBEncoded.Create;
      FBEncoded_Comment.Format := befString;
      FBEncoded_Comment.StringData := FComment;
      Data := TBEncodedData.Create(FBEncoded_Comment);
      Data.Header := 'comment';
      FBEncoded.ListData.Add(Data);
    end
    else
    begin
      FBEncoded_Comment.StringData := FComment;
    end;

  except
    FComment := AValue;
  end;

end;

{
try
    Encoded := TBEncoded.Create;
    Encoded.Format := befString;
    Encoded.StringData := TrackerURL;
    Data := TBEncodedData.Create(Encoded);
    Data.Header := 'announce';
    FBEncoded.ListData.Add(Data);
    FBEncoded.ListData.Sort(@sort_);//text must be in alfabetical order.
except
end;

}

function TDecodeTorrent.RemovePrivateTorrentFlag: boolean;
begin
  try
    FBEncoded_Info.ListData.RemoveElement('private');
    Result := True;
  except
    Result := False;
  end;
  //read databack again
  FPrivateTorrent := GetPrivateTorrent;
end;

function TDecodeTorrent.AddPrivateTorrentFlag: boolean;
var
  Encoded: TBEncoded;
  Data: TBEncodedData;
begin//remove the old one and create a new one
  RemovePrivateTorrentFlag;
  try
    Encoded := TBEncoded.Create;
    Encoded.Format := befInteger;
    Encoded.IntegerData := 1;
    Data := TBEncodedData.Create(Encoded);
    Data.Header := 'private';
    FBEncoded_Info.ListData.Add(Data);
    FBEncoded_Info.ListData.Sort(@sort_);//text must be in alfabetical order.
    Result := True;
  except
    Result := False;
  end;
  //read databack again
  FPrivateTorrent := GetPrivateTorrent;
end;

function TDecodeTorrent.InfoSourceRemove: boolean;
begin
  try
    FBEncoded_Info.ListData.RemoveElement('source');
    Result := True;
  except
    Result := False;
  end;
  //read databack again
  FInfoSource := GetInfoSource;
end;

function TDecodeTorrent.InfoSourceAdd(const Value: utf8string): boolean;
var
  Encoded: TBEncoded;
  Data: TBEncodedData;
begin//remove the old one and create a new one
  InfoSourceRemove;
  try
    Encoded := TBEncoded.Create;
    Encoded.Format := befString;
    Encoded.StringData := Value;
    Data := TBEncodedData.Create(Encoded);
    Data.Header := 'source';
    FBEncoded_Info.ListData.Add(Data);
    FBEncoded_Info.ListData.Sort(@sort_);//text must be in alfabetical order.
    Result := True;
  except
    Result := False;
  end;
  FInfoSource := GetInfoSource;
end;

function TDecodeTorrent.RemoveAnnounce: boolean;
begin
  try
    FBEncoded.ListData.RemoveElement('announce');
    Result := True;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.RemoveAnnounceList: boolean;
begin
  try
    FBEncoded.ListData.RemoveElement('announce-list');
    Result := True;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.SaveTorrent(const Filename: utf8string): boolean;
var
  str: utf8string;
  S: TFileStream;
begin
  try
    //Encode it to string format
    str := '';
    TBEncoded.Encode(FBEncoded, str);
    //Write string to file. Support filename with unicode.
    S := TFileStream.Create(FileName, fmCreate);
    try
      Result := s.Write(Str[1], length(Str)) = length(Str);
    finally
      S.Free;
    end;
  except
    Result := False;
  end;
end;



function TDecodeTorrent.ChangeAnnounce(const TrackerURL: utf8string): boolean;
var
  Encoded: TBEncoded;
  Data: TBEncodedData;
begin//remove the old one and create a new one
  RemoveAnnounce;
  try
    Encoded := TBEncoded.Create;
    Encoded.Format := befString;
    Encoded.StringData := TrackerURL;
    Data := TBEncodedData.Create(Encoded);
    Data.Header := 'announce';
    FBEncoded.ListData.Add(Data);
    FBEncoded.ListData.Sort(@sort_);//text must be in alfabetical order.
    Result := True;
  except
    Result := False;
  end;
end;

function TDecodeTorrent.ChangeAnnounceList(StringList: TStringList): boolean;
var
  EncodedListRoot, EncodedList, EncodedString: TBEncoded;
  DataRootBEncodedData: TBEncodedData;
  i: integer;
begin
  Result := True;
  //remove the present one.
  RemoveAnnounceList;
  //if there is nothing in the list then exit.
  if StringList.Count = 0 then
    Exit;

  //create a new anounce list
  try
    //Create the 'announce-list'
    EncodedListRoot := TBEncoded.Create;
    EncodedListRoot.Format := befList;
    DataRootBEncodedData := TBEncodedData.Create(EncodedListRoot);
    DataRootBEncodedData.Header := 'announce-list';
    FBEncoded.ListData.Add(DataRootBEncodedData); //root

    //Create list inside 'announce-list'
    //            Str := TempBEncoded.ListData.Items[i].Data.ListData.First.Data.StringData;

    for i := 0 to StringList.Count - 1 do
    begin
      //create a list with string element
      EncodedList := TBEncoded.Create;
      EncodedList.Format := befList;
      // add list to the list via TBEncodedData
      EncodedListRoot.ListData.Add(TBEncodedData.Create(EncodedList));

      //String ellement inside the list
      EncodedString := TBEncoded.Create;
      EncodedString.Format := befString;
      EncodedString.StringData := StringList[i];
      // add string to the list via TBEncodedData
      EncodedList.ListData.Add(TBEncodedData.Create(EncodedString));
    end;

    FBEncoded.ListData.Sort(@sort_);//text must be in alfabetical order.
  except
    Result := False;
  end;
end;

function TDecodeTorrent.TorrentVersionToString: utf8string;
begin
  case FTorrentVersion of
    tv_V1: Result := 'V1';
    tv_V2: Result := 'V2';
    tv_Hybrid: Result := 'Hybrid (V1&V2)';
    tv_unknown: Result := 'unknown';
    else
      Result := 'TorrentVersionToString: unkown value';
  end;
end;

function TDecodeTorrent.PaddingToString: utf8string;
begin
  case FTorrentVersion of
    tv_V1:
    begin
      Result := BoolToStr(FPaddingPresent_V1, 'Yes', 'No');
    end;
    tv_V2:
    begin
      Result := BoolToStr(FPaddingPresent_V2, 'Yes', 'No');
    end;
    tv_Hybrid:
    begin // Show only V2 hash. No space for both V1 and V2
      Result := 'V1:' + BoolToStr(FPaddingPresent_V1, 'Yes', 'No') +
        ' V2:' + BoolToStr(FPaddingPresent_V2, 'Yes', 'No');
    end;
    else
      Result := 'N/A'
  end;
end;

function TDecodeTorrent.GetCreatedBy: utf8string;
var
  TempBEncoded: TBEncoded;
begin
  Result := '';
  try
    TempBEncoded := FBEncoded.ListData.FindElement('created by');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.StringData;
  except
    Result := '';
  end;
end;

function TDecodeTorrent.GetCreatedDate: TDateTime;
var
  TempBEncoded: TBEncoded;
begin
  Result := 0; //Some torrent have no creation date
  try
    TempBEncoded := FBEncoded.ListData.FindElement('creation date');
    if assigned(TempBEncoded) then
      Result := UnixToDateTime(TempBEncoded.IntegerData);
  except
    Result := 0;
  end;
end;

function TDecodeTorrent.GetComment: utf8string;
begin
  Result := '';
  try
    FBEncoded_Comment := FBEncoded.ListData.FindElement('comment');
    if assigned(FBEncoded_Comment) then
      Result := UTF8Trim(FBEncoded_Comment.StringData);
  except
    Result := '';
  end;
end;

function TDecodeTorrent.GetName: utf8string;
var
  TempBEncoded: TBEncoded;
begin
  Result := '';
  try
    {find 'name' }
    TempBEncoded := FBEncoded_Info.ListData.FindElement('name');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.StringData;
  except
    Result := '';
  end;
end;

function TDecodeTorrent.GetPieceLenght: int64;
var
  TempBEncoded: TBEncoded;
begin
  Result := 0;
  try
    {find 'piece length' }
    TempBEncoded := FBEncoded_Info.ListData.FindElement('piece length');
    if assigned(TempBEncoded) then
      Result := TempBEncoded.IntegerData;
  except
    Result := 0;
  end;
end;

end.
