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
  Classes, SysUtils, contnrs, LazUTF8Classes, BEncode;

type

  //Every torrent file have one or more files.
  TDecodeTorrentFile = record
    Filename: utf8string;
    FileLength: int64;
  end;

  TDecodeTorrentFileNameAndLength = class
    Filename: utf8string;
    FileLength: int64;
  end;


  { TDecodeTorrent }
  TDecodeTorrent = class
  private
    FFilenameTorrent: UTF8String;
    FMemoryStream: TMemoryStream;
    FBEncoded: TBEncoded;
    FObjectListFileNameAndLength: TObjectList;
    FTotalFileSize: int64;

    //Torrent file must have 'info' item.
    FBEncoded_Info: TBEncoded;
    FBEncoded_Comment: TBEncoded;

    FInfoHash: utf8string;
    FCreatedBy: utf8string;
    FCreatedDate: TDateTime;
    FComment: utf8string;
    FInfoSource: utf8string;
    FName: utf8string;
    FPieceLenght: int64;
    FPrivateTorrent: boolean;

    function DecodeTorrent: boolean; overload;

    function GetAnnounceList: boolean;
    function GetFileList: boolean;
    function GetInfoHash: utf8string;
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

    property FilenameTorrent: UTF8String read FFilenameTorrent;

    //Every torrent file have one or more files.
    //   TorrentFilesArray: Array of TDecodeTorrentFile;
    property TotalFileSize: int64 read FTotalFileSize;

    //Info hash
    property InfoHash: utf8string read FInfoHash;

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
    procedure RemovePrivateTorrentFlag;
    procedure AddPrivateTorrentFlag;

    //info.source
    property InfoSource: utf8string read FInfoSource;
    procedure InfoSourceRemove;
    procedure InfoSourceAdd(const Value: utf8string);

    //info.files
    function InfoFilesCount: integer;
    function InfoFilesNameIndex(index: integer): utf8string;
    function InfoFilesLengthIndex(index: integer): int64;

    //Announce list
    procedure RemoveAnnounce;
    procedure RemoveAnnounceList;
    procedure ChangeAnnounce(const TrackerURL: utf8string);
    procedure ChangeAnnounceList(StringList: TStringList);

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

uses dateutils, SHA1, FileUtil, LazUTF8;

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
  //Every torrent have
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
  S: TFileStreamUtf8;
begin
  FFilenameTorrent := Filename;
  //Load torrent file in FMemoryStream. This will be process by DecodeTorrent();
  try
    //Support FilenameTorrent with unicode.
    S := TFileStreamUtf8.Create(FilenameTorrent, fmOpenRead or fmShareDenyWrite);
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
  TrackerStr: UTF8String;
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

function TDecodeTorrent.GetFileList: boolean;
var
  //TempBEncodedInfo,
  TempBEncodedInfoFiles, TempBEncodedInfoFilesPath: TBEncoded;
  TempBEncodedInfoFilesData: TBEncodedData;

  i, x, countFiles, countPath: integer;
  FilenameWithPathStr, Filename: utf8string;

  DecodeTorrentFileName: TDecodeTorrentFileNameAndLength;
  FileLength: int64;
begin
{ info/files/path  -> all the files names
  or
  info/name -> one file name only
}
  //return false if there is no file at all. This must not be posible.

  FObjectListFileNameAndLength.Clear;
  FTotalFileSize := 0;
  try

    {find 'info.files' }
    TempBEncodedInfoFiles := FBEncoded_Info.ListData.FindElement('files');

    if assigned(TempBEncodedInfoFiles) then
    begin //'info.files' found
      countFiles := TempBEncodedInfoFiles.ListData.Count;
      if countFiles > 0 then
      begin
        for i := 0 to countFiles - 1 do
        begin
          //Get the info.files node.
          TempBEncodedInfoFilesData := TempBEncodedInfoFiles.ListData.Items[i];

          //Get the file name with path
          FilenameWithPathStr := '';
          TempBEncodedInfoFilesPath :=
            TempBEncodedInfoFilesData.Data.ListData.FindElement('path');
          countPath := TempBEncodedInfoFilesPath.ListData.Count;
          for x := 0 to countPath - 1 do
          begin
            FilenameWithPathStr :=
              FilenameWithPathStr + DirectorySeparator +
              TempBEncodedInfoFilesPath.ListData.Items[x].Data.StringData;
          end;


          //Get the file length
          FileLength := TempBEncodedInfoFilesData.Data.ListData.FindElement(
            'length').IntegerData;

          DecodeTorrentFileName := TDecodeTorrentFileNameAndLength.Create;
          DecodeTorrentFileName.Filename := FilenameWithPathStr;
          DecodeTorrentFileName.FileLength := FileLength;
          FObjectListFileNameAndLength.Add(DecodeTorrentFileName);
          //add it to the total sum of all files inside the torrent.
          Inc(FTotalFileSize, FileLength);
        end;

      end;
    end
    else //there is no 'info.files' found. This is an 'one file' torrent.
    begin//  Look for'info.name' and 'info.length'
      //Get the file name
      Filename := FBEncoded_Info.ListData.FindElement('name').StringData;

      FileLength := FBEncoded_Info.ListData.FindElement('length').IntegerData;

      DecodeTorrentFileName := TDecodeTorrentFileNameAndLength.Create;
      DecodeTorrentFileName.Filename := Filename;
      DecodeTorrentFileName.FileLength := FileLength;
      FObjectListFileNameAndLength.Add(DecodeTorrentFileName);


      Inc(FTotalFileSize, FileLength);
    end;



    //There is file found inside the torrent.
    Result := FObjectListFileNameAndLength.Count > 0;
    //We prefer that the file name are in sorted order.
    FObjectListFileNameAndLength.Sort(@SortFileName);

  except
    //Can not found items that should be present.
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

    //the torrent file inside FMemoryStream -> BEnencode it
    FMemoryStream.Position := 0;
    FBEncoded := TBEncoded.Create(FMemoryStream);




    //Read the tracker list and file list inside the torrent file.
    FTotalFileSize := 0;
    TrackerList.Clear;

    FObjectListFileNameAndLength.Clear;
    FTotalFileSize := 0;


    //torrent file MUST begin with befDictionary.
    if FBEncoded.Format <> befDictionary then
      exit; //error

    //torrent MUST have 'info'
    FBEncoded_Info := FBEncoded.ListData.FindElement('info');
    if not assigned(FBEncoded_Info) then
      exit; //error


    //Accept torrent only when there is no issue in reading AnnounceList and file list
    if GetAnnounceList and GetFileList then
    begin
      Result := True;
    end;

    FInfoHash := GetInfoHash;
    FCreatedBy := GetCreatedBy;
    FCreatedDate := GetCreatedDate;
    FComment := GetComment;
    FName := GetName;
    FPieceLenght := GetPieceLenght;
    FPrivateTorrent := GetPrivateTorrent;
    FInfoSource := GetInfoSource;
  except
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

procedure TDecodeTorrent.RemovePrivateTorrentFlag;
begin
  try
    FBEncoded_Info.ListData.RemoveElement('private');
  except
  end;
  //read databack again
  FPrivateTorrent := GetPrivateTorrent;
end;

procedure TDecodeTorrent.AddPrivateTorrentFlag;
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
  except
  end;
  //read databack again
  FPrivateTorrent := GetPrivateTorrent;
end;

procedure TDecodeTorrent.InfoSourceRemove;
begin
  try
    FBEncoded_Info.ListData.RemoveElement('source');
  except
  end;
  //read databack again
  FInfoSource := GetInfoSource;
end;

procedure TDecodeTorrent.InfoSourceAdd(const Value: utf8string);
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
  except
  end;
  FInfoSource := GetInfoSource;
end;

procedure TDecodeTorrent.RemoveAnnounce;
begin
  try
    FBEncoded.ListData.RemoveElement('announce');
  except
  end;
end;

procedure TDecodeTorrent.RemoveAnnounceList;
begin
  try
    FBEncoded.ListData.RemoveElement('announce-list');
  except
  end;
end;

function TDecodeTorrent.SaveTorrent(const Filename: utf8string): boolean;
var
  str: utf8string;
  S: TFileStreamUTF8;
begin
  try
    //Encode it to string format
    str := '';
    TBEncoded.Encode(FBEncoded, str);
    //Write string to file. Support filename with unicode.
    S := TFileStreamUTF8.Create(FileName, fmCreate);
    try
      Result := s.Write(Str[1], length(Str)) = length(Str);
    finally
      S.Free;
    end;
  except
    Result := False;
  end;
end;



procedure TDecodeTorrent.ChangeAnnounce(const TrackerURL: utf8string);
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
  except
  end;
end;

procedure TDecodeTorrent.ChangeAnnounceList(StringList: TStringList);
var
  EncodedListRoot, EncodedList, EncodedString: TBEncoded;
  DataRootBEncodedData: TBEncodedData;
  i: integer;
begin
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
  end;
end;

function TDecodeTorrent.GetInfoHash: utf8string;
begin
  Result := '';
  try
    //The info.value will be hash with SHA1
    TBEncoded.Encode(FBEncoded_Info, Result);
    Result := UpperCase(SHA1Print(SHA1String(Result)));
  except
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
  end;
end;

end.
