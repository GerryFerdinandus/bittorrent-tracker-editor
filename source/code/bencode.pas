// SPDX-License-Identifier: CC0-1.0
{
public domain code?
Source code from:
http://www.torry.net/quicksearchd.php?String=bencode&Title=Yes

With some minor modification made by Gerry Ferdinandus
}

{
Decode and encode the Bencode format used in torrent files
}

{
  Bug fix 2014-10-09
  'i' integer now accept negative number
      if not ((X in ['0'..'9' , '-']) or (X = 'e')) then
        raise Exception.Create('');

}

unit BEncode;
{$mode objfpc}{$H+}
interface


uses
  Classes, Contnrs, SysUtils;

type
  TBEncodedFormat = (befEmpty, befString, befInteger, befList, befDictionary);

  TBEncoded = class;

  TBEncodedData = class
  public
    Header: UTF8String;
    Data: TBEncoded;
    destructor Destroy; override;
  public
    constructor Create(Data_: TBEncoded);
  end;

  { TBEncodedDataList }

  TBEncodedDataList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TBEncodedData;
    procedure SetItems(Index: Integer; AClass: TBEncodedData);
  public
    function FindElement(Header: UTF8String): TBEncoded;
    function Add(AClass: TBEncodedData): Integer;
    function Extract(Item: TBEncodedData): TBEncodedData;
    function Remove(AClass: TBEncodedData): Integer;
    function IndexOf(AClass: TBEncodedData): Integer;

    function RemoveElement(Header: UTF8String): integer;//2011-1030

    function First: TBEncodedData;
    function Last: TBEncodedData;
    procedure Insert(Index: Integer; AClass: TBEncodedData);
    property Items[Index: Integer]: TBEncodedData read GetItems write SetItems;
      default;
  end;

  TBEncoded = class(TObject)
  private
    FFormat: TBEncodedFormat;
    procedure SetFormat(Format: TBEncodedFormat);
  public
    StringData: UTF8String;
    IntegerData: int64;
    ListData: TBEncodedDataList;
    property Format: TBEncodedFormat read FFormat write SetFormat;
    class procedure Encode(Encoded: TBEncoded; var Output: UTF8String);
    destructor Destroy; override;
    constructor Create(Stream: TStream);
    constructor Create;
  end;

implementation

destructor TBEncodedData.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

constructor TBEncodedData.Create(Data_: TBEncoded);
begin
  inherited Create;
  Self.Data := Data_;
end;

destructor TBEncoded.Destroy;
begin
  if ListData <> nil then
    ListData.Free;

  inherited Destroy;
end;

constructor TBEncoded.Create(Stream: TStream);

  function GetString(Buffer: string): string;
  var
    X: char;
  begin
    Result := '';
    // loop until we come across it
    X := ' ';
    repeat
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');
      if not ((X in ['0'..'9']) or (x = ':')) then
        raise Exception.Create('');
      if X = ':' then
      begin
        if Buffer = '' then
          raise Exception.Create('');
        if Length(Buffer) > 7 then
          raise Exception.Create('');
        SetLength(Result, StrToInt(Buffer));
        if Stream.Read(Result[1], Length(Result)) <> Length(Result) then
          raise Exception.Create('');
        Break;
      end
      else
        Buffer := Buffer + X;
    until False;
  end;

var
  X: char;
  Buffer: string;
  Data: TBEncodedData;
  Encoded: TBEncoded;
begin
  inherited Create;

  X := ' ';

  // get first character to determine the format of the proceeding data
  if Stream.Read(X, 1) <> 1 then
    raise Exception.Create('');

  // is it an integer?
  if X = 'i' then
  begin
    // yes it is, let's read until we come across e
    Buffer := '';
    repeat
      //must be able to read the stream
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');

      //Must be a integer value or 'e'
      if not ((X in ['0'..'9' , '-']) or (X = 'e')) then
        raise Exception.Create('');

      //if found the 'end' then decode it.
      if X = 'e' then
      begin
        if Buffer = '' then
          raise Exception.Create('')
        else
        begin
          Format := befInteger;
          IntegerData := StrToInt64(Buffer);
          Break;
        end;
      end
      else
        Buffer := Buffer + X;
    until False;
  end
  // is it a list?
  else if X = 'l' then
  begin
    // its a list
    Format := befList;

    // loop until we come across e
    repeat
      // have a peek around and see if theres an e
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');
      // is it an e?
      if X = 'e' then
        Break;
      // otherwise move the cursor back
      Stream.Seek(-1, soFromCurrent);
      // create the element
      Encoded := TBEncoded.Create(Stream);
      // add it to the list
      ListData.Add(TBEncodedData.Create(Encoded));
    until False;
  end
  // is it a dictionary?
  else if X = 'd' then
  begin
    // its a dictionary :>
    Format := befDictionary;

    // loop until we come across e
    repeat
      // have a peek around and see if theres an e
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');
      // is it an e?
      if X = 'e' then
        Break;
      // if it isnt an e it has to be numerical!
      if not (X in ['0'..'9']) then
        raise Exception.Create('');
      // now read the string data
      Buffer := GetString(string(X));
      // create the element
      Encoded := TBEncoded.Create(Stream);
      // create the data element
      Data := TBEncodedData.Create(Encoded);
      Data.Header := Buffer;
      // add it to the list
      ListData.Add(Data);
    until False;
  end
  // is it a string?
  else if X in ['0'..'9'] then
  begin
    StringData := GetString(string(X));
    Format := befString;
  end
  else
    raise Exception.Create('');
end;

constructor TBEncoded.Create;
begin
  inherited Create;
end;

class procedure TBEncoded.Encode(Encoded: TBEncoded; var Output: UTF8String);
var
  i: integer;
begin
  with Encoded do
  begin
    // what type of member is it?
    case Format of
      befString: Output := Output + IntToStr(Length(StringData)) + ':' +
        StringData;
      befInteger: Output := Output + 'i' + IntToStr(IntegerData) + 'e';
      befList:
      begin
        Output := Output + 'l';
        for i := 0 to ListData.Count - 1 do
          Encode(TBEncoded(ListData[i].Data), Output);
        Output := Output + 'e';
      end;
      befDictionary:
      begin
        Output := Output + 'd';
        for i := 0 to ListData.Count - 1 do
        begin
          Output := Output + IntToStr(Length(ListData[i].Header)) + ':' +
            ListData[i].Header;
          Encode(TBEncoded(ListData[i].Data), Output);
        end;
        Output := Output + 'e';
      end;
    end;
  end;
end;

procedure TBEncoded.SetFormat(Format: TBEncodedFormat);
begin
  if Format in [befList, befDictionary] then
    ListData := TBEncodedDataList.Create;
  FFormat := Format;
end;

function TBEncodedDataList.FindElement(Header: UTF8String): TBEncoded;
var
  i: integer;
begin
  Header := LowerCase(Header);
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].Header) = Header then
    begin
      Result := Items[i].Data;
      Exit;
    end;

  Result := nil;
end;

function TBEncodedDataList.Add(AClass: TBEncodedData): Integer;
begin
  Result := inherited Add(AClass);
end;

function TBEncodedDataList.Extract(Item: TBEncodedData): TBEncodedData;
begin
  Result := TBEncodedData(inherited Extract(Item));
end;

function TBEncodedDataList.First: TBEncodedData;
begin
  Result := TBEncodedData(inherited First);
end;

function TBEncodedDataList.GetItems(Index: Integer): TBEncodedData;
begin
  Result := TBEncodedData(inherited Items[Index]);
end;

function TBEncodedDataList.IndexOf(AClass: TBEncodedData): Integer;
begin
  Result := inherited IndexOf(AClass);
end;

function TBEncodedDataList.RemoveElement(Header: UTF8String): Integer;
var
  i: integer;
begin
  Header := LowerCase(Header);
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].Header) = Header then
    begin
      Result := i;
      Remove(Items[i]);//memory will be released.
      Exit;
    end;
  Result := -1;
end;

procedure TBEncodedDataList.Insert(Index: Integer; AClass: TBEncodedData);
begin
  inherited Insert(Index, AClass);
end;

function TBEncodedDataList.Last: TBEncodedData;
begin
  Result := TBEncodedData(inherited Last);
end;

function TBEncodedDataList.Remove(AClass: TBEncodedData): Integer;
begin
  Result := inherited Remove(AClass);
end;

procedure TBEncodedDataList.SetItems(Index: Integer; AClass: TBEncodedData);
begin
  inherited Items[Index] := AClass;
end;

end.
