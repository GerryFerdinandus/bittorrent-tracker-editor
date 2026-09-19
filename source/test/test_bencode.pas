// SPDX-License-Identifier: MIT
unit test_bencode;

{
  Decode and encode bencoded values directly, without going through a torrent file.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, BEncode;

type

  { TTestBEncode }

  TTestBEncode = class(TTestCase)
  private
    FEncoded: TBEncoded;

    //Decode a bencoded value that is present as a string, result is owned by FEncoded
    function Decode(const Str: UTF8String): TBEncoded;

    //Build a standalone bencoded string value, to be added as a child of FEncoded
    function MakeString(const Str: UTF8String): TBEncoded;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Decode_String;
    procedure Test_Decode_Empty_String;
    procedure Test_Decode_Integer;
    procedure Test_Decode_Negative_Integer;
    procedure Test_Decode_List;
    procedure Test_Decode_Dictionary;
    procedure Test_Decode_Nested_List_In_Dictionary;
    procedure Test_Decode_Round_Trip_Matches_Original;
    procedure Test_Decode_Round_Trip_Preserves_Binary_Bytes;
    procedure Test_FindElement_Is_Case_Insensitive;
    procedure Test_FindElement_Returns_Nil_When_Missing;
    procedure Test_RemoveElement_Removes_Matching_Item;
    procedure Test_Encode_String;
    procedure Test_Encode_Integer;
    procedure Test_Encode_Negative_Integer;
    procedure Test_Encode_List;
    procedure Test_Encode_Dictionary;
    procedure Test_Decode_Invalid_Prefix_Raises_Exception;
    procedure Test_Decode_Unterminated_Integer_Raises_Exception;
    procedure Test_Decode_Truncated_String_Raises_Exception;
  end;

implementation

{ TTestBEncode }

procedure TTestBEncode.SetUp;
begin
  FEncoded := nil;
end;

procedure TTestBEncode.TearDown;
begin
  FEncoded.Free;
end;

function TTestBEncode.Decode(const Str: UTF8String): TBEncoded;
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

function TTestBEncode.MakeString(const Str: UTF8String): TBEncoded;
begin
  Result := TBEncoded.Create;
  Result.Format := befString;
  Result.StringData := Str;
end;

procedure TTestBEncode.Test_Decode_String;
begin
  FEncoded := Decode('4:spam');

  CheckEquals(Ord(befString), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals('spam', FEncoded.StringData, 'Wrong string value');
end;

procedure TTestBEncode.Test_Decode_Empty_String;
begin
  FEncoded := Decode('0:');

  CheckEquals(Ord(befString), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals('', FEncoded.StringData, 'An empty string must decode to an empty value');
end;

procedure TTestBEncode.Test_Decode_Integer;
begin
  FEncoded := Decode('i42e');

  CheckEquals(Ord(befInteger), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(42, FEncoded.IntegerData, 'Wrong integer value');
end;

procedure TTestBEncode.Test_Decode_Negative_Integer;
begin
  FEncoded := Decode('i-42e');

  CheckEquals(Ord(befInteger), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(-42, FEncoded.IntegerData, 'Negative integers must be supported');
end;

procedure TTestBEncode.Test_Decode_List;
begin
  FEncoded := Decode('l4:spam4:eggse');

  CheckEquals(Ord(befList), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(2, FEncoded.ListData.Count, 'Wrong element count');
  CheckEquals('spam', FEncoded.ListData[0].Data.StringData, 'Wrong first element');
  CheckEquals('eggs', FEncoded.ListData[1].Data.StringData, 'Wrong second element');
end;

procedure TTestBEncode.Test_Decode_Dictionary;
begin
  FEncoded := Decode('d3:cow3:moo4:spam4:eggse');

  CheckEquals(Ord(befDictionary), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(2, FEncoded.ListData.Count, 'Wrong element count');
  CheckEquals('moo', FEncoded.ListData.FindElement('cow').StringData, 'Wrong cow value');
  CheckEquals('eggs', FEncoded.ListData.FindElement('spam').StringData, 'Wrong spam value');
end;

procedure TTestBEncode.Test_Decode_Nested_List_In_Dictionary;
begin
  FEncoded := Decode('d4:listl1:a1:bee');

  CheckEquals(Ord(befDictionary), Ord(FEncoded.Format), 'Wrong format');

  with FEncoded.ListData.FindElement('list') do
  begin
    CheckEquals(Ord(befList), Ord(Format), 'Nested value must be a list');
    CheckEquals(2, ListData.Count, 'Wrong nested element count');
    CheckEquals('a', ListData[0].Data.StringData, 'Wrong first nested element');
    CheckEquals('b', ListData[1].Data.StringData, 'Wrong second nested element');
  end;
end;

procedure TTestBEncode.Test_Decode_Round_Trip_Matches_Original;
const
  ORIGINAL = 'd4:listl1:a1:be4:spam4:eggse';
var
  Output: UTF8String;
begin
  FEncoded := Decode(ORIGINAL);

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals(ORIGINAL, Output, 'Encoding a decoded value must reproduce the original');
end;

procedure TTestBEncode.Test_Decode_Round_Trip_Preserves_Binary_Bytes;
var
  BinaryData, Original, Output: UTF8String;
  i: integer;
begin
  //fields like 'pieces' hold arbitrary bytes (e.g. SHA1 hashes), not text
  SetLength(BinaryData, 256);
  for i := 0 to 255 do
    BinaryData[i + 1] := Chr(i);

  Original := IntToStr(Length(BinaryData)) + ':' + BinaryData;
  FEncoded := Decode(Original);

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals(Length(Original), Length(Output), 'Encoded byte length must match the original');
  CheckEquals(Original, Output, 'Encoding a decoded binary string must reproduce every byte exactly');
end;

procedure TTestBEncode.Test_FindElement_Is_Case_Insensitive;
begin
  FEncoded := Decode('d3:cow3:mooe');

  CheckEquals('moo', FEncoded.ListData.FindElement('COW').StringData,
    'FindElement must ignore case');
end;

procedure TTestBEncode.Test_FindElement_Returns_Nil_When_Missing;
begin
  FEncoded := Decode('d3:cow3:mooe');

  CheckNull(FEncoded.ListData.FindElement('missing'), 'A missing key must return nil');
end;

procedure TTestBEncode.Test_RemoveElement_Removes_Matching_Item;
begin
  FEncoded := Decode('d3:cow3:moo4:spam4:eggse');

  CheckEquals(0, FEncoded.ListData.RemoveElement('cow'), 'Wrong removed index');
  CheckEquals(1, FEncoded.ListData.Count, 'Element must be removed from the list');
  CheckNull(FEncoded.ListData.FindElement('cow'), 'Removed key must no longer be found');
  CheckEquals('eggs', FEncoded.ListData.FindElement('spam').StringData,
    'Remaining element must be unaffected');
end;

procedure TTestBEncode.Test_Encode_String;
var
  Output: UTF8String;
begin
  FEncoded := MakeString('spam');

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals('4:spam', Output, 'Wrong encoded string');
end;

procedure TTestBEncode.Test_Encode_Integer;
var
  Output: UTF8String;
begin
  FEncoded := TBEncoded.Create;
  FEncoded.Format := befInteger;
  FEncoded.IntegerData := 42;

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals('i42e', Output, 'Wrong encoded integer');
end;

procedure TTestBEncode.Test_Encode_Negative_Integer;
var
  Output: UTF8String;
begin
  FEncoded := TBEncoded.Create;
  FEncoded.Format := befInteger;
  FEncoded.IntegerData := -42;

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals('i-42e', Output, 'Wrong encoded negative integer');
end;

procedure TTestBEncode.Test_Encode_List;
var
  Output: UTF8String;
begin
  FEncoded := TBEncoded.Create;
  FEncoded.Format := befList;
  FEncoded.ListData.Add(TBEncodedData.Create(MakeString('spam')));
  FEncoded.ListData.Add(TBEncodedData.Create(MakeString('eggs')));

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals('l4:spam4:eggse', Output, 'Wrong encoded list');
end;

procedure TTestBEncode.Test_Encode_Dictionary;
var
  Output: UTF8String;
  Item: TBEncodedData;
begin
  FEncoded := TBEncoded.Create;
  FEncoded.Format := befDictionary;

  Item := TBEncodedData.Create(MakeString('moo'));
  Item.Header := 'cow';
  FEncoded.ListData.Add(Item);

  Output := '';
  TBEncoded.Encode(FEncoded, Output);

  CheckEquals('d3:cow3:mooe', Output, 'Wrong encoded dictionary');
end;

procedure TTestBEncode.Test_Decode_Invalid_Prefix_Raises_Exception;
begin
  try
    Decode('x');
    Fail('An unknown bencode prefix must raise an exception');
  except
    on E: Exception do; //expected
  end;
end;

procedure TTestBEncode.Test_Decode_Unterminated_Integer_Raises_Exception;
begin
  try
    Decode('i42');
    Fail('An integer without a terminating ''e'' must raise an exception');
  except
    on E: Exception do; //expected
  end;
end;

procedure TTestBEncode.Test_Decode_Truncated_String_Raises_Exception;
begin
  try
    //string length says 4 bytes, but only 2 are present
    Decode('4:sp');
    Fail('A truncated string must raise an exception');
  except
    on E: Exception do; //expected
  end;
end;

initialization
  RegisterTest(TTestBEncode);
end.
