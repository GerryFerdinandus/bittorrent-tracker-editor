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

    //Check that decoding Str raises an exception, with MessagePart in its message when given
    procedure CheckDecodeRaises(const Str: UTF8String; const Msg: string;
      const MessagePart: string = '');
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Decode_String;
    procedure Test_Decode_Empty_String;
    procedure Test_Decode_Integer;
    procedure Test_Decode_Negative_Integer;
    procedure Test_Decode_Zero_Integer;
    procedure Test_Decode_Integer_With_Misplaced_Minus_Raises_Exception;
    procedure Test_Decode_Integer_With_Leading_Zero_Raises_Exception;
    procedure Test_Decode_Negative_Zero_Raises_Exception;
    procedure Test_Decode_Integer_Without_Digits_Raises_Exception;
    procedure Test_Decode_List;
    procedure Test_Decode_Dictionary;
    procedure Test_Decode_Nested_List_In_Dictionary;
    procedure Test_Decode_Round_Trip_Matches_Original;
    procedure Test_Decode_Round_Trip_Preserves_Binary_Bytes;
    procedure Test_FindElement_Is_Case_Insensitive;
    procedure Test_FindElement_Returns_Nil_When_Missing;
    procedure Test_RemoveElement_Removes_Matching_Item;
    procedure Test_RemoveElement_Returns_Index_Of_Later_Item;
    procedure Test_RemoveElement_Returns_Minus_One_When_Missing;
    procedure Test_Encode_String;
    procedure Test_Encode_Integer;
    procedure Test_Encode_Negative_Integer;
    procedure Test_Encode_List;
    procedure Test_Encode_Dictionary;
    procedure Test_Decode_Invalid_Prefix_Raises_Exception;
    procedure Test_Decode_Unterminated_Integer_Raises_Exception;
    procedure Test_Decode_Truncated_String_Raises_Exception;
    procedure Test_Decode_String_With_Eight_Digit_Length;
    procedure Test_Decode_String_Length_Larger_Than_Stream_Raises_Exception;
    procedure Test_Decode_String_Length_With_Eleven_Digits_Raises_Exception;
    procedure Test_Decode_String_Length_Limit_Is_High_Longint;
    procedure Test_Decode_String_Length_Without_Digits_Raises_Exception;
    procedure Test_Decode_Integer_Beyond_Int64_Raises_Exception;
    procedure Test_Decode_Empty_Input_Raises_Exception;
    procedure Test_Decode_Unterminated_List_Raises_Exception;
    procedure Test_Decode_Unterminated_Dictionary_Raises_Exception;
    procedure Test_Decode_Dictionary_Key_That_Is_Not_A_String_Raises_Exception;
    procedure Test_Decode_Nested_Lists_Within_The_Limit;
    procedure Test_Decode_Nested_Lists_Beyond_The_Limit_Raises_Exception;
    procedure Test_Decode_Nested_Dictionaries_Beyond_The_Limit_Raises_Exception;
    procedure Test_Decode_Extremely_Deep_Nesting_Raises_Exception;
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
    //An empty string has no first byte to write
    if Str <> '' then
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

procedure TTestBEncode.CheckDecodeRaises(const Str: UTF8String; const Msg: string;
  const MessagePart: string);
var
  Raised: boolean;
begin
  Raised := False;
  try
    FEncoded := Decode(Str);
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do
    begin
      Raised := True;
      if MessagePart <> '' then
        Check(Pos(MessagePart, E.Message) > 0,
          Msg + ': the message ''' + E.Message + ''' must contain ''' + MessagePart + '''');
    end;
  end;
  Check(Raised, Msg);
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

procedure TTestBEncode.Test_Decode_Zero_Integer;
begin
  FEncoded := Decode('i0e');

  CheckEquals(Ord(befInteger), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(0, FEncoded.IntegerData, 'Zero must be supported');
end;

procedure TTestBEncode.Test_Decode_Integer_With_Misplaced_Minus_Raises_Exception;
begin
  CheckDecodeRaises('i1-2e', 'A minus sign inside the digits must raise an exception');
  CheckDecodeRaises('i--2e', 'A double minus sign must raise an exception');
  CheckDecodeRaises('i2-e', 'A trailing minus sign must raise an exception');
end;

procedure TTestBEncode.Test_Decode_Integer_With_Leading_Zero_Raises_Exception;
begin
  CheckDecodeRaises('i03e', 'A leading zero must raise an exception');
  CheckDecodeRaises('i-03e', 'A leading zero after the minus sign must raise an exception');
  CheckDecodeRaises('i00e', 'Double zero must raise an exception');
end;

procedure TTestBEncode.Test_Decode_Negative_Zero_Raises_Exception;
begin
  CheckDecodeRaises('i-0e', 'Negative zero must raise an exception');
end;

procedure TTestBEncode.Test_Decode_Integer_Without_Digits_Raises_Exception;
begin
  CheckDecodeRaises('ie', 'An empty integer must raise an exception');
  CheckDecodeRaises('i-e', 'A minus sign without digits must raise an exception');
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

procedure TTestBEncode.Test_RemoveElement_Returns_Index_Of_Later_Item;
begin
  FEncoded := Decode('d3:cow3:moo4:spam4:eggse');

  CheckEquals(1, FEncoded.ListData.RemoveElement('SPAM'), 'Wrong removed index');
  CheckEquals(1, FEncoded.ListData.Count, 'Element must be removed from the list');
  CheckEquals('moo', FEncoded.ListData.FindElement('cow').StringData,
    'Remaining element must be unaffected');
end;

procedure TTestBEncode.Test_RemoveElement_Returns_Minus_One_When_Missing;
begin
  FEncoded := Decode('d3:cow3:mooe');
  CheckEquals(-1, FEncoded.ListData.RemoveElement('missing'), 'Missing key');
  CheckEquals(1, FEncoded.ListData.Count, 'Nothing may be removed');

  //The second remove of the same key is a missing key too
  CheckEquals(0, FEncoded.ListData.RemoveElement('cow'), 'First remove');
  CheckEquals(-1, FEncoded.ListData.RemoveElement('cow'), 'Second remove');

  FEncoded.Free;
  FEncoded := Decode('de');
  CheckEquals(-1, FEncoded.ListData.RemoveElement('cow'), 'Empty dictionary');
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
  CheckDecodeRaises('x', 'An unknown bencode prefix must raise an exception',
    'Unknown bencode value prefix');
  CheckDecodeRaises(':abc', 'A string without length must raise an exception',
    'Unknown bencode value prefix');
end;

procedure TTestBEncode.Test_Decode_Unterminated_Integer_Raises_Exception;
begin
  CheckDecodeRaises('i42', 'An integer without a terminating ''e'' must raise an exception',
    'end of stream');
end;

procedure TTestBEncode.Test_Decode_Truncated_String_Raises_Exception;
begin
  //string length says 4 bytes, but only 2 are present
  CheckDecodeRaises('4:sp', 'A truncated string must raise an exception', 'end of stream');
  //the length itself is cut
  CheckDecodeRaises('4', 'A string without ''e'' must raise an exception', 'end of stream');
end;

procedure TTestBEncode.Test_Decode_String_With_Eight_Digit_Length;
var
  Data: UTF8String;
begin
  //Fields like 'pieces' or 'piece layers' can be bigger than 10 MB
  Data := StringOfChar('x', 12345678);
  Data[1] := 'a';
  Data[Length(Data)] := 'z';
  FEncoded := Decode(IntToStr(Length(Data)) + ':' + Data);

  CheckEquals(Ord(befString), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(Length(Data), Length(FEncoded.StringData), 'Wrong string length');
  CheckEquals('a', FEncoded.StringData[1], 'Wrong first byte');
  CheckEquals('z', FEncoded.StringData[Length(Data)], 'Wrong last byte');
end;

procedure TTestBEncode.Test_Decode_String_Length_Larger_Than_Stream_Raises_Exception;
var
  Raised: boolean;
begin
  //The length is far more than the data. It must fail without allocating that much memory.
  Raised := False;
  try
    FEncoded := Decode('999999999:abc');
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'A string length larger than the stream must raise an exception');
end;

procedure TTestBEncode.Test_Decode_String_Length_With_Eleven_Digits_Raises_Exception;
var
  Raised: boolean;
begin
  Raised := False;
  try
    FEncoded := Decode('12345678901:abc');
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'A string length with 11 digits must raise an exception');
end;

procedure TTestBEncode.Test_Decode_String_Length_Limit_Is_High_Longint;
begin
  //10 digits, one more than High(longint): refused by the limit
  CheckDecodeRaises('2147483648:abc', 'A length above High(longint) must raise an exception',
    'too large');
  CheckDecodeRaises('9999999999:abc', 'A 10 digit length must raise an exception',
    'too large');
  //High(longint) is allowed by the limit, but the stream is much shorter
  CheckDecodeRaises('2147483647:abc', 'A length that is longer than the stream must raise',
    'end of stream while reading string data');
end;

procedure TTestBEncode.Test_Decode_String_Length_Without_Digits_Raises_Exception;
begin
  CheckDecodeRaises('1x:a', 'A letter inside the length must raise an exception',
    'Invalid character in bencode string length');
  CheckDecodeRaises('-1:a', 'A negative length must raise an exception',
    'Unknown bencode value prefix');
end;

procedure TTestBEncode.Test_Decode_Integer_Beyond_Int64_Raises_Exception;
begin
  CheckDecodeRaises('i9223372036854775808e', 'An integer above High(int64) must raise');
  CheckDecodeRaises('i-9223372036854775809e', 'An integer below Low(int64) must raise');

  FEncoded := Decode('i9223372036854775807e');
  CheckEquals(High(int64), FEncoded.IntegerData, 'High(int64) must be accepted');
  FEncoded.Free;

  FEncoded := Decode('i-9223372036854775808e');
  CheckEquals(Low(int64), FEncoded.IntegerData, 'Low(int64) must be accepted');
end;

procedure TTestBEncode.Test_Decode_Empty_Input_Raises_Exception;
begin
  CheckDecodeRaises('', 'An empty input must raise an exception', 'end of stream');
end;

procedure TTestBEncode.Test_Decode_Unterminated_List_Raises_Exception;
const
  LISTS: array[0..4] of UTF8String = ('l', 'li1e', 'l4:spam', 'll', 'lli1ee');
var
  Str: UTF8String;
begin
  for Str in LISTS do
    CheckDecodeRaises(Str, 'The list ''' + Str + ''' must raise an exception',
      'end of stream');
end;

procedure TTestBEncode.Test_Decode_Unterminated_Dictionary_Raises_Exception;
const
  DICTIONARIES: array[0..3] of UTF8String = ('d', 'd3:cow', 'd3:cow3:moo', 'd3:cowd3:foo');
var
  Str: UTF8String;
begin
  for Str in DICTIONARIES do
    CheckDecodeRaises(Str, 'The dictionary ''' + Str + ''' must raise an exception',
      'end of stream');
end;

procedure TTestBEncode.Test_Decode_Dictionary_Key_That_Is_Not_A_String_Raises_Exception;
const
  DICTIONARIES: array[0..3] of UTF8String =
    ('di1e3:mooe', 'dl3:cowe3:mooe', 'dd3:cow3:mooe3:mooe', 'dx3:cow3:mooe');
var
  Str: UTF8String;
begin
  for Str in DICTIONARIES do
    CheckDecodeRaises(Str, 'The dictionary ''' + Str + ''' must raise an exception',
      'dictionary key');
end;

procedure TTestBEncode.Test_Decode_Nested_Lists_Within_The_Limit;
const
  DEPTH = 200;
begin
  FEncoded := Decode(StringOfChar('l', DEPTH) + StringOfChar('e', DEPTH));

  CheckEquals(Ord(befList), Ord(FEncoded.Format), 'Wrong format');
  CheckEquals(1, FEncoded.ListData.Count, 'Wrong element count');
end;

procedure TTestBEncode.Test_Decode_Nested_Lists_Beyond_The_Limit_Raises_Exception;
const
  DEPTH = 300;
var
  Raised: boolean;
begin
  Raised := False;
  try
    FEncoded := Decode(StringOfChar('l', DEPTH) + StringOfChar('e', DEPTH));
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'Nested lists beyond the limit must raise an exception');
end;

procedure TTestBEncode.Test_Decode_Nested_Dictionaries_Beyond_The_Limit_Raises_Exception;
var
  Nested: UTF8String;
  i: integer;
  Raised: boolean;
begin
  Nested := '';
  for i := 1 to 300 do
    Nested := Nested + 'd1:a';
  Nested := Nested + 'i1e' + StringOfChar('e', 300);

  Raised := False;
  try
    FEncoded := Decode(Nested);
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'Nested dictionaries beyond the limit must raise an exception');
end;

procedure TTestBEncode.Test_Decode_Extremely_Deep_Nesting_Raises_Exception;
const
  //Without a limit this overflows the stack and ends the program
  DEPTH = 1000000;
var
  Raised: boolean;
begin
  Raised := False;
  try
    FEncoded := Decode(StringOfChar('l', DEPTH));
  except
    on E: EAssertionFailedError do raise;
    on E: Exception do Raised := True;
  end;
  Check(Raised, 'Extremely deep nesting must raise an exception');
end;

initialization
  RegisterTest(TTestBEncode);
end.
