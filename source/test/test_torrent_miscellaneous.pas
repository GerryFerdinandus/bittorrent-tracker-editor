// SPDX-License-Identifier: MIT
unit test_torrent_miscellaneous;

{
  Tracker list logic of torrent_miscellaneous: list combining, sanitizing, URL checks and
  the decoding of the console parameters. All in memory, except the folder scan.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, torrent_miscellaneous, main_common;

type

  { TTestTorrentMiscellaneous }

  TTestTorrentMiscellaneous = class(TTestCase)
  private
    FTrackerList: TTrackerList;
    FPresentTorrentTrackers: TStringList;
    FTempFolder: string;

    //Combine with the standard fixture and check the order of the final list.
    procedure CheckCombine(Order: TTrackerListOrder; Expected: array of string);

    procedure CheckStringList(Expected: array of string; Actual: TStrings;
      const Msg: string);

    //Result of ConsoleModeDecodeArguments for the given arguments
    function Decode(Arguments: array of string; out FileNameOrDirStr: UTF8String): boolean;
    procedure CreateEmptyFile(const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Combine_U0_Insert_Before_Keep_New_Intact;
    procedure Test_Combine_U1_Insert_Before_Keep_Original_Intact;
    procedure Test_Combine_U2_Append_After_Keep_New_Intact;
    procedure Test_Combine_U3_Append_After_Keep_Original_Intact;
    procedure Test_Combine_U4_Sort;
    procedure Test_Combine_U5_Insert_Before_Remove_Nothing;
    procedure Test_Combine_U6_Append_After_Remove_Nothing;
    procedure Test_Combine_U7_Randomize_Keeps_All_Trackers;
    procedure Test_Combine_Ban_List_And_Deselected_List_Remove_Trackers;
    procedure Test_Combine_Added_Tracker_Overrides_Deselected;
    procedure Test_Combine_Does_Not_Change_Callers_Lists;
    procedure Test_Combine_Remove_Nothing_Keeps_Banned_And_Deselected;

    procedure Test_Sanitize_Removes_Comment_Spaces_And_Tabs;
    procedure Test_Sanitize_Keeps_Empty_Lines_And_UTF8;
    procedure Test_RemoveTrackersFromList_Removes_Matches_Only;
    procedure Test_AddButIgnoreDuplicates_Unsorted_List;
    procedure Test_AddButIgnoreDuplicates_Sorted_List;
    procedure Test_Randomize_Empty_And_Single_Item_List;
    procedure Test_Randomize_Keeps_All_Items;

    procedure Test_ValidTrackerURL;
    procedure Test_WebTorrentTrackerURL;
    procedure Test_TrackerURLWithAnnounce;

    procedure Test_DecodeConsoleUpdateParameter_Valid_Range;
    procedure Test_DecodeConsoleUpdateParameter_Invalid_Values;

    procedure Test_LoadTorrentViaDir_Empty_And_Missing_Folder;
    procedure Test_LoadTorrentViaDir_Without_Torrent_Files;

    procedure Test_Arguments_None;
    procedure Test_Arguments_One_Path_Sorts;
    procedure Test_Arguments_Update_Parameter_First_Or_Second;
    procedure Test_Arguments_Without_Update_Parameter;
    procedure Test_Arguments_Invalid_Update_Parameter;
    procedure Test_Arguments_SAC_And_SOURCE;
    procedure Test_Arguments_Empty_SOURCE_Removes_Source_Tag;
    procedure Test_Arguments_SOURCE_Without_Value;
  end;

implementation

const
  ADDED_1 = 'udp://a1.test/announce';
  PRESENT_1 = 'udp://p1.test/announce';
  PRESENT_2 = 'udp://p2.test/announce';
  PRESENT_3 = 'udp://p3.test/announce';
  OTHER_1 = 'udp://x1.test/announce';

{ TTestTorrentMiscellaneous }

procedure TTestTorrentMiscellaneous.SetUp;
begin
  CreateTrackerList(FTrackerList);
  FPresentTorrentTrackers := TStringList.Create;
  FTempFolder := IncludeTrailingPathDelimiter(GetTempDir) + 'test_torrent_miscellaneous' +
    PathDelim;

  //The new trackers. PRESENT_2 is also inside the torrent.
  FTrackerList.TrackerAddedByUserList.Add(ADDED_1);
  FTrackerList.TrackerAddedByUserList.Add(PRESENT_2);

  //The trackers inside all the torrent files that the user keeps.
  FTrackerList.TrackerFromInsideTorrentFilesList.Add(PRESENT_1);
  FTrackerList.TrackerFromInsideTorrentFilesList.Add(PRESENT_3);
  FTrackerList.TrackerFromInsideTorrentFilesList.Add(OTHER_1);

  //The trackers inside the one torrent file that is updated.
  FPresentTorrentTrackers.Add(PRESENT_1);
  FPresentTorrentTrackers.Add(PRESENT_2);
  FPresentTorrentTrackers.Add(PRESENT_3);
end;

procedure TTestTorrentMiscellaneous.TearDown;
begin
  FPresentTorrentTrackers.Free;
  FreeTrackerList(FTrackerList);
end;

procedure TTestTorrentMiscellaneous.CheckStringList(Expected: array of string;
  Actual: TStrings; const Msg: string);
var
  i: integer;
begin
  CheckEquals(Length(Expected), Actual.Count, Msg + ': wrong count');
  for i := 0 to High(Expected) do
    CheckEquals(Expected[i], Actual[i], Msg + ': wrong item ' + IntToStr(i));
end;

procedure TTestTorrentMiscellaneous.CheckCombine(Order: TTrackerListOrder;
  Expected: array of string);
begin
  CombineFiveTrackerListToOne(Order, FTrackerList, FPresentTorrentTrackers);
  CheckStringList(Expected, FTrackerList.TrackerFinalList, 'Final list');
end;

function TTestTorrentMiscellaneous.Decode(Arguments: array of string;
  out FileNameOrDirStr: UTF8String): boolean;
var
  ArgumentList: TStringList;
  Argument: string;
begin
  ArgumentList := TStringList.Create;
  try
    for Argument in Arguments do
      ArgumentList.Add(Argument);
    Result := ConsoleModeDecodeArguments(ArgumentList, FileNameOrDirStr, FTrackerList);
  finally
    ArgumentList.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.CreateEmptyFile(const FileName: string);
var
  EmptyFile: TStringList;
begin
  EmptyFile := TStringList.Create;
  try
    EmptyFile.SaveToFile(FileName);
  finally
    EmptyFile.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U0_Insert_Before_Keep_New_Intact;
begin
  //New trackers first and in their own order, the duplicate of the torrent moves with them.
  CheckCombine(tloInsertNewBeforeAndKeepNewIntact,
    [ADDED_1, PRESENT_2, PRESENT_1, PRESENT_3, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U1_Insert_Before_Keep_Original_Intact;
begin
  //The torrent keeps its own order, the duplicate is removed from the new trackers.
  CheckCombine(tloInsertNewBeforeAndKeepOriginalIntact,
    [ADDED_1, PRESENT_1, PRESENT_2, PRESENT_3, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U2_Append_After_Keep_New_Intact;
begin
  //The new trackers are last and in their own order, the duplicate is removed from the torrent.
  CheckCombine(tloAppendNewAfterAndKeepNewIntact,
    [PRESENT_1, PRESENT_3, ADDED_1, PRESENT_2, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U3_Append_After_Keep_Original_Intact;
begin
  CheckCombine(tloAppendNewAfterAndKeepOriginalIntact,
    [PRESENT_1, PRESENT_2, PRESENT_3, ADDED_1, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U4_Sort;
begin
  CheckCombine(tloSort, [ADDED_1, PRESENT_1, PRESENT_2, PRESENT_3, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U5_Insert_Before_Remove_Nothing;
begin
  //The trackers of the other torrent files are not added.
  CheckCombine(tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing,
    [ADDED_1, PRESENT_1, PRESENT_2, PRESENT_3]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U6_Append_After_Remove_Nothing;
begin
  CheckCombine(tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing,
    [PRESENT_1, PRESENT_2, PRESENT_3, ADDED_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_U7_Randomize_Keeps_All_Trackers;
var
  Result: TStringList;
begin
  CombineFiveTrackerListToOne(tloRandomize, FTrackerList, FPresentTorrentTrackers);

  Result := TStringList.Create;
  try
    Result.Assign(FTrackerList.TrackerFinalList);
    Result.Sort;
    CheckStringList([ADDED_1, PRESENT_1, PRESENT_2, PRESENT_3, OTHER_1], Result,
      'Random list');
  finally
    Result.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Combine_Ban_List_And_Deselected_List_Remove_Trackers;
begin
  FTrackerList.TrackerBanByUserList.Add(PRESENT_1);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(OTHER_1);

  CheckCombine(tloInsertNewBeforeAndKeepNewIntact, [ADDED_1, PRESENT_2, PRESENT_3]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_Added_Tracker_Overrides_Deselected;
begin
  //ADDED_1 is added by the user and also deselected: the user wins. PRESENT_3 stays deselected.
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(ADDED_1);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(PRESENT_3);

  CheckCombine(tloInsertNewBeforeAndKeepNewIntact,
    [ADDED_1, PRESENT_2, PRESENT_1, OTHER_1]);
end;

procedure TTestTorrentMiscellaneous.Test_Combine_Does_Not_Change_Callers_Lists;
begin
  FTrackerList.TrackerBanByUserList.Add(PRESENT_1);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(ADDED_1);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(PRESENT_3);

  CombineFiveTrackerListToOne(tloSort, FTrackerList, FPresentTorrentTrackers);

  CheckStringList([PRESENT_1], FTrackerList.TrackerBanByUserList, 'Ban list');
  CheckStringList([ADDED_1, PRESENT_3], FTrackerList.TrackerManuallyDeselectedByUserList,
    'Deselected list');
  CheckStringList([ADDED_1, PRESENT_2], FTrackerList.TrackerAddedByUserList, 'Added list');
  CheckStringList([PRESENT_1, PRESENT_2, PRESENT_3], FPresentTorrentTrackers,
    'Trackers of the torrent');
end;

procedure TTestTorrentMiscellaneous.Test_Combine_Remove_Nothing_Keeps_Banned_And_Deselected;
begin
  FTrackerList.TrackerBanByUserList.Add(PRESENT_1);
  FTrackerList.TrackerManuallyDeselectedByUserList.Add(PRESENT_3);

  //The remove nothing modes must neither remove a tracker nor clear the lists of the caller.
  CheckCombine(tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing,
    [PRESENT_1, PRESENT_2, PRESENT_3, ADDED_1]);
  CheckStringList([PRESENT_1], FTrackerList.TrackerBanByUserList, 'Ban list');
  CheckStringList([PRESENT_3], FTrackerList.TrackerManuallyDeselectedByUserList,
    'Deselected list');

  CheckCombine(tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing,
    [ADDED_1, PRESENT_1, PRESENT_2, PRESENT_3]);
  CheckStringList([PRESENT_1], FTrackerList.TrackerBanByUserList, 'Ban list');
  CheckStringList([PRESENT_3], FTrackerList.TrackerManuallyDeselectedByUserList,
    'Deselected list');
end;

procedure TTestTorrentMiscellaneous.Test_Sanitize_Removes_Comment_Spaces_And_Tabs;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('  udp://a.test/announce  ');
    Lines.Add('udp://b.test/announce # comment');
    Lines.Add('udp://c.test/announce' + #9 + '# comment');
    Lines.Add(#9 + 'udp://d.test/announce' + #9);

    SanitizeTrackerList(Lines);

    CheckStringList(['udp://a.test/announce', 'udp://b.test/announce',
      'udp://c.test/announce', 'udp://d.test/announce'], Lines, 'Sanitized list');
  finally
    Lines.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Sanitize_Keeps_Empty_Lines_And_UTF8;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('');
    Lines.Add('   ');
    //A multi byte character must survive and must not hide a comment
    Lines.Add('udp://t' + #$C3#$A9 + '.test/announce # ' + #$C3#$A9);

    SanitizeTrackerList(Lines);

    CheckStringList(['', '', 'udp://t' + #$C3#$A9 + '.test/announce'], Lines,
      'Sanitized list');
  finally
    Lines.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_RemoveTrackersFromList_Removes_Matches_Only;
var
  RemoveList, UpdatedList: TStringList;
begin
  RemoveList := TStringList.Create;
  UpdatedList := TStringList.Create;
  try
    UpdatedList.Add(PRESENT_1);
    UpdatedList.Add(PRESENT_2);
    UpdatedList.Add(PRESENT_3);

    //A spaces around the item is ignored, a tracker that is not present is ignored.
    RemoveList.Add('  ' + PRESENT_2 + ' ');
    RemoveList.Add(OTHER_1);

    RemoveTrackersFromList(RemoveList, UpdatedList);

    CheckStringList([PRESENT_1, PRESENT_3], UpdatedList, 'Updated list');
    CheckEquals(2, RemoveList.Count, 'The remove list must stay unchanged');
  finally
    UpdatedList.Free;
    RemoveList.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_AddButIgnoreDuplicates_Unsorted_List;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Sorted := False;
    AddButIgnoreDuplicates(List, PRESENT_2);
    AddButIgnoreDuplicates(List, PRESENT_1);
    AddButIgnoreDuplicates(List, PRESENT_2);

    //The order of adding is kept
    CheckStringList([PRESENT_2, PRESENT_1], List, 'Unsorted list');
  finally
    List.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_AddButIgnoreDuplicates_Sorted_List;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Duplicates := dupIgnore;
    List.Sorted := True;
    AddButIgnoreDuplicates(List, PRESENT_2);
    AddButIgnoreDuplicates(List, PRESENT_1);
    AddButIgnoreDuplicates(List, PRESENT_2);

    CheckStringList([PRESENT_1, PRESENT_2], List, 'Sorted list');
  finally
    List.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Randomize_Empty_And_Single_Item_List;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    RandomizeTrackerList(List);
    CheckEquals(0, List.Count, 'An empty list must stay empty');

    List.Add(PRESENT_1);
    RandomizeTrackerList(List);
    CheckStringList([PRESENT_1], List, 'A single item list');
  finally
    List.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Randomize_Keeps_All_Items;
var
  List: TStringList;
  i: integer;
begin
  List := TStringList.Create;
  try
    for i := 1 to 20 do
      List.Add('udp://t' + IntToStr(i) + '.test/announce');

    RandomizeTrackerList(List);

    CheckEquals(20, List.Count, 'Wrong count');
    for i := 1 to 20 do
      Check(List.IndexOf('udp://t' + IntToStr(i) + '.test/announce') >= 0,
        'Tracker ' + IntToStr(i) + ' is lost');
  finally
    List.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_ValidTrackerURL;
begin
  CheckTrue(ValidTrackerURL('udp://a.test:6969/announce'), 'udp');
  CheckTrue(ValidTrackerURL('http://a.test/announce'), 'http');
  CheckTrue(ValidTrackerURL('https://a.test/announce'), 'https');
  CheckTrue(ValidTrackerURL('ws://a.test'), 'ws');
  CheckTrue(ValidTrackerURL('wss://a.test'), 'wss');

  CheckFalse(ValidTrackerURL(''), 'empty');
  CheckFalse(ValidTrackerURL('ftp://a.test/announce'), 'ftp');
  CheckFalse(ValidTrackerURL('a.test/announce'), 'no scheme');
  CheckFalse(ValidTrackerURL('udp:/a.test/announce'), 'one slash');
  CheckFalse(ValidTrackerURL(' udp://a.test/announce'), 'The caller must trim the URL');
  CheckFalse(ValidTrackerURL('xhttp://a.test/announce'), 'The scheme must be at the begin');
end;

procedure TTestTorrentMiscellaneous.Test_WebTorrentTrackerURL;
begin
  CheckTrue(WebTorrentTrackerURL('ws://a.test'), 'ws');
  CheckTrue(WebTorrentTrackerURL('wss://a.test'), 'wss');

  CheckFalse(WebTorrentTrackerURL('udp://a.test/announce'), 'udp');
  CheckFalse(WebTorrentTrackerURL('http://a.test/announce'), 'http');
  CheckFalse(WebTorrentTrackerURL('https://a.test/announce'), 'https');
  CheckFalse(WebTorrentTrackerURL(''), 'empty');
end;

procedure TTestTorrentMiscellaneous.Test_TrackerURLWithAnnounce;
begin
  CheckTrue(TrackerURLWithAnnounce('udp://a.test:6969/announce'), '/announce');
  CheckTrue(TrackerURLWithAnnounce('http://a.test/announce.php'), '/announce.php');
  CheckTrue(TrackerURLWithAnnounce('https://a.test/passkey/announce'), 'passkey in the path');

  CheckFalse(TrackerURLWithAnnounce('udp://a.test:6969'), 'no announce');
  CheckFalse(TrackerURLWithAnnounce('http://a.test/xannounce'), 'part of a name');
  CheckFalse(TrackerURLWithAnnounce('http://a.test/announce/'), 'trailing slash');
  //A passkey as query needs the SkipAnnounceCheck option
  CheckFalse(TrackerURLWithAnnounce('https://a.test/announce?passkey=abc'), 'passkey as query');
end;

procedure TTestTorrentMiscellaneous.Test_DecodeConsoleUpdateParameter_Valid_Range;
var
  Order: TTrackerListOrder;
begin
  for Order := Low(TTrackerListOrder) to High(TTrackerListOrder) do
  begin
    CheckTrue(DecodeConsoleUpdateParameter('-U' + IntToStr(Ord(Order)), FTrackerList),
      '-U' + IntToStr(Ord(Order)) + ' must be accepted');
    CheckEquals(Ord(Order), Ord(FTrackerList.TrackerListOrderForUpdatedTorrent),
      'Wrong order for -U' + IntToStr(Ord(Order)));
  end;
  CheckEquals(0, FTrackerList.LogStringList.Count, 'No error expected');
end;

procedure TTestTorrentMiscellaneous.Test_DecodeConsoleUpdateParameter_Invalid_Values;
const
  INVALID_PARAMETERS: array[0..8] of string =
    ('-U8', '-U9', '-U', '-U10', '-Ux', '-U-', 'U1', '-u1', '');
var
  Parameter: string;
begin
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloAppendNewAfterAndKeepNewIntact;

  for Parameter in INVALID_PARAMETERS do
  begin
    FTrackerList.LogStringList.Clear;

    CheckFalse(DecodeConsoleUpdateParameter(Parameter, FTrackerList),
      '"' + Parameter + '" must be rejected');
    CheckEquals(1, FTrackerList.LogStringList.Count,
      'One error expected for "' + Parameter + '"');
    CheckEquals(Ord(tloAppendNewAfterAndKeepNewIntact),
      Ord(FTrackerList.TrackerListOrderForUpdatedTorrent),
      'The order must stay unchanged for "' + Parameter + '"');
  end;
end;

procedure TTestTorrentMiscellaneous.Test_LoadTorrentViaDir_Empty_And_Missing_Folder;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    ForceDirectories(FTempFolder + 'empty');
    try
      CheckFalse(LoadTorrentViaDir(FTempFolder + 'empty', Files), 'Empty folder');
      CheckEquals(0, Files.Count, 'Empty folder must give no files');

      CheckFalse(LoadTorrentViaDir(FTempFolder + 'no_such_folder', Files),
        'Missing folder');
      CheckEquals(0, Files.Count, 'Missing folder must give no files');
    finally
      RemoveDir(FTempFolder + 'empty');
      RemoveDir(FTempFolder);
    end;
  finally
    Files.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_LoadTorrentViaDir_Without_Torrent_Files;
var
  Files: TStringList;
  SubFolder: string;
begin
  Files := TStringList.Create;
  SubFolder := FTempFolder + 'sub.torrent';
  try
    ForceDirectories(SubFolder);
    CreateEmptyFile(FTempFolder + 'readme.txt');
    CreateEmptyFile(FTempFolder + 'a.torrent.bak');
    CreateEmptyFile(FTempFolder + 'torrent');
    try
      CheckFalse(LoadTorrentViaDir(FTempFolder, Files),
        'Other files and a folder named .torrent are no torrent files');
      CheckEquals(0, Files.Count, 'Wrong file count');
    finally
      DeleteFile(FTempFolder + 'readme.txt');
      DeleteFile(FTempFolder + 'a.torrent.bak');
      DeleteFile(FTempFolder + 'torrent');
      RemoveDir(SubFolder);
      RemoveDir(FTempFolder);
    end;
  finally
    Files.Free;
  end;
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_None;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckFalse(Decode([], FileNameOrDirStr), 'No arguments must fail');
  CheckEquals(1, FTrackerList.LogStringList.Count, 'One error expected');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_One_Path_Sorts;
var
  FileNameOrDirStr: UTF8String;
begin
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloRandomize;

  CheckTrue(Decode(['  C:\torrents  '], FileNameOrDirStr), 'One path must be accepted');
  CheckEquals('C:\torrents', FileNameOrDirStr, 'The path must be trimmed');
  CheckEquals(Ord(tloSort), Ord(FTrackerList.TrackerListOrderForUpdatedTorrent),
    'One parameter must sort');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_Update_Parameter_First_Or_Second;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckTrue(Decode(['-U3', 'torrents'], FileNameOrDirStr), '-U first');
  CheckEquals('torrents', FileNameOrDirStr, 'Wrong path with -U first');
  CheckEquals(Ord(tloAppendNewAfterAndKeepOriginalIntact),
    Ord(FTrackerList.TrackerListOrderForUpdatedTorrent), 'Wrong order with -U first');

  CheckTrue(Decode(['other', '-U6'], FileNameOrDirStr), '-U second');
  CheckEquals('other', FileNameOrDirStr, 'Wrong path with -U second');
  CheckEquals(Ord(tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing),
    Ord(FTrackerList.TrackerListOrderForUpdatedTorrent), 'Wrong order with -U second');

  CheckEquals(0, FTrackerList.LogStringList.Count, 'No error expected');
  CheckFalse(FTrackerList.SkipAnnounceCheck, 'No -SAC given');
  CheckEquals('', FTrackerList.SourceTag, 'No -SOURCE given');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_Without_Update_Parameter;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckFalse(Decode(['torrents', 'other'], FileNameOrDirStr),
    'Two arguments without -U must fail');
  CheckEquals('', FileNameOrDirStr, 'No path expected');
  CheckEquals(1, FTrackerList.LogStringList.Count, 'One error expected');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_Invalid_Update_Parameter;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckFalse(Decode(['-U8', 'torrents'], FileNameOrDirStr), '-U8 must fail');
  CheckEquals(1, FTrackerList.LogStringList.Count, 'One error expected');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_SAC_And_SOURCE;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckTrue(Decode(['torrents', '-U4', '-SAC', '-SOURCE', 'ABC'], FileNameOrDirStr),
    'Valid arguments');
  CheckTrue(FTrackerList.SkipAnnounceCheck, '-SAC must be detected');
  CheckEquals('ABC', FTrackerList.SourceTag, 'Wrong source tag');
  CheckFalse(FTrackerList.RemoveAllSourceTag, 'A source tag is given');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_Empty_SOURCE_Removes_Source_Tag;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckTrue(Decode(['-U4', 'torrents', '-SOURCE', ''], FileNameOrDirStr),
    'Valid arguments');
  CheckEquals('', FTrackerList.SourceTag, 'Wrong source tag');
  CheckTrue(FTrackerList.RemoveAllSourceTag, 'An empty -SOURCE must remove the source tag');
end;

procedure TTestTorrentMiscellaneous.Test_Arguments_SOURCE_Without_Value;
var
  FileNameOrDirStr: UTF8String;
begin
  CheckFalse(Decode(['torrents', '-U4', '-SOURCE'], FileNameOrDirStr),
    '-SOURCE without a value must fail');
  CheckEquals(1, FTrackerList.LogStringList.Count, 'One error expected');
end;

initialization
  RegisterTest(TTestTorrentMiscellaneous);
end.
