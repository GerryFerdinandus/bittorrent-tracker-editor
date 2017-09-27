unit torrent_miscellaneous;

{
 Some generic routine

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  //Updated torrent file trackers list order.
  //Do not change the order of TTrackerListOrder.
  TTrackerListOrder = (

    // Console parameter: -U0
    // Insert new trackers list BEFORE, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the ORIGINAL trackers list.
    tloInsertNewBeforeAndKeepNewIntact = 0,

    // Console parameter: -U1
    // Insert new trackers list BEFORE, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the NEW trackers list.
    tloInsertNewBeforeAndKeepOriginalIntact,

    // Console parameter: -U2
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the ORIGINAL trackers list.
    tloAppendNewAfterAndKeepNewIntact,

    // Console parameter: -U3
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the NEW trackers list.
    tloAppendNewAfterAndKeepOriginalIntact,

    // Console parameter: -U4
    // Sort the trackers list by name.
    tloSort,

    // Console parameter: -U5
    // Append new trackers list BEFORE, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torent may have diferent tracker list!
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing,

    // Console parameter: -U6
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torent may have diferent tracker list!
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing,

    // Console parameter: -U7
    // Randomize the trackers list.
    tloRandomize

    );


  TTrackerList = record
    //The new trackers list order
    TrackerListOrderForUpdatedTorrent: TTrackerListOrder;

    //Trackers that must be put inside the torrent.
    TrackerFinalList,

    //Trackers that we want too add.
    TrackerAddedByUserList,

    //trackers that must not be present inside torrent.
    TrackerBanByUserList,

    //Trackers that are already inside the torrent.
    TrackerFromInsideTorrentFilesList,

    //trackers that must not be present inside torrent.
    TrackerManualyDeselectedByUserList,

    // All the torrent files that must be updated
    TorrentFileNameList,

    //Log string text output
    LogStringList: TStringList;

  end;


procedure RemoveTrackersFromList(RemoveList, UpdatedList: TStringList);

procedure SanatizeTrackerList(StringList: TStringList);

procedure RandomizeTrackerList(StringList: TStringList);

procedure AddButIngnoreDuplicates(StringList: TStringList; const Str: UTF8String);

function ByteSizeToBiggerSizeFormatStr(ByteSize: int64): string;

function LoadTorrentViaDir(const Dir: UTF8String;
  TorrentFilesNameStringList: TStringList): boolean;

function ValidTrackerURL(const TrackerURL: UTF8String): boolean;

procedure CombineFiveTrackerListToOne(TrackerListOrder: TTrackerListOrder;
  var TrackerList: TTrackerList; PresentTorrentTrackerList: TStringList);

function ConsoleModeDecodeParameter(out FileNameOrDirStr: UTF8String;
  var TrackerList: TTrackerList): boolean;

function DecodeConsoleUpdateParameter(const ConsoleUpdateParameter: UTF8String;
  var TrackerList: TTrackerList): boolean;



const
  VALID_TRACKERS_URL: array[0..4] of UTF8String =
    (
    'udp://',
    'http://',
    'https://',
    'ws://',
    'wss://'
    );


implementation

uses LazUTF8, LazFileUtils;

procedure RemoveTrackersFromList(RemoveList, UpdatedList: TStringList);
var
  TrackerStr: string;
  i: integer;
begin
  //Remove the trackers that we do not want in the list
  for TrackerStr in RemoveList do
  begin
    //Find the tracker and remove it from the list.
    i := UpdatedList.IndexOf(UTF8Trim(TrackerStr));
    if i >= 0 then
      UpdatedList.Delete(i);
  end;
end;

procedure SanatizeTrackerList(StringList: TStringList);
var
  TrackerStr: UTF8String;
  i: integer;
  PositionSpace: PtrInt;
begin
  //remove all empty space and comment after the URL

  if StringList.Count > 0 then
  begin
    for i := 0 to StringList.Count - 1 do
    begin
      //process every line one by one
      TrackerStr := StringList[i];

      //remove empty spaces at the begin/end of line
      TrackerStr := UTF8Trim(TrackerStr);

      //find the first 'space' found in line
      PositionSpace := UTF8Pos(' ', TrackerStr);
      if PositionSpace > 0 then
      begin
        // There is a 'space' found
        // Remove everything after this 'space'
        TrackerStr := UTF8LeftStr(TrackerStr, PositionSpace - 1);
      end;

      //write the modified string back
      StringList[i] := TrackerStr;
    end;
  end;
end;

procedure RandomizeTrackerList(StringList: TStringList);
var
  i: integer;
begin
  //The order of the string list must be randomize
  if StringList.Count > 1 then
  begin
    Randomize;
    for i := 0 to StringList.Count - 1 do
    begin
      StringList.Exchange(i, Random(StringList.Count));
    end;
  end;
end;

procedure AddButIngnoreDuplicates(StringList: TStringList; const Str: UTF8String);
begin
  //Stringlist that are not sorted must use IndexOf to ignore Duplicates.
  if not StringList.Sorted then
  begin
    //not sorted version
    if StringList.IndexOf(Str) < 0 then
    begin
      StringList.add(Str);
    end;
  end
  else
  begin
    //sorted version
    StringList.add(Str);
  end;

end;

function ByteSizeToBiggerSizeFormatStr(ByteSize: int64): string;
begin
  if ByteSize >= (1024 * 1024 * 1024) then
    Result := Format('%0.2f GiB', [ByteSize / (1024 * 1024 * 1024)])
  else
  if ByteSize >= (1024 * 1024) then
    Result := Format('%0.2f MiB', [ByteSize / (1024 * 1024)])
  else
  if ByteSize >= (1024) then
    Result := Format('%0.2f KiB', [ByteSize / 1024]);
  Result := Result + Format('  (%d Bytes)', [ByteSize]);
end;


function LoadTorrentViaDir(const Dir: UTF8String;
  TorrentFilesNameStringList: TStringList): boolean;
var
  Info: TSearchRec;
begin
  //place all the torrent file name in TorrentFilesNameStringList
  //  TorrentFilesNameStringList := TStringList.Create;

  if FindFirstUTF8(dir + PathDelim + '*.torrent', faAnyFile, Info) = 0 then
  begin
    //Read all the torrent files inside this dir.
    repeat
      TorrentFilesNameStringList.Add(UTF8Trim(dir + PathDelim + Info.Name));
    until FindNextUTF8(info) <> 0;
  end;
  FindCloseUTF8(Info);

  Result := TorrentFilesNameStringList.Count > 0;

end;



function DecodeConsoleUpdateParameter(ConsoleUpdateParameter: UTF8String;
  var TrackerListOrder: TTrackerListOrder; LogStringList: TStringList = nil): boolean;
var
  i: integer;
begin
  //Decode the '-Ux' x is number [0..4]

  //verify string content.
  Result := (Pos('-U', ConsoleUpdateParameter) = 1) and
    (length(ConsoleUpdateParameter) = 3);

  if Result then
  begin
    //get the number
    Result := TryStrToInt(ConsoleUpdateParameter[3], i);
    if Result then
    begin
      //decode number [0..7]
      case i of
        0: TrackerListOrder := tloInsertNewBeforeAndKeepNewIntact;
        1: TrackerListOrder := tloInsertNewBeforeAndKeepOriginalIntact;
        2: TrackerListOrder := tloAppendNewAfterAndKeepNewIntact;
        3: TrackerListOrder := tloAppendNewAfterAndKeepOriginalIntact;
        4: TrackerListOrder := tloSort;
        5: TrackerListOrder := tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing;
        6: TrackerListOrder := tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing;
        7: TrackerListOrder := tloRandomize;
        else
        begin
          //the number is out of range.
          Result := False;
        end;
      end;
    end;
  end;

  if not Result then
  begin
    if assigned(LogStringList) then
    begin
      LogStringList.Add('ERROR: can not decode update parameter -U : ' +
        ConsoleUpdateParameter);
    end;

  end;
end;




function ConsoleModeDecodeParameter(out FileNameOrDirStr: UTF8String;
  out CountParameter: integer; var TrackerListOrder: TTrackerListOrder;
  LogStringList: TStringList = nil): boolean;
begin

  // Console mode can be started with 2 parameter
  //    Update methode: -U0 , -U1, -U2, -U3, -U4
  //    String with a link to folder or to torrent file. 'C:\dir'

  CountParameter := Paramcount;
  case CountParameter of
    0:
    begin
      if assigned(LogStringList) then
      begin
        LogStringList.Add('ERROR: There are no parameter detected.');
      end;
      Result := False;
      exit;
    end;
    1:
    begin
      //one parameter. Must be a link.
      FileNameOrDirStr := UTF8Trim(ParamStr(1));
      //Keep the same behaviour as the previeus software version.
      //      FTrackerListOrderForUpdatedTorrent := tloSort;
      TrackerListOrder := tloSort;
      Result := True;
    end;
    2:
    begin
      //Two parameters. The user can select the update methode.
      //Check for '-U' contruction as first parameter
      if (Pos('-U', ParamStr(1)) = 1) then
      begin
        //Update parameter is the first parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(1), TrackerListOrder,
          LogStringList);
        FileNameOrDirStr := UTF8Trim(ParamStr(2));
      end
      else
      begin
        //Update parameter is the second parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(2), TrackerListOrder,
          LogStringList);
        FileNameOrDirStr := UTF8Trim(ParamStr(1));
      end;

    end;
    else
    begin
      if assigned(LogStringList) then
      begin
        LogStringList.Add('ERROR: There can only be maximum of 2 parameter. Not: ' +
          IntToStr(ParamCount));
      end;
    end;
      Result := False;
      exit;
  end;

end;

function ValidTrackerURL(const TrackerURL: UTF8String): boolean;
begin
  //TrackerURL should be cleanup with UTF8trim()
  Result := (Pos('http://', TrackerURL) = 1) or (Pos('https://', TrackerURL) = 1) or
    (Pos('ws://', TrackerURL) = 1) or (Pos('wss://', TrackerURL) = 1) or
    (Pos('udp://', TrackerURL) = 1);
end;



procedure CombineFiveTrackerListToOne(TrackerListOrder: TTrackerListOrder;
  var TrackerList: TTrackerList; PresentTorrentTrackerList: TStringList);
var
  TrackerStr: UTF8String;
  TrackerDeselectTempList, TrackerFromInsideOneTorrentFile: TStringList;

begin
  //The new trackers can be added at the begin or at the end of the list.

  // FTrackerFinalList =
  //                   (TrackerFromInsideOneTorrentFile
  //                   + TrackerList.TrackerAddedByUserList
  //                   + TrackerList.TrackerFromInsideTorrentFilesList)
  //                   - TrackerList.TrackerBanByUserList
  //                   - TrackerList.TrackerManualyDeselectedByUserList


  TrackerFromInsideOneTorrentFile := TStringList.Create;

  try
    //Begin with an empty list
    TrackerList.TrackerFinalList.Clear;

    if TrackerListOrder <> tloSort then
    begin

      //Read the trackers inside the torrent file
      //Copy the trackers found in one torrent file to TrackerFromInsideOneTorrentFile
      for TrackerStr in PresentTorrentTrackerList do //FDecodePresentTorrent.TrackerList
      begin
        AddButIngnoreDuplicates(TrackerFromInsideOneTorrentFile, TrackerStr);
      end;

    end;

    //Add the new tracker list before of after the original trackers list inside the torrent file.
    case TrackerListOrder of

      tloInsertNewBeforeAndKeepOriginalIntact:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //original tracker list is second place (Keep original intact)
        RemoveTrackersFromList(TrackerFromInsideOneTorrentFile,
          TrackerList.TrackerFinalList);
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);
      end;


      tloInsertNewBeforeAndKeepNewIntact:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList (keep new instact)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //original tracker list is second place (Not instact when duplicated)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);
      end;


      tloAppendNewAfterAndKeepOriginalIntact:
      begin
        //After

        //original tracker list must be place first. (keep original instact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place after TrackerFromInsideOneTorrentFile (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last.  (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

      end;

      tloAppendNewAfterAndKeepNewIntact:
      begin
        //After

        //original tracker list must be place first. (Not instact when duplicated)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place after TrackerFromInsideOneTorrentFile (keep new instact)
        RemoveTrackersFromList(TrackerList.TrackerAddedByUserList,
          TrackerList.TrackerFinalList);
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not instact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

      end;

      tloSort:
      begin
        //Sort

        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        TrackerList.TrackerFinalList.Sort;
      end;

      tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList.
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //remove duplicate from the list.
        RemoveTrackersFromList(TrackerFromInsideOneTorrentFile,
          TrackerList.TrackerFinalList);

        //original tracker list is second place (Keep original intact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Nothing should be removed
        TrackerList.TrackerManualyDeselectedByUserList.Clear;
      end;


      tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing:
      begin
        //After

        //original tracker list is first place (Keep original intact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place as second TrackerList.TrackerAddedByUserList.
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Nothing should be removed
        TrackerList.TrackerManualyDeselectedByUserList.Clear;
      end;

      tloRandomize:
      begin
        //Randomize

        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIngnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        RandomizeTrackerList(TrackerList.TrackerFinalList);
      end;


      else
      begin
        Assert(True, 'case else: Should never been called. CombineFiveTrackerListToOne');
      end;
    end;

    //Trackers from TrackerList.TrackerAddedByUserList overrule the one from TrackerList.TrackerManualyDeselectedByUserList
    //This is when there is a conflict betwean 'add' and 'remove manual selection'

    //Must keep TrackerList.TrackerManualyDeselectedByUserList intact. Copy it to TrackerDeselectTempList
    TrackerDeselectTempList := TStringList.Create;
    TrackerDeselectTempList.Text :=
      TrackerList.TrackerManualyDeselectedByUserList.Text;
    RemoveTrackersFromList(TrackerList.TrackerAddedByUserList,
      TrackerDeselectTempList);

    //Remove the trackers that we do not want in FTrackerFinalList must be the last step.
    RemoveTrackersFromList(TrackerList.TrackerBanByUserList,
      TrackerList.TrackerFinalList);
    RemoveTrackersFromList(TrackerDeselectTempList, TrackerList.TrackerFinalList);

    //No longer needed
    TrackerDeselectTempList.Free;

  finally
    //No longer needed
    TrackerFromInsideOneTorrentFile.Free;
  end;
end;



function ConsoleModeDecodeParameter(out FileNameOrDirStr: UTF8String;
  var TrackerList: TTrackerList): boolean;
begin
  {
   Console mode can be started with 2 parameter
      Update methode: -U0 , -U1, -U2, -U3, -U4
      String with a link to folder or to torrent file. 'C:\dir'

  }

  case Paramcount of
    0:
    begin
      TrackerList.LogStringList.Add('ERROR: There are no parameter detected.');
      Result := False;
      exit;
    end;
    1:
    begin
      //one parameter. Must be a link.
      FileNameOrDirStr := UTF8Trim(ParamStr(1));
      //Keep the same behaviour as the previeus software version.
      TrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
      Result := True;
    end;
    2:
    begin
      //Two parameters. The user can select the update methode.
      //Check for '-U' contruction as first parameter
      if (Pos('-U', ParamStr(1)) = 1) then
      begin
        //Update parameter is the first parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(1), TrackerList);
        FileNameOrDirStr := UTF8Trim(ParamStr(2));
      end
      else
      begin
        //Update parameter is the second parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(2), TrackerList);
        FileNameOrDirStr := UTF8Trim(ParamStr(1));
      end;

    end;
    else
    begin
      TrackerList.LogStringList.Add(
        'ERROR: There can only be maximum of 2 parameter. Not: ' + IntToStr(ParamCount));
      Result := False;
      exit;
    end;

  end;
end;

function DecodeConsoleUpdateParameter(const ConsoleUpdateParameter: UTF8String;
  var TrackerList: TTrackerList): boolean;
var
  i: integer;
begin
  //Decode the '-Ux' x is number [0..4]

  //verify string content.
  Result := (Pos('-U', ConsoleUpdateParameter) = 1) and
    (length(ConsoleUpdateParameter) = 3);

  if Result then
  begin
    //get the number
    Result := TryStrToInt(ConsoleUpdateParameter[3], i);
    if Result then
    begin

      //check if it is within range
      Result := (i >= Ord(low(TTrackerListOrder))) and
        (i <= Ord(high(TTrackerListOrder)));

      if Result then
        TrackerList.TrackerListOrderForUpdatedTorrent := TTrackerListOrder(i);

{
       //decode number [0..7]
      case i of
        0: TrackerList.TrackerListOrderForUpdatedTorrent := tloInsertNewBeforeAndKeepNewIntact;      //tloInsertNewBeforeAndKeepNewIntact
        1: TrackerList.TrackerListOrderForUpdatedTorrent := tloInsertNewBeforeAndKeepOriginalIntact; //tloInsertNewBeforeAndKeepOriginalIntact
        2: TrackerList.TrackerListOrderForUpdatedTorrent := tloAppendNewAfterAndKeepNewIntact;       //tloAppendNewAfterAndKeepNewIntact
        3: TrackerList.TrackerListOrderForUpdatedTorrent := tloAppendNewAfterAndKeepOriginalIntact; //tloAppendNewAfterAndKeepOriginalIntact
        4: TrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
        5: TrackerList.TrackerListOrderForUpdatedTorrent := tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing;
        6: TrackerList.TrackerListOrderForUpdatedTorrent := tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing;
        7: TrackerList.TrackerListOrderForUpdatedTorrent := tloRandomize;
        else
        begin
          //the number is out of range.
          Result := False;
        end;
      end;
}

    end;
  end;

  if not Result then
  begin
    TrackerList.LogStringList.Add('ERROR: can not decode update parameter -U : ' +
      ConsoleUpdateParameter);
  end;
end;







end.
