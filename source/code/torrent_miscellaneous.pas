// SPDX-License-Identifier: MIT
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
    // Insert new trackers list BEFORE, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torrent may have different tracker list!
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing,

    // Console parameter: -U6
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torrent may have different tracker list!
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
    TrackerManuallyDeselectedByUserList,

    // All the torrent files that must be updated
    TorrentFileNameList,

    //Log string text output
    LogStringList: TStringList;

    // No announce check needed for some private trackers
    SkipAnnounceCheck: boolean;

    // Private tracker may need extra 'info:source' variable
    SourceTag: UTF8String;

    // This is needed if someone want to convert private torrent to public torrent
    // Source tag is not used in public tracker.
    RemoveAllSourceTag: Boolean;

  end;


procedure RemoveTrackersFromList(RemoveList, UpdatedList: TStringList);

procedure SanitizeTrackerList(StringList: TStringList);

procedure RandomizeTrackerList(StringList: TStringList);

procedure AddButIgnoreDuplicates(StringList: TStringList; const Str: UTF8String);

function ByteSizeToBiggerSizeFormatStr(ByteSize: int64): string;

function LoadTorrentViaDir(const Dir: UTF8String;
  TorrentFilesNameStringList: TStringList): boolean;

function ValidTrackerURL(const TrackerURL: UTF8String): boolean;

function WebTorrentTrackerURL(const TrackerURL: UTF8String): boolean;

procedure CombineFiveTrackerListToOne(TrackerListOrder: TTrackerListOrder;
  var TrackerList: TTrackerList; PresentTorrentTrackerList: TStringList);

function ConsoleModeDecodeParameter(out FileNameOrDirStr: UTF8String;
  var TrackerList: TTrackerList): boolean;

function DecodeConsoleUpdateParameter(const ConsoleUpdateParameter: UTF8String;
  var TrackerList: TTrackerList): boolean;

function TrackerURLWithAnnounce(const TrackerURL: UTF8String): boolean;

const
  //Index [3] and [4] must stay the WebTorrent ws:// and wss:// prefixes, in this order.
  //WebTorrentTrackerURL() relies on this fixed layout - do not reorder.
  VALID_TRACKERS_URL: array[0..4] of UTF8String =
    (
    'udp://',
    'http://',
    'https://',
    'ws://',
    'wss://'
    );


  //'add trackers' text file must be place in the same directory as the program.
  FILE_NAME_ADD_TRACKERS: string = 'add_trackers.txt';

  //'remove trackers' text file must be place in the same directory as the program.
  FILE_NAME_REMOVE_TRACKERS: string = 'remove_trackers.txt';

  //'export trackers' text file will be created in the same directory as the program.
  FILE_NAME_EXPORT_TRACKERS: string = 'export_trackers.txt';

  //'log' text file will be saved in the same directory as the program
  // only in the console mode.
  FILE_NAME_CONSOLE_LOG: string = 'console_log.txt';

  CONSOLE_SUCCESS_STATUS: string = 'OK';

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

procedure SanitizeTrackerList(StringList: TStringList);
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
  //Fisher-Yates shuffle, so every permutation is equally likely
  if StringList.Count > 1 then
  begin
    for i := StringList.Count - 1 downto 1 do
    begin
      StringList.Exchange(i, Random(i + 1));
    end;
  end;
end;

procedure AddButIgnoreDuplicates(StringList: TStringList; const Str: UTF8String);
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
    Result := Format('%0.2f KiB', [ByteSize / 1024])
  else
    Result := '';

  Result := Result + Format(' (%d Bytes)', [ByteSize]);
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



function ValidTrackerURL(const TrackerURL: UTF8String): boolean;
var
  i: integer;
begin
  //TrackerURL should be cleanup with UTF8trim()
  Result := False;
  for i := low(VALID_TRACKERS_URL) to high(VALID_TRACKERS_URL) do
  begin
    if Pos(VALID_TRACKERS_URL[i], TrackerURL) = 1 then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function WebTorrentTrackerURL(const TrackerURL: UTF8String): boolean;
var
  i: integer;
begin
  //VALID_TRACKERS_URL[3] and [4] are the WebTorrent ws:// and wss:// prefixes
  Result := False;
  for i := 3 to 4 do
  begin
    if Pos(VALID_TRACKERS_URL[i], TrackerURL) = 1 then
    begin
      Result := True;
      exit;
    end;
  end;
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
  //                   - TrackerList.TrackerManuallyDeselectedByUserList


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
        AddButIgnoreDuplicates(TrackerFromInsideOneTorrentFile, TrackerStr);
      end;

    end;

    //Add the new tracker list before of after the original trackers list inside the torrent file.
    case TrackerListOrder of

      tloInsertNewBeforeAndKeepOriginalIntact:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //original tracker list is second place (Keep original intact)
        RemoveTrackersFromList(TrackerFromInsideOneTorrentFile,
          TrackerList.TrackerFinalList);
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);
      end;


      tloInsertNewBeforeAndKeepNewIntact:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList (keep new intact)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //original tracker list is second place (Not intact when duplicated)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);
      end;


      tloAppendNewAfterAndKeepOriginalIntact:
      begin
        //After

        //original tracker list must be place first. (keep original intact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place after TrackerFromInsideOneTorrentFile (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last.  (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

      end;

      tloAppendNewAfterAndKeepNewIntact:
      begin
        //After

        //original tracker list must be place first. (Not intact when duplicated)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place after TrackerFromInsideOneTorrentFile (keep new intact)
        RemoveTrackersFromList(TrackerList.TrackerAddedByUserList,
          TrackerList.TrackerFinalList);
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //'Others' trackers added as last. (Not intact when duplicated)
        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

      end;

      tloSort:
      begin
        //Sort

        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        TrackerList.TrackerFinalList.Sort;
      end;

      tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing:
      begin
        //Before

        //Must be place as first TrackerList.TrackerAddedByUserList.
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //remove duplicate from the list.
        RemoveTrackersFromList(TrackerFromInsideOneTorrentFile,
          TrackerList.TrackerFinalList);

        //original tracker list is second place (Keep original intact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Nothing should be removed
        TrackerList.TrackerManuallyDeselectedByUserList.Clear;
        TrackerList.TrackerBanByUserList.Clear;
      end;


      tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing:
      begin
        //After

        //original tracker list is first place (Keep original intact)
        for TrackerStr in TrackerFromInsideOneTorrentFile do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Must be place as second TrackerList.TrackerAddedByUserList.
        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        //Nothing should be removed
        TrackerList.TrackerManuallyDeselectedByUserList.Clear;
        TrackerList.TrackerBanByUserList.Clear;
      end;

      tloRandomize:
      begin
        //Randomize

        for TrackerStr in TrackerList.TrackerAddedByUserList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        for TrackerStr in TrackerList.TrackerFromInsideTorrentFilesList do
          AddButIgnoreDuplicates(TrackerList.TrackerFinalList, TrackerStr);

        RandomizeTrackerList(TrackerList.TrackerFinalList);
      end;


      else
      begin
        Assert(False, 'case else: Should never been called. CombineFiveTrackerListToOne');
      end;
    end;

    //Trackers from TrackerList.TrackerAddedByUserList overrule the one from TrackerList.TrackerManuallyDeselectedByUserList
    //This is when there is a conflict between 'add' and 'remove manual selection'

    //Must keep TrackerList.TrackerManuallyDeselectedByUserList intact. Copy it to TrackerDeselectTempList
    TrackerDeselectTempList := TStringList.Create;
    try
      TrackerDeselectTempList.Text :=
        TrackerList.TrackerManuallyDeselectedByUserList.Text;
      RemoveTrackersFromList(TrackerList.TrackerAddedByUserList,
        TrackerDeselectTempList);

      //Remove the trackers that we do not want in FTrackerFinalList must be the last step.
      RemoveTrackersFromList(TrackerList.TrackerBanByUserList,
        TrackerList.TrackerFinalList);
      RemoveTrackersFromList(TrackerDeselectTempList, TrackerList.TrackerFinalList);
    finally
      //No longer needed
      TrackerDeselectTempList.Free;
    end;

  finally
    //No longer needed
    TrackerFromInsideOneTorrentFile.Free;
  end;
end;



function ConsoleModeDecodeParameter(out FileNameOrDirStr: UTF8String;
  var TrackerList: TTrackerList): boolean;
var
  i: integer;
begin
  {
   Console mode can be started with example 2 parameter
      Update method: -U0 , -U1, -U2, -U3, -U4 etc.
      String with a link to folder or to torrent file. 'C:\dir'

   Must keep backward compatible with the first and previous release.
   First or second parameter must be related to -Ux

   other parameter after are optional -SOC and -SOURCE

   example:
   "path_to_folder" -U3 -SAC -SOURCE "ABC"
   -U3 "path_to_folder" -SAC -SOURCE "ABC"
  }

  Result := False;
  case Paramcount of
    0:
    begin
      TrackerList.LogStringList.Add('ERROR: There are no parameter detected.');
      exit;
    end;
    1:
    begin
      //one parameter. Must be a link.
      FileNameOrDirStr := UTF8Trim(ParamStr(1));
      //Keep the same behaviour as the previous software version.
      TrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
      Result := True;
    end;
    else
    begin
      //Two parameters. The user can select the update method.
      //Check for '-U' construction as first parameter
      if (Pos('-U', ParamStr(1)) = 1) then
      begin
        //Update parameter is the first parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(1), TrackerList);
        // second parameter is the file/folder
        FileNameOrDirStr := UTF8Trim(ParamStr(2));
      end
      else
      //Check for '-U' construction as second parameter
      if (Pos('-U', ParamStr(2)) = 1) then
      begin
        // Update parameter is the second parameter
        Result := DecodeConsoleUpdateParameter(ParamStr(2), TrackerList);
        // first parameter MUST be the file/folder
        FileNameOrDirStr := UTF8Trim(ParamStr(1));
      end
      else
      begin
        //Neither parameter starts with '-U': parameters can not be decoded.
        FileNameOrDirStr := '';
        TrackerList.LogStringList.Add(
          'ERROR: Can not find update parameter -U in the given parameters.');
        Result := False;
        exit;
      end;

      //Check for parameter -SAC and -SOURCE.
      //Must be done for both '-Ux' parameter positions.
      for i := 2 to ParamCount do
      begin
        if ParamStr(i) = '-SAC' then
        begin
          TrackerList.SkipAnnounceCheck := True;
          Continue;
        end;

        if ParamStr(i) = '-SOURCE' then
        begin
          if ParamCount < i + 1 then
          begin
            TrackerList.LogStringList.Add(
              'ERROR: There is no value after -SOURCE');
            Result := False;
            exit;
          end;

          // parameter after -SOURCE must be the value.
          TrackerList.SourceTag := ParamStr(i + 1);
          // Empty '' -> remove all source tag
          TrackerList.RemoveAllSourceTag := TrackerList.SourceTag = '';
        end;
      end;

    end;
      //else
      //begin
      //  TrackerList.LogStringList.Add(
      //    'ERROR: There can only be maximum of 2 parameter. Not: ' + IntToStr(ParamCount));
      //  Result := False;
      //  exit;
      //end;

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

    end;
  end;

  if not Result then
  begin
    TrackerList.LogStringList.Add('ERROR: can not decode update parameter -U : ' +
      ConsoleUpdateParameter);
  end;
end;

function TrackerURLWithAnnounce(const TrackerURL: UTF8String): boolean;
const
  ANNOUNCE_STRING: string = '/announce';
  ANNOUNCE_PHP_STRING: string = '/announce.php';
begin
  //TrackerURL must end with ANNOUNCE_STRING
  Result := (RightStr(TrackerURL, length(ANNOUNCE_STRING)) = ANNOUNCE_STRING) or
    (RightStr(TrackerURL, length(ANNOUNCE_PHP_STRING)) = ANNOUNCE_PHP_STRING);

end;

end.
