// SPDX-License-Identifier: MIT
unit test_miscellaneous;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torrent_miscellaneous;

type
  TVerifyTrackerResult = record
    TrackerListOrder: TTrackerListOrder;

    TrackerOriginal,
    TrackerAdded,
    TrackerRemoved,
    TrackerEndResult: TStringList;

    ErrorString: string;
  end;


function LoadConsoleLog(const FileNameWithPath: string; out StatusOK: boolean;
  out TorrentFilesCount: integer; out TrackersCount: integer): boolean;

function GetProjectRootFolderWithPathDelimiter: string;

function VerifyTrackerResult(var VerifyTracker: TVerifyTrackerResult): boolean;

procedure RemoveLineSeperation(TrackerList: TStringList);

implementation

type
  TTrackerFoundInList = (tf_NotFound, tf_Increamental, tf_Skip);


function GetProjectRootFolderWithPathDelimiter: string;
begin
  //the present folder is /enduser where the executable file is run
  Result := ExtractFilePath(ParamStr(0));
  Result := ExcludeTrailingBackslash(Result);
  Result := ExtractFilePath(Result);
end;

function FindTracker(const Tracker: string; TrackerEndResult: TStringList;
  var OldIndex, NewIndex: integer): TTrackerFoundInList;
begin
  NewIndex := TrackerEndResult.IndexOf(tracker);

  if NewIndex < 0 then
  begin
    Result := tf_NotFound;
  end
  else
  begin
    if (NewIndex = OldIndex + 1) then
      Result := tf_Increamental
    else
      Result := tf_Skip;
  end;
end;

procedure FindTrackerList(ListToBeFound, SearchInlist, TrackerRemoved: TStringList;
  out FirstIndex, LastIndex, ItemsFound: integer;
  out TrackerFoundInList: TTrackerFoundInList);
var
  trackerToBeFoundStr: string;
  IndexFound: integer;
  OldIndex: integer;

begin
  //All ListToBeFound should be present in SearchInlist except when present in TrackerRemoved

  //FirstIndex, LastIndex give information if the list is place in begin or end of the list.

  //ItemsFound -> how many of the ListToBeFound are present in the list.
  //           this is not realy helpfull because the items can be removed via TrackerRemoved

  //TrackerFoundInList
  //                  tf_NotFound -> if this items is also not present in TrackerRemoved
  //                              This is a error condition
  //                  tf_Increamental this is when ListToBeFound is intact
  //                  tf_Skip          this is when ListToBeFound is NOT intact

  //start with not found index range
  FirstIndex := -1;
  LastIndex := -1;
  OldIndex := -1;

  //start with no items found
  ItemsFound := 0;

  //start with not found status
  TrackerFoundInList := tf_NotFound;


  //look for each string one by one
  for trackerToBeFoundStr in ListToBeFound do
  begin
    IndexFound := SearchInlist.IndexOf(trackerToBeFoundStr);

    //It is found or not?
    if IndexFound < 0 then
    begin
      //not found, maybe it is in the TrackerRemoved?
      if TrackerRemoved.IndexOf(trackerToBeFoundStr) < 0 then
      begin
        //not found in TrackerRemoved. This is an error.
        TrackerFoundInList := tf_NotFound;
        exit;
      end;
      //data is found in TrackerRemoved
      //do the next item in the loop
      //nothing to do when there is no data is found
    end
    else
    begin
      //new data is found

      //data is found so increase ItemsFound
      Inc(ItemsFound);

      //keep updating LastIndex where data is found
      LastIndex := IndexFound;

      //update first index only one time in this for loop.
      if FirstIndex < 0 then
      begin
        FirstIndex := IndexFound;

        //Start with tf_Increamental. Will be change to tf_Skip if needed.
        TrackerFoundInList := tf_Increamental;
      end
      else
      begin
        //comprare if this is one step index increase with one items or more.
        if (IndexFound = OldIndex + 1) then
        begin
          //Must not be able to go from tf_Skip -> tf_Increamental
          if TrackerFoundInList <> tf_Skip then
            TrackerFoundInList := tf_Increamental;
        end
        else
          TrackerFoundInList := tf_Skip;
      end;

      //keep updating the previeus index
      OldIndex := IndexFound;
    end;

  end;//for loop

end;

procedure RemoveLineSeperation(TrackerList: TStringList);
var
  i: integer;
begin
  if TrackerList.Count = 0 then
    exit;

  //remove all the empty lines
  for i := TrackerList.Count - 1 downto 0 do
  begin
    if Trim(TrackerList[i]) = '' then
      TrackerList.Delete(i);
  end;
end;

procedure CreateCopyTVerifyTrackerResult(
  const VerifyTrackerResult_Original: TVerifyTrackerResult;
  out VerifyTrackerResult_copy: TVerifyTrackerResult);
begin
  //create working copy
  VerifyTrackerResult_copy.TrackerAdded := TStringList.Create;
  VerifyTrackerResult_copy.TrackerEndResult := TStringList.Create;
  VerifyTrackerResult_copy.TrackerOriginal := TStringList.Create;
  VerifyTrackerResult_copy.TrackerRemoved := TStringList.Create;

  //copy original to copyed version
  VerifyTrackerResult_copy.TrackerAdded.Assign(
    VerifyTrackerResult_Original.TrackerAdded);
  VerifyTrackerResult_copy.TrackerEndResult.Assign(
    VerifyTrackerResult_Original.TrackerEndResult);
  VerifyTrackerResult_copy.TrackerOriginal.Assign(
    VerifyTrackerResult_Original.TrackerOriginal);
  VerifyTrackerResult_copy.TrackerRemoved.Assign(
    VerifyTrackerResult_Original.TrackerRemoved);

  //remove empty lines
  RemoveLineSeperation(VerifyTrackerResult_copy.TrackerAdded);
  RemoveLineSeperation(VerifyTrackerResult_copy.TrackerEndResult);
  RemoveLineSeperation(VerifyTrackerResult_copy.TrackerOriginal);
  RemoveLineSeperation(VerifyTrackerResult_copy.TrackerRemoved);
end;

procedure FreeTVerifyTrackerResult(var VerifyTrackerResult: TVerifyTrackerResult);
begin
  VerifyTrackerResult.TrackerAdded.Free;
  VerifyTrackerResult.TrackerEndResult.Free;
  VerifyTrackerResult.TrackerOriginal.Free;
  VerifyTrackerResult.TrackerRemoved.Free;
end;

function CheckIfEverythingIsCompleat(var VerifyTracker: TVerifyTrackerResult): boolean;
var
  FirstIndex, LastIndex, ItemsFound: integer;
  TrackerFoundInList: TTrackerFoundInList;
  VerifyTrackerResult_copy: TVerifyTrackerResult;

  procedure ListToBeFound(list: TStringList);
  begin
    FindTrackerList(list,
      VerifyTrackerResult_copy.TrackerEndResult,
      VerifyTrackerResult_copy.TrackerRemoved, FirstIndex, LastIndex,
      ItemsFound, TrackerFoundInList);
  end;

begin
  //This is for 'sort' and 'random' list URL order.
  //Only need to check if nothing is missing

  CreateCopyTVerifyTrackerResult(VerifyTracker, VerifyTrackerResult_copy);

  try
    //check if TrackerAdded is present in the end result
    ListToBeFound(VerifyTrackerResult_copy.TrackerAdded);
    if TrackerFoundInList = tf_NotFound then
    begin
      Result := False;
      VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are missing';
      exit;
    end;

    //check if TrackerOriginal is present in the end result
    ListToBeFound(VerifyTrackerResult_copy.TrackerOriginal);
    if TrackerFoundInList = tf_NotFound then
    begin
      Result := False;
      VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are missing';
      exit;
    end;

    //no error found;
    Result := True;

  finally
    //free all the memory
    FreeTVerifyTrackerResult(VerifyTrackerResult_copy);
  end;

end;

function CheckIfStillIntack(var VerifyTracker: TVerifyTrackerResult;
  atBegin_TrackerAdded, MustBeIntact_TrackerAdded: boolean): boolean;
var
  FirstIndex, LastIndex, ItemsFound: integer;
  TrackerFoundInList: TTrackerFoundInList;
  VerifyTrackerResult_copy: TVerifyTrackerResult;

  procedure ListToBeFound(list: TStringList);
  begin
    FindTrackerList(list,
      VerifyTrackerResult_copy.TrackerEndResult,
      VerifyTrackerResult_copy.TrackerRemoved, FirstIndex, LastIndex,
      ItemsFound, TrackerFoundInList);
  end;

begin
  //the TrackerAdded can be place 'at the begin' or 'at the end' of the end result list.
  //the TrackerAdded can be intack or not, if not intact then it must also present some where in the TrackerEndResult
  //    except when it is present in the TrackerRemoved

  CreateCopyTVerifyTrackerResult(VerifyTracker, VerifyTrackerResult_copy);

  try
    //Must find 'the added' and 'the original' in the endresult

    if atBegin_TrackerAdded then
    begin

      if MustBeIntact_TrackerAdded then
      begin
        //check for TrackerAdded, should be place at begin
        ListToBeFound(VerifyTrackerResult_copy.TrackerAdded);
        Result := FirstIndex = 0;
        if not Result then
        begin
          VerifyTracker.ErrorString := 'Error: TrackerAdded should be place at begin';
          exit;
        end;
      end
      else
      begin
        //Problem here. if NOT intact then all the URL can be place every where.
        //There is no check if this is correct or not

        //The only valid check will be done later if the original tracker is place at the correct place.
        ListToBeFound(VerifyTrackerResult_copy.TrackerAdded);

        //do nothing here
      end;

      if TrackerFoundInList = tf_NotFound then
      begin
        Result := False;
        VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are missing';
        exit;
      end;

      if MustBeIntact_TrackerAdded then
      begin
        Result := TrackerFoundInList = tf_Increamental;
        if not Result then
        begin
          VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are not intact';
          exit;
        end;
      end;


      //check for TrackerOriginal, should be place at end
      ListToBeFound(VerifyTrackerResult_copy.TrackerOriginal);
      Result := LastIndex = VerifyTrackerResult_copy.TrackerEndResult.Count - 1;
      if not Result then
      begin
        VerifyTracker.ErrorString := 'Error: TrackerOriginal should be place at end';
        exit;
      end;

      if TrackerFoundInList = tf_NotFound then
      begin
        Result := False;
        VerifyTracker.ErrorString := 'Error: TrackerOriginal trackers are missing';
        exit;
      end;

    end
    else
    begin
      //place at end
      //check for TrackerOriginal, should be place at begin
      ListToBeFound(VerifyTrackerResult_copy.TrackerOriginal);
      Result := FirstIndex = 0;
      if not Result then
      begin
        VerifyTracker.ErrorString := 'Error: TrackerOriginal should be place at begin';
        exit;
      end;

      if TrackerFoundInList = tf_NotFound then
      begin
        Result := False;
        VerifyTracker.ErrorString := 'Error: TrackerOriginal trackers are missing';
        exit;
      end;

      //check for TrackerAdded, should be place at end
      ListToBeFound(VerifyTrackerResult_copy.TrackerAdded);
      Result := LastIndex = VerifyTrackerResult_copy.TrackerEndResult.Count - 1;
      if not Result then
      begin
        VerifyTracker.ErrorString := 'Error: TrackerAdded should be place at end';
        exit;
      end;

      if MustBeIntact_TrackerAdded then
      begin
        Result := TrackerFoundInList = tf_Increamental;
        if not Result then
        begin
          VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are not intact';
          exit;
        end;
      end;

      if TrackerFoundInList = tf_NotFound then
      begin
        Result := False;
        VerifyTracker.ErrorString := 'Error: TrackerAdded trackers are missing';
        exit;
      end;

    end;


  finally
    //free all the memory
    FreeTVerifyTrackerResult(VerifyTrackerResult_copy);
  end;

end;

function CheckValidURL_EndResult(var VerifyTracker: TVerifyTrackerResult): boolean;
var
  tracker_URL: string;

  function IsValid(const str: string): boolean;
  var
    i: integer;
  begin
    for i := low(VALID_TRACKERS_URL) to high(VALID_TRACKERS_URL) do
    begin
      //compare the string
      if (Pos(VALID_TRACKERS_URL[i], str) = 1) then
      begin
        //found correct URL
        Result := True;
        exit;
      end;
    end;
    VerifyTracker.ErrorString := 'Can not validate URL: ' + str;
    Result := False;
  end;

begin
  for tracker_URL in VerifyTracker.TrackerEndResult do
  begin
    //skip empty line
    if tracker_URL = '' then
      Continue;

    if not IsValid(tracker_URL) then
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function CheckIfRemoveIsNotPresent(var VerifyTracker: TVerifyTrackerResult): boolean;
var
  tracker_URL: string;
begin
  //scan the TrackerEndResult for trackers present in TrackerRemoved
  for tracker_URL in VerifyTracker.TrackerRemoved do
  begin
    //it should not be present in TrackerEndResult
    if VerifyTracker.TrackerEndResult.IndexOf(tracker_URL) >= 0 then
    begin
      Result := False;
      VerifyTracker.ErrorString :=
        'This tracker URL should not be present: ' + tracker_URL;
    end;
  end;
  Result := True;
end;

function VerifyTrackerResult(var VerifyTracker: TVerifyTrackerResult): boolean;
var
  atBegin_TrackerAdded, MustBeIntact_TrackerAdded: boolean;
begin

  //Check if URL is valid
  Result := CheckValidURL_EndResult(VerifyTracker);
  if not Result then
  begin
    exit;
  end;

  //There should not be any removed trackers items.
  Result := CheckIfRemoveIsNotPresent(VerifyTracker);
  if not Result then
  begin
    exit;
  end;

  //Must verify if the output is what we expected
  case VerifyTracker.TrackerListOrder of
    // Console parameter: -U0
    // Insert new trackers list BEFORE, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the ORIGINAL trackers list.
    tloInsertNewBeforeAndKeepNewIntact:
    begin
      atBegin_TrackerAdded := True;
      MustBeIntact_TrackerAdded := True;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);

    end;

    // Console parameter: -U1
    // Insert new trackers list BEFORE, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the NEW trackers list.
    tloInsertNewBeforeAndKeepOriginalIntact:
    begin
      atBegin_TrackerAdded := True;
      MustBeIntact_TrackerAdded := False;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);

    end;

    // Console parameter: -U2
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the ORIGINAL trackers list.
    tloAppendNewAfterAndKeepNewIntact:
    begin
      atBegin_TrackerAdded := False;
      MustBeIntact_TrackerAdded := True;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);

    end;

    // Console parameter: -U3
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // And remove possible duplicated trackers from the NEW trackers list.
    tloAppendNewAfterAndKeepOriginalIntact:
    begin
      atBegin_TrackerAdded := False;
      MustBeIntact_TrackerAdded := False;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);

    end;

    // Console parameter: -U4
    // Sort the trackers list by name.
    tloSort:
    begin
      //Must check if all trackers are still present.
      Result := CheckIfEverythingIsCompleat(VerifyTracker);
    end;

    // Console parameter: -U5
    // Append new trackers list BEFORE, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torent may have diferent tracker list!
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing:
    begin
      atBegin_TrackerAdded := True;
      MustBeIntact_TrackerAdded := False;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);
    end;

    // Console parameter: -U6
    // Append new trackers list AFTER, the original trackers list inside the torrent file.
    // Keep original tracker list 'of each individual torrent' unchanged and remove nothing.
    // Every torent may have diferent tracker list!
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing:
    begin
      atBegin_TrackerAdded := False;
      MustBeIntact_TrackerAdded := False;
      Result := CheckIfStillIntack(VerifyTracker, atBegin_TrackerAdded,
        MustBeIntact_TrackerAdded);
    end;

    // Console parameter: -U7
    // Randomize the trackers list.
    tloRandomize:
    begin
      //Must check if all trackers are still present.
      Result := CheckIfEverythingIsCompleat(VerifyTracker);
    end;
    else
    begin
      Assert(False, 'ELSE: VerifyTrackerResult()');
    end;

  end;
end;

function LoadConsoleLog(const FileNameWithPath: string; out StatusOK: boolean;
  out TorrentFilesCount: integer; out TrackersCount: integer): boolean;
var
  ConsoleStringlist: TStringList;
begin
{
 console_log.txt is only created in console mode.
 Show the in console mode the success/failure of the torrent update.
 First line status: 'OK' or 'ERROR: xxxxxxx' xxxxxxx = error description
 Second line files count: '1'
 Third line tracker count: 23
 Second and third line info are only valid if the first line is 'OK'
}

  //Result = true if file can be readed as expected.
  // with 2 lines of values if first line is 'OK'

  ConsoleStringlist := TStringList.Create;
  try
    ConsoleStringlist.LoadFromFile(FileNameWithPath);

    Result := ConsoleStringlist.Count > 0;
    if Result then
    begin
      StatusOK := ConsoleStringlist[0] = CONSOLE_SUCCESS_STATUS;
    end;

    if StatusOK then
    begin
      //The totall lines in the file must be 3
      Result := ConsoleStringlist.Count >= 3;
      if Result then
      begin
        Result := TryStrToInt(ConsoleStringlist[1], TorrentFilesCount);
      end;

      //read only if the TorrentFilesCount is successful
      if Result then
      begin
        Result := TryStrToInt(ConsoleStringlist[2], TrackersCount);
      end;
    end;

  except
    Result := False;
  end;

  ConsoleStringlist.Free;
end;

end.
