// SPDX-License-Identifier: MIT
unit test_start_up_parameter;

{
  Tracker Editor can be started via parameter.
  The working of these parameter must be tested.

  ------------------------------------
  There are 4 txt files that are being used for this test
  Every test has its own temp folder with a copy of the program and a copy of the
  torrent files, so the tests do not change the files of the project and do not
  depend on the order of the tests. (macOS: the txt files are in the config folder.)
  The txt files are place in the same folder as the copy of the program

  List of all the trackers that must added.
  add_trackers.txt

  remove_trackers.txt
  List of all the trackers that must removed
  note: if the file is empty then all trackers from the present torrent will be REMOVED.
  note: if the file is not present then no trackers will be automatic removed.

  Check if the program is working as expected:
  log.txt is only created in console mode.
  Show the in console mode the success/failure of the torrent update.
  First line status: 'OK' or 'ERROR: xxxxxxx' xxxxxxx = error description
  Second line files count: '1'
  Third line tracker count: 23
  Second and third line info are only valid if the first line is 'OK'

  Check what the torrent output is:
  export_trackers.txt

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, newtrackon, torrent_miscellaneous,
  test_miscellaneous, decodetorrent;

type
  TConsoleLogData = record
    StatusOK: boolean;
    TorrentFilesCount: integer;
    TrackersCount: integer;
  end;

  { TTestStartUpParameter }

  TTestStartUpParameter = class(TTestCase)
  private
    FFullPathToRoot: string;
    FFullPathToTorrent: string;
    FFullPathToEndUser: string;
    FFullPathToBinary: string;

    //The program of the project. FFullPathToBinary is a copy of it, but without its DLL files.
    FFullPathToOriginalBinary: string;
    //Temp folder of this test, with the copies of the program and the torrent files
    FTestFolder: string;

    FTorrentFilesNameStringList: TStringList;
    FNewTrackon: TNewTrackon;
    FVerifyTrackerResult: TVerifyTrackerResult;
    FExitCode: integer;
    FCommandLine: string;
    FConsoleLogData: TConsoleLogData;

    //'-Ux' may be placed before or after the torrent folder parameter
    FUpdateParameterFirst: boolean;

    function ReadConsoleLogFile: boolean;
    procedure TestParameter(const StartupParameter: TStartupParameter);
    procedure DownloadPreTestTrackerList;
    procedure LoadTrackerListAddAndRemoved;
    procedure CallExecutableFile;
    procedure CopyTrackerEndResultToVerifyTrackerResult;
    procedure CreateEmptyTorrent(const StartupParameter: TStartupParameter);
    procedure TestEmptyTorrentResult;
    procedure CreateFilledTorrent(const StartupParameter: TStartupParameter);
    procedure DownloadNewTrackonTrackers;
    procedure Test_Parameter_Ux(TrackerListOrder: TTrackerListOrder);
    procedure Test_Parameter_U5_U6(TrackerListOrder: TTrackerListOrder);
    procedure Add_One_URL(const StartupParameter: TStartupParameter;
      const tracker_URL: string; TestMustBeSuccess: boolean);
    procedure Verify_SAC_And_SOURCE(UpdateParameterFirst: boolean);

  protected
    function GetProgramName: string; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Parameter_TEST_SSL;
    procedure Test_Tracker_UserInput_All_Different_URL;
    procedure Test_Tracker_UserInput_All_Different_URL_And_SAC;
    procedure Test_Create_Empty_Torrent_And_Then_Filled_It_All_List_Order_Mode;

    procedure Test_Parameter_SAC_And_SOURCE_With_Folder_First;
    procedure Test_Parameter_SAC_And_SOURCE_With_Update_Parameter_First;

    procedure Test_Parameter_U0;
    procedure Test_Parameter_U1;
    procedure Test_Parameter_U2;
    procedure Test_Parameter_U3;
    procedure Test_Parameter_U4;
    procedure Test_Parameter_U5;
    procedure Test_Parameter_U6;
    procedure Test_Parameter_U7;

    procedure Test_Parameter_Invalid_No_Dash_U;

  end;

implementation

uses  LazUTF8, FileUtil, main_common
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

const
  PROGRAM_TO_BE_TESTED_NAME = 'trackereditor';
  TORRENT_FOLDER = 'test_torrent';
  END_USER_FOLDER = 'enduser';

  //there are 5 test torrent files in 'test_torrent' folder.
  TEST_TORRENT_FILES_COUNT = 5;

var
  TestFolderCounter: integer = 0;

//A path with a space is one parameter. A trailing path delimiter before the closing quote
//would escape the quote on Windows, so it is removed.
function QuoteParameter(const Path: string): string;
begin
  Result := '"' + ExcludeTrailingPathDelimiter(Path) + '"';
end;

//The first live tracker that is in no tracker list of the torrent files, '' when there is none.
//OriginalTrackersPerFile has one TStringList in Objects[] for every torrent file.
function FirstLiveTrackerNotInTorrents(Live, OriginalTrackersPerFile: TStringList): UTF8String;
var
  i, j: integer;
  Found: boolean;
begin
  Result := '';
  for i := 0 to Live.Count - 1 do
  begin
    Found := False;
    for j := 0 to OriginalTrackersPerFile.Count - 1 do
      if TStringList(OriginalTrackersPerFile.Objects[j]).IndexOf(Live[i]) >= 0 then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Exit(Live[i]);
  end;
end;

//Copy all the torrent files of FromFolder into ToFolder
procedure CopyTorrentFiles(const FromFolder, ToFolder: string);
var
  Files: TStringList;
  FileName: string;
begin
  Files := TStringList.Create;
  try
    torrent_miscellaneous.LoadTorrentViaDir(ExcludeTrailingPathDelimiter(FromFolder), Files);
    for FileName in Files do
      if not CopyFile(FileName, ToFolder + ExtractFileName(FileName)) then
        raise Exception.Create('Can not copy ' + FileName);
  finally
    Files.Free;
  end;
end;

procedure TTestStartUpParameter.Test_Parameter_U0;
begin
  Test_Parameter_Ux(tloInsertNewBeforeAndKeepNewIntact);
end;

procedure TTestStartUpParameter.Test_Parameter_U1;
begin
  Test_Parameter_Ux(tloInsertNewBeforeAndKeepOriginalIntact);
end;

procedure TTestStartUpParameter.Test_Parameter_U2;
begin
  Test_Parameter_Ux(tloAppendNewAfterAndKeepNewIntact);
end;

procedure TTestStartUpParameter.Test_Parameter_U3;
begin
  Test_Parameter_Ux(tloAppendNewAfterAndKeepOriginalIntact);
end;

procedure TTestStartUpParameter.Test_Parameter_U4;
begin
  Test_Parameter_Ux(tloSort);
end;

procedure TTestStartUpParameter.Test_Parameter_U5;
begin
  Test_Parameter_U5_U6(tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing);
end;

procedure TTestStartUpParameter.Test_Parameter_U6;
begin
  Test_Parameter_U5_U6(tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing);
end;

procedure TTestStartUpParameter.Test_Parameter_U7;
begin
  Test_Parameter_Ux(tloRandomize);
end;

procedure TTestStartUpParameter.Test_Parameter_TEST_SSL;
begin
  // Check if SSL connection is working.
  //The copy of the program has no DLL files, so use the program of the project.
  FFullPathToBinary := FFullPathToOriginalBinary;
  FCommandLine := '-TEST_SSL';
  CallExecutableFile;
  // Exit code should be zero
  CheckEquals(0, FExitCode);
end;

procedure TTestStartUpParameter.Test_Parameter_Invalid_No_Dash_U;
begin
  //Neither parameter starts with '-U'. Decoding must fail with a clear error,
  //not silently continue with an unassigned FileNameOrDirStr.
  FCommandLine := QuoteParameter(FFullPathToTorrent) + ' -BOGUS';
  CallExecutableFile;

  //exit code must indicate failure
  CheckEquals(1, FExitCode);

  //the console log must report the decode failure
  Check(ReadConsoleLogFile, 'Log data is not present');
  Check(not FConsoleLogData.StatusOK,
    'Status should indicate failure when no -U parameter is given');
end;

//procedure TTestStartUpParameter.TestParameter(TrackerListOrder: TTrackerListOrder);
procedure TTestStartUpParameter.TestParameter(const StartupParameter: TStartupParameter);
begin
  FVerifyTrackerResult.StartupParameter := StartupParameter;

  //Fill in the command line parameter '-Ux', x = number
  //Both parameter orders must be supported.
  if FUpdateParameterFirst then
    FCommandLine := format('-U%d %s', [Ord(StartupParameter.TrackerListOrder),
      QuoteParameter(FFullPathToTorrent)])
  else
    FCommandLine := format('%s -U%d', [QuoteParameter(FFullPathToTorrent),
      Ord(StartupParameter.TrackerListOrder)]);

  if StartupParameter.SkipAnnounceCheck then
  begin
    FCommandLine := FCommandLine + ' -SAC';
  end;

  if StartupParameter.SourcePresent then
  begin
    FCommandLine := FCommandLine + ' -SOURCE ' + StartupParameter.SourceText;
  end;
end;

procedure TTestStartUpParameter.DownloadPreTestTrackerList;
begin
  DownloadNewTrackonTrackers;

  //copy all the download to txt file.

  //TrackerList_All have all the trackers available
  FNewTrackon.TrackerList_All.SaveToFile(FFullPathToEndUser + FILE_NAME_ADD_TRACKERS);

  //TrackerList_Dead must remove the dead one.
  FNewTrackon.TrackerList_Dead.SaveToFile(FFullPathToEndUser +
    FILE_NAME_REMOVE_TRACKERS);

end;

procedure TTestStartUpParameter.LoadTrackerListAddAndRemoved;
begin
  FVerifyTrackerResult.TrackerAdded.LoadFromFile(FFullPathToEndUser +
    FILE_NAME_ADD_TRACKERS);

  FVerifyTrackerResult.TrackerRemoved.LoadFromFile(FFullPathToEndUser +
    FILE_NAME_REMOVE_TRACKERS);
end;

procedure TTestStartUpParameter.CallExecutableFile;
var
  OldDir: string;
begin
  //An AppImage writes its txt files to the working directory ($OWD), not next to itself.
  OldDir := GetCurrentDir;
  Check(SetCurrentDir(FFullPathToEndUser), 'Can not change to the work folder');
  try
    //start the test program. This will return the Exit code
    FExitCode := SysUtils.ExecuteProcess(UTF8ToSys(FFullPathToBinary), FCommandLine, []);
  finally
    SetCurrentDir(OldDir);
  end;
end;

procedure TTestStartUpParameter.CopyTrackerEndResultToVerifyTrackerResult;
begin
  FVerifyTrackerResult.TrackerEndResult.LoadFromFile(FFullPathToEndUser +
    FILE_NAME_EXPORT_TRACKERS);

  //remove empty space
  RemoveLineSeparation(FVerifyTrackerResult.TrackerEndResult);

end;

procedure TTestStartUpParameter.CreateEmptyTorrent(
  const StartupParameter: TStartupParameter);
begin
  //write a empty remove trackers
  FVerifyTrackerResult.TrackerRemoved.Clear;
  FVerifyTrackerResult.TrackerRemoved.SaveToFile(FFullPathToEndUser +
    FILE_NAME_REMOVE_TRACKERS);

  //write a empty add trackers
  FVerifyTrackerResult.TrackerAdded.Clear;
  FVerifyTrackerResult.TrackerAdded.SaveToFile(FFullPathToEndUser +
    FILE_NAME_ADD_TRACKERS);

  //Generate the command line parameter
  TestParameter(StartupParameter);

  //call the tracker editor exe file
  CallExecutableFile;
end;

procedure TTestStartUpParameter.TestEmptyTorrentResult;
begin
  //Copy test result into FVerifyTrackerResult
  CopyTrackerEndResultToVerifyTrackerResult;

  Check(FVerifyTrackerResult.TrackerEndResult.Count = 0,
    'The tracker list should be empty.');
end;

procedure TTestStartUpParameter.CreateFilledTorrent(
  const StartupParameter: TStartupParameter);
begin
  DownloadNewTrackonTrackers;

  //write a empty remove trackers
  FVerifyTrackerResult.TrackerRemoved.Clear;
  FVerifyTrackerResult.TrackerRemoved.SaveToFile(FFullPathToEndUser +
    FILE_NAME_REMOVE_TRACKERS);

  //write a some trackers to add. This
  FVerifyTrackerResult.TrackerAdded.Clear;
  FVerifyTrackerResult.TrackerAdded.Add('udp://1.test/announce');

  //Add one torrent that later will be removed
  if FNewTrackon.TrackerList_Dead.Count > 0 then
  begin
    FVerifyTrackerResult.TrackerAdded.Add(FNewTrackon.TrackerList_Dead[0]);
  end;

  //Add one torrent that that must may be removed
  if FNewTrackon.TrackerList_Live.Count > 0 then
  begin
    FVerifyTrackerResult.TrackerAdded.Add(FNewTrackon.TrackerList_Live[0]);
  end;

  FVerifyTrackerResult.TrackerAdded.Add('udp://2.test/announce');
  FVerifyTrackerResult.TrackerAdded.SaveToFile(FFullPathToEndUser +
    FILE_NAME_ADD_TRACKERS);

  //Generate the command line parameter
  TestParameter(StartupParameter);

  //call the tracker editor exe file
  CallExecutableFile;

  //Copy test result into FVerifyTrackerResult
  CopyTrackerEndResultToVerifyTrackerResult;

  Check(FVerifyTrackerResult.TrackerEndResult.Count =
    FVerifyTrackerResult.TrackerAdded.Count,
    'TrackerEndResult should have the same count as TrackerAdded');

  //For the next test load the present content inside the torrent
  FVerifyTrackerResult.TrackerOriginal.Assign(FVerifyTrackerResult.TrackerAdded);

end;

procedure TTestStartUpParameter.DownloadNewTrackonTrackers;
begin
  //download only one time
  if FNewTrackon.TrackerList_All.Count = 0 then
  begin
    //An outage of newtrackon.com is a network issue, not a code defect: skip instead of failing the build.
    if not FNewTrackon.DownloadEverything then
      Ignore('newtrackon.com is unreachable; skipping network-dependent test');
  end;
end;

procedure TTestStartUpParameter.Test_Parameter_Ux(TrackerListOrder: TTrackerListOrder);

var
  OK: boolean;
  StartupParameter: TStartupParameter;
begin
  //Create a torrent with fix torrent items
  //this is the pre test condition
  StartupParameter.TrackerListOrder := tloInsertNewBeforeAndKeepNewIntact;
  StartupParameter.SkipAnnounceCheck := False;
  StartupParameter.SourcePresent := False;
  CreateFilledTorrent(StartupParameter);

  //Download all the trackers .txt files
  DownloadPreTestTrackerList;

  //load all the txt file into memory
  LoadTrackerListAddAndRemoved;

  //Generate the command line parameter
  StartupParameter.TrackerListOrder := TrackerListOrder;
  TestParameter(StartupParameter);

  //call the tracker editor exe file
  CallExecutableFile;

  //Copy test result into FVerifyTrackerResult
  CopyTrackerEndResultToVerifyTrackerResult;

  //check if the test result is correct
  OK := VerifyTrackerResult(FVerifyTrackerResult);
  Check(OK, FVerifyTrackerResult.ErrorString);

  //check the exit code
  CheckEquals(0, FExitCode);

  //Check the logdata status
  Check(ReadConsoleLogFile, 'Log data is not present');
  Check(FConsoleLogData.StatusOK);
  Check(FConsoleLogData.TrackersCount > 0);
  Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);
end;

procedure TTestStartUpParameter.Test_Parameter_U5_U6(TrackerListOrder: TTrackerListOrder);
var
  OriginalTrackersPerFile: TStringList;
  DecodeTorrent: TDecodeTorrent;
  StartupParameter: TStartupParameter;
  OK: boolean;
  i: integer;
  RemovedTracker, LiveTracker: UTF8String;
begin
  //Pre test condition: every torrent file has trackers. Some test torrent files have none.
  StartupParameter.TrackerListOrder := tloInsertNewBeforeAndKeepNewIntact;
  StartupParameter.SkipAnnounceCheck := False;
  StartupParameter.SourcePresent := False;
  CreateFilledTorrent(StartupParameter);

  //Every torrent file may already have its own different tracker list, capture it first
  OriginalTrackersPerFile := TStringList.Create;
  DecodeTorrent := TDecodeTorrent.Create;
  try
    for i := 0 to FTorrentFilesNameStringList.Count - 1 do
    begin
      Check(DecodeTorrent.DecodeTorrent(FTorrentFilesNameStringList[i]),
        'Failed to decode torrent before update: ' + FTorrentFilesNameStringList[i]);
      OriginalTrackersPerFile.AddObject(FTorrentFilesNameStringList[i], TStringList.Create);
      TStringList(OriginalTrackersPerFile.Objects[i]).Assign(DecodeTorrent.TrackerList);
    end;

    DownloadNewTrackonTrackers;

    //write some trackers to add.
    //Use tracker literals unique per TrackerListOrder: a tracker that is added by this
    //test must never look like a tracker that was already in the torrent files.
    FVerifyTrackerResult.TrackerAdded.Clear;
    FVerifyTrackerResult.TrackerAdded.Add('udp://' + IntToStr(Ord(TrackerListOrder)) +
      'a.test/announce');
    //A live tracker from the internet changes every day. It must not be a tracker that
    //is already inside a torrent file, because then it is not an 'added' tracker.
    LiveTracker := FirstLiveTrackerNotInTorrents(FNewTrackon.TrackerList_Live,
      OriginalTrackersPerFile);
    if LiveTracker <> '' then
      FVerifyTrackerResult.TrackerAdded.Add(LiveTracker);
    FVerifyTrackerResult.TrackerAdded.Add('udp://' + IntToStr(Ord(TrackerListOrder)) +
      'b.test/announce');
    FVerifyTrackerResult.TrackerAdded.SaveToFile(FFullPathToEndUser +
      FILE_NAME_ADD_TRACKERS);

    //this mode must remove nothing, so a tracker requested for removal must still survive
    RemovedTracker := FVerifyTrackerResult.TrackerAdded[0];
    FVerifyTrackerResult.TrackerRemoved.Clear;
    FVerifyTrackerResult.TrackerRemoved.Add(RemovedTracker);
    FVerifyTrackerResult.TrackerRemoved.SaveToFile(FFullPathToEndUser +
      FILE_NAME_REMOVE_TRACKERS);

    //Generate the command line parameter
    StartupParameter.TrackerListOrder := TrackerListOrder;
    StartupParameter.SkipAnnounceCheck := False;
    StartupParameter.SourcePresent := False;
    TestParameter(StartupParameter);

    //call the tracker editor exe file
    CallExecutableFile;

    //check the exit code
    CheckEquals(0, FExitCode);

    //Check the logdata status
    Check(ReadConsoleLogFile, 'Log data is not present');
    Check(FConsoleLogData.StatusOK);
    Check(FConsoleLogData.TrackersCount > 0);
    Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);

    //Every torrent file has its own tracker list, so it must be verified one by one
    for i := 0 to FTorrentFilesNameStringList.Count - 1 do
    begin
      Check(DecodeTorrent.DecodeTorrent(FTorrentFilesNameStringList[i]),
        'Failed to decode torrent after update: ' + FTorrentFilesNameStringList[i]);

      FVerifyTrackerResult.StartupParameter.TrackerListOrder := TrackerListOrder;
      FVerifyTrackerResult.TrackerOriginal.Assign(
        TStringList(OriginalTrackersPerFile.Objects[i]));
      FVerifyTrackerResult.TrackerEndResult.Assign(DecodeTorrent.TrackerList);

      //The message must be read after the call: the call sets ErrorString.
      OK := VerifyTrackerResult(FVerifyTrackerResult);
      Check(OK, FTorrentFilesNameStringList[i] + ': ' + FVerifyTrackerResult.ErrorString);

      Check(DecodeTorrent.TrackerList.IndexOf(RemovedTracker) >= 0,
        FTorrentFilesNameStringList[i] +
        ': tracker requested for removal must survive in RemoveNothing mode');
    end;

  finally
    for i := 0 to OriginalTrackersPerFile.Count - 1 do
      OriginalTrackersPerFile.Objects[i].Free;
    OriginalTrackersPerFile.Free;
    DecodeTorrent.Free;
  end;
end;

procedure TTestStartUpParameter.Add_One_URL(const StartupParameter: TStartupParameter;
  const tracker_URL: string; TestMustBeSuccess: boolean);
begin
  //add one tracker to the 'add_trackers'
  FVerifyTrackerResult.TrackerAdded.Clear;
  FVerifyTrackerResult.TrackerAdded.Add(tracker_URL);

  FVerifyTrackerResult.TrackerAdded.SaveToFile(FFullPathToEndUser +
    FILE_NAME_ADD_TRACKERS);

  //Generate the command line parameter
  TestParameter(StartupParameter);

  //call the tracker editor exe file
  CallExecutableFile;

  //Check the logdata status
  Check(ReadConsoleLogFile, 'Log data is not present');


  if TestMustBeSuccess then
  begin
    //check the exit code. Must be OK
    CheckEquals(0, FExitCode, tracker_URL);

    //the result must be True
    CheckTrue(FConsoleLogData.StatusOK, tracker_URL);
  end
  else
  begin
    //check the exit code. Must be an error
    CheckNotEquals(0, FExitCode, tracker_URL);

    //the result must be false
    CheckFalse(FConsoleLogData.StatusOK, tracker_URL);
  end;

end;


procedure TTestStartUpParameter.Verify_SAC_And_SOURCE(UpdateParameterFirst: boolean);
const
  SOURCE_TAG = 'PARAMETER_ORDER_TEST';
  TRACKER_WITHOUT_ANNOUNCE = 'udp://parameter.order.test';
var
  StartupParameter: TStartupParameter;
  DecodeTorrent: TDecodeTorrent;
  i: integer;
begin
  //'-SAC' and '-SOURCE' must be decoded for both '-Ux' parameter positions.
  FUpdateParameterFirst := UpdateParameterFirst;

  StartupParameter.TrackerListOrder := tloSort;
  StartupParameter.SkipAnnounceCheck := True;
  StartupParameter.SourcePresent := True;
  StartupParameter.SourceText := SOURCE_TAG;

  //A tracker URL without '/announce' is only accepted when -SAC is decoded.
  Add_One_URL(StartupParameter, TRACKER_WITHOUT_ANNOUNCE, True);

  //-SOURCE must have written the source tag into every torrent file.
  DecodeTorrent := TDecodeTorrent.Create;
  try
    Check(FTorrentFilesNameStringList.Count > 0, 'No torrent files found');
    for i := 0 to FTorrentFilesNameStringList.Count - 1 do
    begin
      Check(DecodeTorrent.DecodeTorrent(FTorrentFilesNameStringList[i]),
        'Failed to decode torrent: ' + FTorrentFilesNameStringList[i]);
      CheckEquals(SOURCE_TAG, DecodeTorrent.InfoSource,
        FTorrentFilesNameStringList[i]);
    end;
  finally
    DecodeTorrent.Free;
  end;
end;

procedure TTestStartUpParameter.Test_Parameter_SAC_And_SOURCE_With_Folder_First;
begin
  // "path_to_folder" -U4 -SAC -SOURCE "xxx"
  Verify_SAC_And_SOURCE(False);
end;

procedure TTestStartUpParameter.Test_Parameter_SAC_And_SOURCE_With_Update_Parameter_First;
begin
  // -U4 "path_to_folder" -SAC -SOURCE "xxx"
  Verify_SAC_And_SOURCE(True);
end;

procedure TTestStartUpParameter.Test_Tracker_UserInput_All_Different_URL;
var
  TrackerListOrder: TTrackerListOrder;
  TrackerURL: string;
  StartupParameter: TStartupParameter;
const
  ANNOUNCE = '/announce';
  ANNOUNCE_PHP = '/announce.php';
begin
  StartupParameter.SkipAnnounceCheck := False;
  StartupParameter.SourcePresent := False;

  //Test if all the tracker update mode is working
  for TrackerListOrder in TTrackerListOrder do
  begin
    StartupParameter.TrackerListOrder := TrackerListOrder;

    TrackerURL := 'udp://test.com';
    Add_One_URL(StartupParameter, TrackerURL, False);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE_PHP, True);

    TrackerURL := 'http://test.com';
    Add_One_URL(StartupParameter, TrackerURL, False);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE_PHP, True);

    TrackerURL := 'https://test.com';
    Add_One_URL(StartupParameter, TrackerURL, False);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE_PHP, True);

    //webtorrent may have NOT announce
    TrackerURL := 'ws://test.com';
    Add_One_URL(StartupParameter, TrackerURL, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);

    TrackerURL := 'wss://test.com';
    Add_One_URL(StartupParameter, TrackerURL, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);
  end;
end;

procedure TTestStartUpParameter.Test_Tracker_UserInput_All_Different_URL_And_SAC;
var
  TrackerListOrder: TTrackerListOrder;
  TrackerURL: string;
  StartupParameter: TStartupParameter;
const
  ANNOUNCE = '/announce';
  ANNOUNCE_PHP = '/announce.php';
  ANNOUNCE_private = '/announce?abcd';

  procedure TestStartUpParameter;
  begin
    Add_One_URL(StartupParameter, TrackerURL, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE_PHP, True);
    Add_One_URL(StartupParameter, TrackerURL + ANNOUNCE_private, True);
  end;

  procedure TestAllURL;
  begin
    //Test if all the tracker update mode is working
    for TrackerListOrder in TTrackerListOrder do
    begin
      StartupParameter.TrackerListOrder := TrackerListOrder;

      TrackerURL := 'udp://test.com';
      TestStartUpParameter;

      TrackerURL := 'http://test.com';
      TestStartUpParameter;

      TrackerURL := 'https://test.com';
      TestStartUpParameter;

      TrackerURL := 'ws://test.com';
      TestStartUpParameter;

      TrackerURL := 'wss://test.com';
      TestStartUpParameter;
    end;
  end;

begin
  // Skip announce check.
  StartupParameter.SkipAnnounceCheck := True;
  StartupParameter.SourcePresent := False;
  TestAllURL;

  // Skip announce check and add SOURCE
  StartupParameter.SkipAnnounceCheck := True;
  StartupParameter.SourcePresent := True;
  StartupParameter.SourceText := 'ABCDE';
  TestAllURL;
end;

procedure TTestStartUpParameter.
Test_Create_Empty_Torrent_And_Then_Filled_It_All_List_Order_Mode;
var
  TrackerListOrder: TTrackerListOrder;
  StartupParameter: TStartupParameter;
begin
  //Test if all the mode that support empty torrent creation
  //Empty torrent -> filled torrent -> empty torrent

  StartupParameter.SkipAnnounceCheck := False;
  StartupParameter.SourcePresent := False;

  for TrackerListOrder in TTrackerListOrder do
  begin
    StartupParameter.TrackerListOrder := TrackerListOrder;

    //It is by design that it can not create a empty torrent
    //The 'KeepOriginalIntactAndRemoveNothing' prevent it from deleting the torrent
    //must skip the test for this one
    if TrackerListOrder = tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing then
      continue;

    //It is by design that it can not create a empty torrent
    //The 'KeepOriginalIntactAndRemoveNothing' prevent it from deleting the torrent
    //must skip the test for this one
    if TrackerListOrder = tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing then
      continue;

    //Create empty the torrent
    CreateEmptyTorrent(StartupParameter);
    //check the exit code
    CheckEquals(0, FExitCode);

    TestEmptyTorrentResult;
    //Check the logdata status
    Check(ReadConsoleLogFile, 'Log data is not present');
    Check(FConsoleLogData.StatusOK);
    Check(FConsoleLogData.TrackersCount = 0);
    Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);

    //fill the empty torrent with data
    CreateFilledTorrent(StartupParameter);
    //check the exit code
    CheckEquals(0, FExitCode);

    //Check the logdata status
    Check(ReadConsoleLogFile, 'Log data is not present');
    Check(FConsoleLogData.StatusOK);
    Check(FConsoleLogData.TrackersCount > 0);
    Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);

    //Create empty the torrent again
    CreateEmptyTorrent(StartupParameter);
    //check the exit code
    CheckEquals(0, FExitCode);

    TestEmptyTorrentResult;
    //Check the log data status
    Check(ReadConsoleLogFile, 'Log data is not present');
    Check(FConsoleLogData.StatusOK);
    Check(FConsoleLogData.TrackersCount = 0);
    Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);

  end;
end;

function TTestStartUpParameter.GetProgramName: string;
begin
  Result := PROGRAM_TO_BE_TESTED_NAME;
end;

procedure TTestStartUpParameter.SetUp;
begin
  WriteLn('TTestStartUpParameter.SetUp');
  //Create all the TStringList items
  FVerifyTrackerResult.TrackerOriginal := nil;
  FVerifyTrackerResult.TrackerAdded := nil;
  FVerifyTrackerResult.TrackerRemoved := nil;
  FVerifyTrackerResult.TrackerEndResult := nil;
  try
    FVerifyTrackerResult.TrackerOriginal := TStringList.Create;
    FVerifyTrackerResult.TrackerAdded := TStringList.Create;
    FVerifyTrackerResult.TrackerRemoved := TStringList.Create;
    FVerifyTrackerResult.TrackerEndResult := TStringList.Create;
  except
    //TTestCase.RunBare does not call TearDown when SetUp raises, so free here
    FVerifyTrackerResult.TrackerOriginal.Free;
    FVerifyTrackerResult.TrackerAdded.Free;
    FVerifyTrackerResult.TrackerRemoved.Free;
    FVerifyTrackerResult.TrackerEndResult.Free;
    raise;
  end;

  //Default parameter order: "path_to_folder" -Ux
  FUpdateParameterFirst := False;

  //Create some full path link
  FFullPathToRoot := GetProjectRootFolderWithPathDelimiter;

  //The test works in its own folder: a copy of the torrent files and of the program.
  //The program is started with torrent files that the other tests have not changed.
  Inc(TestFolderCounter);
  FTestFolder := IncludeTrailingPathDelimiter(GetTempDir) + 'test_start_up_parameter_' +
    IntToStr(GetProcessID) + '_' + IntToStr(TestFolderCounter) + PathDelim;
  FFullPathToTorrent := FTestFolder + TORRENT_FOLDER + PathDelim;

  {$IFDEF DARWIN}
  // PATH: ~/.config/test_trackereditor/ -> ~/.config/trackereditor/
  // The path is created in SetUp when it does not exist yet
  // This unit test must use the same working forder as trackereditor
  FFullPathToEndUser := GetAppConfigDir(False);
  FFullPathToEndUser := IncludeTrailingPathDelimiter(FFullPathToEndUser)
                     + '..' + PathDelim;
  FFullPathToEndUser := ExpandFileName(FFullPathToEndUser) + 'trackereditor' + PathDelim;

  //path to the program we want to test. It is not copied, it must use the config folder.
  FFullPathToOriginalBinary := FFullPathToRoot + END_USER_FOLDER + PathDelim
                    + GetProgramName;
  FFullPathToBinary := FFullPathToOriginalBinary;

  {$ELSE DARWIN}
  //The program reads and writes its txt files next to itself
  FFullPathToEndUser := FTestFolder;

  //path to the program we want to test.
  FFullPathToOriginalBinary := FFullPathToRoot + END_USER_FOLDER + PathDelim +
    GetProgramName + ExtractFileExt(ParamStr(0));
  FFullPathToBinary := FTestFolder + ExtractFileName(FFullPathToOriginalBinary);
  {$ENDIF DARWIN}

  FTorrentFilesNameStringList := nil;
  FNewTrackon := nil;
  try
    ForceDirectories(FFullPathToTorrent);
    CopyTorrentFiles(FFullPathToRoot + TORRENT_FOLDER + PathDelim, FFullPathToTorrent);

    {$IFNDEF DARWIN}
    if not CopyFile(FFullPathToOriginalBinary, FFullPathToBinary) then
      raise Exception.Create('Can not copy the program ' + FFullPathToOriginalBinary);
    {$IFDEF UNIX}
    //CopyFile does not keep the execute permission.
    if FpChmod(FFullPathToBinary, &755) <> 0 then
      raise Exception.Create('Can not make the program executable');
    {$ENDIF}
    {$ENDIF DARWIN}

    //fill with torrent file(s)
    FTorrentFilesNameStringList := TStringList.Create;
    torrent_miscellaneous.LoadTorrentViaDir(ExcludeTrailingPathDelimiter(FFullPathToTorrent),
      FTorrentFilesNameStringList);

    FNewTrackon := TNewTrackon.Create;
  except
    //TTestCase.RunBare does not call TearDown when SetUp raises
    TearDown;
    raise;
  end;

  {$IFDEF DARWIN}
  //The folder does not exist yet on a fresh machine (CI), the program is not started yet
  ForceDirectories(FFullPathToEndUser);
  //Delete all the previous test result
  DeleteFile(FFullPathToEndUser + FILE_NAME_CONSOLE_LOG);
  DeleteFile(FFullPathToEndUser + FILE_NAME_EXPORT_TRACKERS);
  DeleteFile(FFullPathToEndUser + FILE_NAME_ADD_TRACKERS);
  DeleteFile(FFullPathToEndUser + FILE_NAME_REMOVE_TRACKERS);
  {$ENDIF DARWIN}
end;

procedure TTestStartUpParameter.TearDown;
begin
  WriteLn('TTestStartUpParameter.TearDown');
  //Free the TStringList items
  FVerifyTrackerResult.TrackerOriginal.Free;
  FVerifyTrackerResult.TrackerAdded.Free;
  FVerifyTrackerResult.TrackerRemoved.Free;
  FVerifyTrackerResult.TrackerEndResult.Free;


  FTorrentFilesNameStringList.Free;
  FNewTrackon.Free;

  //The copies of the program and of the torrent files
  if FTestFolder <> '' then
    DeleteDirectory(FTestFolder, False);
  FTestFolder := '';
end;

function TTestStartUpParameter.ReadConsoleLogFile: boolean;
begin
  Result := LoadConsoleLog(FFullPathToEndUser + FILE_NAME_CONSOLE_LOG,
    FConsoleLogData.StatusOK, FConsoleLogData.TorrentFilesCount,
    FConsoleLogData.TrackersCount);
end;

  {$IFNDEF DARWIN}
type

  { TTestStartUpParameterCli }

  //Runs every TTestStartUpParameter test against trackereditor_cli instead of trackereditor.
  //Excluded on macOS: the macOS build does not produce a trackereditor_cli binary.
  TTestStartUpParameterCli = class(TTestStartUpParameter)
  private
    //Every test of this class uses the copy of the program in the folder of the test. The program
    //reads and writes add_trackers.txt, remove_trackers.txt and console_log.txt next to itself.
    FWorkFolder: string;
    FWorkExe: string;
    FReadOnlyFile: string;
    FLog: TStringList;

    procedure PrepareWorkFolder;
    procedure CreateTorrentFile(const FileName: string; Trackers: array of string;
      const Source: string = '');
    procedure WriteTextFile(const Name: string; Lines: array of string);
    procedure CheckTrackersInFile(const FileName: string; Expected: array of string);
    procedure CheckSource(const FileName, Expected: string);

    //Run the copy of the program. Every argument is passed as it is, an empty one too.
    procedure RunCli(const Args: array of RawByteString);
    procedure CheckSuccessLog(ExpectedTrackerCount: integer);
    procedure CheckErrorLog(const ExpectedFirstLine: string);
  protected
    function GetProgramName: string; override;
    procedure TearDown; override;
  published
    //trackereditor_cli has no networking code and does not support '-TEST_SSL': it is treated
    //like any other invalid single argument, i.e. an unresolvable torrent path/folder.
    procedure Test_Parameter_TEST_SSL;

    //Safe only for trackereditor_cli: unlike the GUI, it never shows a window and always
    //terminates, so a bare 1-parameter invocation can't hang a blocking ExecuteProcess call.
    procedure Test_Parameter_Single_Path_Only;

    //An exception inside the console mode must be written to console_log.txt.
    procedure Test_Exception_Is_Written_To_Console_Log;

    //These tests do not use the torrent files of the project. They never need a network.
    procedure Test_Single_Torrent_File_Path_Without_Update_Parameter;
    procedure Test_Single_Torrent_File_Path_With_Update_Parameter;
    procedure Test_Empty_Remove_Trackers_File_Removes_All_Trackers_Inside_Torrent;
    procedure Test_Missing_Add_Trackers_File_Uses_Recommended_Trackers;
    procedure Test_ReadOnly_Torrent_Is_Reported;
    procedure Test_SOURCE_Without_Value_Fails;
    procedure Test_Unknown_Parameter_Fails_And_Changes_Nothing;
    procedure Test_Announce_Error_Of_Add_Trackers_Is_Logged_Once;
    procedure Test_Empty_SOURCE_Removes_Source_Tag;
    procedure Test_Update_Parameter_U8_Fails;
    procedure Test_Empty_Announce_Is_Not_Counted_As_Tracker;
    procedure Test_Invalid_Add_Trackers_File_Fails_And_Changes_Nothing;
    procedure Test_Unreadable_Remove_Trackers_File_Fails_And_Changes_Nothing;
    procedure Test_Folder_With_Corrupt_Torrent_Fails_And_Changes_Nothing;
    procedure Test_Folder_Without_Torrents_Fails;
    procedure Test_Folder_Name_With_Dot_Is_Accepted;
  end;
  {$ENDIF DARWIN}

{$IFNDEF DARWIN}
function TTestStartUpParameterCli.GetProgramName: string;
begin
  Result := 'trackereditor_cli';
end;

procedure TTestStartUpParameterCli.TearDown;
begin
  //A read only file can not be deleted on Windows
  if FReadOnlyFile <> '' then
    SetFileReadOnly(FReadOnlyFile, False);
  FReadOnlyFile := '';

  FLog.Free;
  FLog := nil;
  FWorkFolder := '';

  inherited TearDown;
end;

procedure TTestStartUpParameterCli.PrepareWorkFolder;
begin
  //The program is the copy in the folder of the test, its text files are next to it
  FWorkFolder := FFullPathToEndUser;
  FWorkExe := FFullPathToBinary;
  FLog := TStringList.Create;
end;

procedure TTestStartUpParameterCli.CreateTorrentFile(const FileName: string;
  Trackers: array of string; const Source: string);
var
  Torrent: UTF8String;
  Stream: TFileStream;
  i: integer;

  function BEncodeString(const Str: UTF8String): UTF8String;
  begin
    Result := IntToStr(Length(Str)) + ':' + Str;
  end;

begin
  //Dictionary keys must be in alphabetical order
  Torrent := 'd';
  if Length(Trackers) > 0 then
  begin
    Torrent := Torrent + '8:announce' + BEncodeString(Trackers[0]) + '13:announce-listl';
    for i := Low(Trackers) to High(Trackers) do
      Torrent := Torrent + 'l' + BEncodeString(Trackers[i]) + 'e';
    Torrent := Torrent + 'e';
  end;
  Torrent := Torrent + '4:info' + 'd6:lengthi1024e4:name8:test.bin' +
    '12:piece lengthi16384e6:pieces20:AAAAAAAAAAAAAAAAAAAA';
  if Source <> '' then
    Torrent := Torrent + '6:source' + BEncodeString(Source);
  Torrent := Torrent + 'ee';

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Torrent[1], Length(Torrent));
  finally
    Stream.Free;
  end;
end;

procedure TTestStartUpParameterCli.WriteTextFile(const Name: string;
  Lines: array of string);
var
  TextFile: TStringList;
  Line: string;
begin
  TextFile := TStringList.Create;
  try
    for Line in Lines do
      TextFile.Add(Line);
    //No line gives an empty file
    TextFile.SaveToFile(FWorkFolder + Name);
  finally
    TextFile.Free;
  end;
end;

procedure TTestStartUpParameterCli.CheckTrackersInFile(const FileName: string;
  Expected: array of string);
var
  Torrent: TDecodeTorrent;
  i: integer;
begin
  Torrent := TDecodeTorrent.Create;
  try
    Check(Torrent.DecodeTorrent(FileName), 'Can not decode ' + FileName);
    CheckEquals(Length(Expected), Torrent.TrackerList.Count,
      'Wrong tracker count in ' + FileName);
    for i := Low(Expected) to High(Expected) do
      CheckEquals(Expected[i], Torrent.TrackerList[i], 'Wrong tracker ' + IntToStr(i));
  finally
    Torrent.Free;
  end;
end;

procedure TTestStartUpParameterCli.CheckSource(const FileName, Expected: string);
var
  Torrent: TDecodeTorrent;
begin
  Torrent := TDecodeTorrent.Create;
  try
    Check(Torrent.DecodeTorrent(FileName), 'Can not decode ' + FileName);
    CheckEquals(Expected, Torrent.InfoSource, 'Wrong source in ' + FileName);
  finally
    Torrent.Free;
  end;
end;

procedure TTestStartUpParameterCli.RunCli(const Args: array of RawByteString);
{$IFDEF WINDOWS}
var
  CommandLine: string;
  Arg: RawByteString;
begin
  //Quote every argument, else an empty argument is lost
  CommandLine := '';
  for Arg in Args do
    CommandLine := CommandLine + ' "' + Arg + '"';
  FExitCode := SysUtils.ExecuteProcess(UTF8ToSys(FWorkExe), Trim(CommandLine), []);
{$ELSE}
begin
  //The string version of ExecuteProcess drops an empty argument
  FExitCode := SysUtils.ExecuteProcess(UTF8ToSys(FWorkExe), Args, []);
{$ENDIF}
  FLog.Clear;
  if FileExists(FWorkFolder + FILE_NAME_CONSOLE_LOG) then
    FLog.LoadFromFile(FWorkFolder + FILE_NAME_CONSOLE_LOG);
end;

procedure TTestStartUpParameterCli.CheckSuccessLog(ExpectedTrackerCount: integer);
begin
  CheckEquals(0, FExitCode, 'Exit code');
  Check(FLog.Count >= 3, 'The log of a success has 3 lines');
  CheckEquals(CONSOLE_SUCCESS_STATUS, FLog[0], 'Status');
  CheckEquals('1', FLog[1], 'Torrent file count');
  CheckEquals(IntToStr(ExpectedTrackerCount), FLog[2], 'Tracker count');
end;

procedure TTestStartUpParameterCli.CheckErrorLog(const ExpectedFirstLine: string);
begin
  CheckEquals(1, FExitCode, 'Exit code');
  Check(FLog.Count > 0, 'The log must have the error');
  CheckEquals(ExpectedFirstLine, FLog[0], 'First line of the log');
end;

procedure TTestStartUpParameterCli.Test_Single_Torrent_File_Path_Without_Update_Parameter;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  //Just the path of one torrent file: sort
  RunCli([TorrentFile]);

  CheckSuccessLog(2);
  CheckTrackersInFile(TorrentFile, ['udp://new.test/announce', 'udp://orig1.test/announce']);
end;

procedure TTestStartUpParameterCli.Test_Single_Torrent_File_Path_With_Update_Parameter;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  //-U3: the original tracker stays first, the new tracker is appended
  RunCli([TorrentFile, '-U3']);

  CheckSuccessLog(2);
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce', 'udp://new.test/announce']);
end;

procedure TTestStartUpParameterCli.
Test_Empty_Remove_Trackers_File_Removes_All_Trackers_Inside_Torrent;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  //remove_trackers.txt is present, but empty: every tracker inside the torrent is removed
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
  WriteTextFile(FILE_NAME_REMOVE_TRACKERS, []);
  RunCli([TorrentFile, '-U4']);
  CheckSuccessLog(1);
  CheckTrackersInFile(TorrentFile, ['udp://new.test/announce']);

  //No remove_trackers.txt: nothing is removed
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
  DeleteFile(FWorkFolder + FILE_NAME_REMOVE_TRACKERS);
  RunCli([TorrentFile, '-U4']);
  CheckSuccessLog(3);
  CheckTrackersInFile(TorrentFile, ['udp://new.test/announce', 'udp://orig1.test/announce',
    'udp://orig2.test/announce']);
end;

procedure TTestStartUpParameterCli.Test_Missing_Add_Trackers_File_Uses_Recommended_Trackers;
var
  TorrentFile: string;
  Torrent: TDecodeTorrent;
  Tracker: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);

  //There is no add_trackers.txt in the folder of the program
  RunCli([TorrentFile, '-U4']);

  CheckSuccessLog(Length(RECOMMENDED_TRACKERS) + 1);
  Torrent := TDecodeTorrent.Create;
  try
    Check(Torrent.DecodeTorrent(TorrentFile), 'Can not decode the torrent');
    Check(Torrent.TrackerList.IndexOf('udp://orig1.test/announce') >= 0,
      'The original tracker must stay');
    for Tracker in RECOMMENDED_TRACKERS do
      Check(Torrent.TrackerList.IndexOf(Tracker) >= 0,
        'The recommended tracker ' + Tracker + ' must be added');
  finally
    Torrent.Free;
  end;
end;

procedure TTestStartUpParameterCli.Test_ReadOnly_Torrent_Is_Reported;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);
  Check(SetFileReadOnly(TorrentFile, True), 'Can not make the torrent read only');
  FReadOnlyFile := TorrentFile;

  RunCli([TorrentFile, '-U4']);

  CheckErrorLog('ERROR: Some torrent files are READ-ONLY and were not updated.');
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
end;

procedure TTestStartUpParameterCli.Test_SOURCE_Without_Value_Fails;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce'], 'OLD');
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([TorrentFile, '-U4', '-SOURCE']);

  CheckErrorLog('ERROR: There is no value after -SOURCE');
  //Nothing is changed
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
  CheckSource(TorrentFile, 'OLD');
end;

procedure TTestStartUpParameterCli.Test_Unknown_Parameter_Fails_And_Changes_Nothing;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce'], 'OLD');
  //The URL has no '/announce': it is only accepted with -SAC
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test']);

  //A misspelled flag
  RunCli([TorrentFile, '-U4', '-SORUCE', 'NEW']);
  CheckErrorLog('ERROR: Unknown parameter: -SORUCE');
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
  CheckSource(TorrentFile, 'OLD');

  //-SAC in the wrong letter case
  RunCli([TorrentFile, '-U4', '-sac']);
  CheckErrorLog('ERROR: Unknown parameter: -sac');
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);

  //-SAC is the value of -SOURCE here: it is not an option, and the value is missing
  RunCli([TorrentFile, '-U4', '-SOURCE', '-SAC']);
  CheckErrorLog('ERROR: There is no value after -SOURCE');
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
  CheckSource(TorrentFile, 'OLD');
end;

procedure TTestStartUpParameterCli.Test_Announce_Error_Of_Add_Trackers_Is_Logged_Once;
const
  NEW_TRACKER = 'udp://new.test';
  //Every mode: the 'remove nothing' modes -U5 and -U6 check the list in an other place
  MODES: array[0..7] of string = ('-U0', '-U1', '-U2', '-U3', '-U4', '-U5', '-U6', '-U7');
var
  TorrentFile, Mode, Line: string;
  ErrorLines: integer;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  //The URL has no '/announce': it is only accepted with -SAC
  WriteTextFile(FILE_NAME_ADD_TRACKERS, [NEW_TRACKER]);

  for Mode in MODES do
  begin
    CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
    RunCli([TorrentFile, Mode]);

    CheckErrorLog(NEW_TRACKER + ' : ERROR: Tracker URL must end with /announce or /announce.php');
    ErrorLines := 0;
    for Line in FLog do
      if Line <> '' then
        Inc(ErrorLines);
    CheckEquals(1, ErrorLines, 'The error must be in the log once, mode ' + Mode);
    CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
  end;
end;

procedure TTestStartUpParameterCli.Test_Empty_SOURCE_Removes_Source_Tag;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce'], 'OLD');
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([TorrentFile, '-U4', '-SOURCE', '']);

  CheckSuccessLog(2);
  CheckSource(TorrentFile, '');
end;

procedure TTestStartUpParameterCli.Test_Empty_Announce_Is_Not_Counted_As_Tracker;
var
  TorrentFile: string;
  Mode: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  //A tracker-less torrent has an empty 'announce'. The empty URL is no tracker, not even for
  //-U5 that removes nothing.
  for Mode in ['-U4', '-U5'] do
  begin
    CreateTorrentFile(TorrentFile, ['']);
    RunCli([TorrentFile, Mode]);
    CheckSuccessLog(1);
    CheckTrackersInFile(TorrentFile, ['udp://new.test/announce']);
  end;
end;

procedure TTestStartUpParameterCli.Test_Update_Parameter_U8_Fails;
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([TorrentFile, '-U8']);

  CheckErrorLog('ERROR: can not decode update parameter -U : -U8');
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce']);
end;

procedure TTestStartUpParameterCli.
Test_Invalid_Add_Trackers_File_Fails_And_Changes_Nothing;
const
  TYPO_TRACKER = 'udpp://typo.test/announce';
var
  TorrentFile: string;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  WriteTextFile(FILE_NAME_ADD_TRACKERS, [TYPO_TRACKER]);

  //No remove_trackers.txt: the error is reported and the torrent is not touched
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
  RunCli([TorrentFile, '-U4']);
  CheckErrorLog(TYPO_TRACKER + ' : ' + InvalidTrackerURLMessage);
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);

  //An empty remove_trackers.txt must not remove the trackers inside the torrent either
  WriteTextFile(FILE_NAME_REMOVE_TRACKERS, []);
  RunCli([TorrentFile, '-U4']);
  CheckErrorLog(TYPO_TRACKER + ' : ' + InvalidTrackerURLMessage);
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
end;

procedure TTestStartUpParameterCli.
Test_Unreadable_Remove_Trackers_File_Fails_And_Changes_Nothing;
var
  TorrentFile: string;
  LockedFile: TFileStream;
begin
  PrepareWorkFolder;
  TorrentFile := FWorkFolder + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);
  WriteTextFile(FILE_NAME_REMOVE_TRACKERS, ['udp://orig1.test/announce']);

  //An exclusive lock makes the file unreadable for the other process
  LockedFile := TFileStream.Create(FWorkFolder + FILE_NAME_REMOVE_TRACKERS,
    fmOpenRead or fmShareExclusive);
  try
    RunCli([TorrentFile, '-U4']);
  finally
    LockedFile.Free;
  end;

  CheckErrorLog('ERROR: Can not read ' + FILE_NAME_REMOVE_TRACKERS);
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce', 'udp://orig2.test/announce']);
end;

procedure TTestStartUpParameterCli.
Test_Folder_With_Corrupt_Torrent_Fails_And_Changes_Nothing;
var
  Folder, GoodFile: string;
  Corrupt: TStringList;
begin
  PrepareWorkFolder;
  Folder := FWorkFolder + 'torrents';
  ForceDirectories(Folder);
  GoodFile := Folder + PathDelim + 'good.torrent';
  CreateTorrentFile(GoodFile, ['udp://orig1.test/announce']);
  Corrupt := TStringList.Create;
  try
    Corrupt.Add('this is not bencode');
    Corrupt.SaveToFile(Folder + PathDelim + 'corrupt.torrent');
  finally
    Corrupt.Free;
  end;
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([Folder, '-U4']);

  CheckEquals(1, FExitCode, 'Exit code');
  Check(Pos('Can not read torrent', FLog.Text) > 0, 'The corrupt torrent must be reported');
  Check(Pos('Can not load torrent via folder', FLog.Text) > 0,
    'The folder must be reported');
  //The good torrent is not updated either: it is all or nothing
  CheckTrackersInFile(GoodFile, ['udp://orig1.test/announce']);
end;

procedure TTestStartUpParameterCli.Test_Folder_Without_Torrents_Fails;
var
  Folder: string;
begin
  PrepareWorkFolder;
  Folder := FWorkFolder + 'empty';
  ForceDirectories(Folder);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([Folder, '-U4']);
  CheckErrorLog('ERROR: No torrent file selected');

  //A folder that has only other files
  WriteTextFile('empty' + PathDelim + 'readme.txt', ['not a torrent']);
  RunCli([Folder, '-U4']);
  CheckErrorLog('ERROR: No torrent file selected');
end;

procedure TTestStartUpParameterCli.Test_Folder_Name_With_Dot_Is_Accepted;
var
  Folder, TorrentFile: string;
begin
  PrepareWorkFolder;
  Folder := FWorkFolder + 'My.Torrents';
  ForceDirectories(Folder);
  TorrentFile := Folder + PathDelim + 'a.torrent';
  CreateTorrentFile(TorrentFile, ['udp://orig1.test/announce']);
  WriteTextFile(FILE_NAME_ADD_TRACKERS, ['udp://new.test/announce']);

  RunCli([Folder, '-U3']);

  CheckSuccessLog(2);
  CheckTrackersInFile(TorrentFile, ['udp://orig1.test/announce', 'udp://new.test/announce']);
end;

procedure TTestStartUpParameterCli.Test_Parameter_TEST_SSL;
begin
  //'-TEST_SSL' is not a recognized CLI parameter, so it's decoded as a non-existent
  //torrent path/folder and must fail like any other invalid single argument.
  FCommandLine := '-TEST_SSL';
  CallExecutableFile;
  CheckEquals(1, FExitCode);
end;

procedure TTestStartUpParameterCli.Test_Parameter_Single_Path_Only;
var
  StartupParameter: TStartupParameter;
begin
  //Bare 1-parameter form (just a path, no -Ux) must default to sort order.
  StartupParameter.TrackerListOrder := tloSort;
  StartupParameter.SkipAnnounceCheck := False;
  StartupParameter.SourcePresent := False;
  CreateFilledTorrent(StartupParameter);

  DownloadPreTestTrackerList;
  LoadTrackerListAddAndRemoved;

  //No -Ux at all - just the torrent path.
  FCommandLine := QuoteParameter(FFullPathToTorrent);
  CallExecutableFile;

  CopyTrackerEndResultToVerifyTrackerResult;

  FVerifyTrackerResult.StartupParameter := StartupParameter;
  Check(VerifyTrackerResult(FVerifyTrackerResult), FVerifyTrackerResult.ErrorString);

  CheckEquals(0, FExitCode);

  Check(ReadConsoleLogFile, 'Log data is not present');
  Check(FConsoleLogData.StatusOK);
  Check(FConsoleLogData.TrackersCount > 0);
  Check(FConsoleLogData.TorrentFilesCount = TEST_TORRENT_FILES_COUNT);
end;

procedure TTestStartUpParameterCli.Test_Exception_Is_Written_To_Console_Log;
var
  Folder, ExeCopy, TorrentCopy, LogFileName: string;
  Log: TStringList;
begin
  //Everything happens in a temp folder, a copy of the program writes its files next to itself.
  Folder := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cli_exception' + PathDelim;
  ForceDirectories(Folder);
  ExeCopy := Folder + ExtractFileName(FFullPathToBinary);
  TorrentCopy := Folder + 'a.torrent';
  LogFileName := Folder + FILE_NAME_CONSOLE_LOG;
  Log := TStringList.Create;
  try
    Check(CopyFile(FFullPathToBinary, ExeCopy), 'Can not copy the program');
    {$IFDEF UNIX}
    //CopyFile does not keep the execute permission.
    Check(FpChmod(ExeCopy, &755) = 0, 'Can not make the program executable');
    {$ENDIF}
    Check(CopyFile(FFullPathToTorrent + 'bittorrent-v2-test.torrent', TorrentCopy),
      'Can not copy the torrent');

    //A folder with this name makes writing the export file fail with an exception.
    ForceDirectories(Folder + FILE_NAME_EXPORT_TRACKERS);

    FExitCode := SysUtils.ExecuteProcess(UTF8ToSys(ExeCopy),
      QuoteParameter(TorrentCopy) + ' -U4', []);

    CheckEquals(1, FExitCode, 'An exception must give an error exit code');
    Log.LoadFromFile(LogFileName);
    Check(Log.Count > 0, 'The console log must not be empty');
    Check(Pos('ERROR: ', Log[0]) = 1, 'The exception must be the first line of the log');
  finally
    Log.Free;
    DeleteFile(LogFileName);
    DeleteFile(TorrentCopy);
    DeleteFile(ExeCopy);
    RemoveDir(Folder + FILE_NAME_EXPORT_TRACKERS);
    RemoveDir(Folder);
  end;
end;
  {$ENDIF DARWIN}

initialization
  RegisterTest(TTestStartUpParameter);
  {$IFNDEF DARWIN}
  RegisterTest(TTestStartUpParameterCli);
  {$ENDIF DARWIN}

end.
