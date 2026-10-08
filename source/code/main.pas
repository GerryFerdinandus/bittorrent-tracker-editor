// SPDX-License-Identifier: MIT
unit main;

{
Unicode:
variable 'Utf8string' is the same as 'string'
UTF8String          = type ansistring;
All 'string' should be rename to 'UTF8String' to show the intention that we should
use UTF8 in the program.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, DecodeTorrent, LCLType, ActnList, Menus, ComCtrls,
  Grids, controllergridtorrentdata, torrent_miscellaneous, update_torrent,
  controller_trackerlist_online, controller_treeview_torrent_data, ngosang_trackerslist,
  main_common;

type


  { TFormTrackerModify }

  TFormTrackerModify = class(TForm)
    CheckBoxRemoveAllSourceTag: TCheckBox;
    CheckBoxSkipAnnounceCheck: TCheckBox;
    CheckListBoxPublicPrivateTorrent: TCheckListBox;
    GroupBoxInfoSource: TGroupBox;
    GroupBoxItemsForPrivateTrackers: TGroupBox;
    GroupBoxPublicPrivateTorrent: TGroupBox;
    GroupBoxNewTracker: TGroupBox;
    GroupBoxPresentTracker: TGroupBox;
    LabeledEditInfoSource: TLabeledEdit;
    MainMenu: TMainMenu;
    MemoNewTrackers: TMemo;
    MenuFile: TMenuItem;
    MenuFileTorrentFolder: TMenuItem;
    MenuFileOpenTrackerList: TMenuItem;
    MenuHelpReportingIssue: TMenuItem;
    MenuHelpSeparator1: TMenuItem;
    MenuHelpVisitNewTrackon: TMenuItem;
    MenuItem1: TMenuItem;
    MenuHelpVisitNgosang: TMenuItem;
    MenuItemNgosangAppendAllIp: TMenuItem;
    MenuItemNgosangAppendAllBestIp: TMenuItem;
    MenuItemNgosangAppendAllWs: TMenuItem;
    MenuItemNgosangAppendAllHttps: TMenuItem;
    MenuItemNgosangAppendAllHttp: TMenuItem;
    MenuItemNgosangAppendAllUdp: TMenuItem;
    MenuItemNgosangAppendBest: TMenuItem;
    MenuItemNgosangAppendAll: TMenuItem;
    MenuItemOnlineCheckSubmitNewTrackon: TMenuItem;
    MenuItemOnlineCheckAppendStableTrackers: TMenuItem;
    MenuTrackersDeleteDeadTrackers: TMenuItem;
    MenuTrackersDeleteUnstableTrackers: TMenuItem;
    MenuTrackersDeleteUnknownTrackers: TMenuItem;
    MenuTrackersSeparator2: TMenuItem;
    MenuTrackersSeparator1: TMenuItem;
    MenuItemOnlineCheckDownloadNewTrackon: TMenuItem;
    MenuOnlineCheck: TMenuItem;
    MenuUpdateRandomize: TMenuItem;
    MenuUpdateTorrentAddBeforeKeepOriginalIntactAndRemoveNothing: TMenuItem;
    MenuUpdateTorrentAddAfterKeepOriginalIntactAndRemoveNothing: TMenuItem;
    MenuUpdateTorrentAddBeforeRemoveOriginal: TMenuItem;
    MenuUpdateTorrentAddAfterRemoveOriginal: TMenuItem;
    MenuUpdateTorrentAddBeforeRemoveNew: TMenuItem;
    MenuUpdateTorrentAddAfterRemoveNew: TMenuItem;
    MenuUpdateTorrentSort: TMenuItem;
    MenuUpdateTorrentAddAfter: TMenuItem;
    MenuUpdateTorrentAddBefore: TMenuItem;
    MenuTrackersAllTorrentArePrivate: TMenuItem;
    MenuTrackersAllTorrentArePublic: TMenuItem;
    MenuUpdateTorrent: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpVisitWebsite: TMenuItem;
    MenuTrackersDeleteAllTrackers: TMenuItem;
    MenuTrackersKeepAllTrackers: TMenuItem;
    MenuTrackers: TMenuItem;
    MenuOpenTorrentFile: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PanelTopPublicTorrent: TPanel;
    PanelTop: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StringGridTrackerOnline: TStringGrid;
    StringGridTorrentData: TStringGrid;
    TabSheetPrivateTrackers: TTabSheet;
    TabSheetTorrentsContents: TTabSheet;
    TabSheetTorrentData: TTabSheet;
    TabSheetTrackersList: TTabSheet;
    TabSheetPublicPrivateTorrent: TTabSheet;
    procedure CheckBoxRemoveAllSourceTagChange(Sender: TObject);
    procedure CheckBoxSkipAnnounceCheckChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //Drag and drop '*.torrent' files/directory or 'tracker.txt'
    procedure FormDropFiles(Sender: TObject; const FileNames: array of utf8string);

    //At start of the program the form will be show/hide
    procedure FormShow(Sender: TObject);
    procedure LabeledEditInfoSourceEditingDone(Sender: TObject);
    procedure MenuHelpReportingIssueClick(Sender: TObject);
    procedure MenuHelpVisitNewTrackonClick(Sender: TObject);
    procedure MenuHelpVisitWebsiteClick(Sender: TObject);
    procedure MenuHelpVisitNgosangClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllBestIpClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllHttpClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllHttpsClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllIpClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllUdpClick(Sender: TObject);
    procedure MenuItemNgosangAppendAllWsClick(Sender: TObject);

    procedure MenuItemNgosangAppendBestClick(Sender: TObject);
    procedure MenuItemOnlineCheckSubmitNewTrackonClick(Sender: TObject);

    //Select via menu torrent file or directory
    procedure MenuOpenTorrentFileClick(Sender: TObject);
    procedure MenuFileTorrentFolderClick(Sender: TObject);
    procedure MenuFileOpenTrackerListClick(Sender: TObject);

    //Menu trackers
    procedure MenuTrackersAllTorrentArePublicPrivateClick(Sender: TObject);
    procedure MenuTrackersKeepOrDeleteAllTrackersClick(Sender: TObject);
    procedure MenuTrackersDeleteTrackersWithStatusClick(Sender: TObject);

    //Menu update torrent
    procedure MenuUpdateTorrentAddAfterRemoveNewClick(Sender: TObject);
    procedure MenuUpdateTorrentAddAfterRemoveOriginalClick(Sender: TObject);
    procedure MenuUpdateTorrentAddBeforeKeepOriginalIntactAndRemoveNothingClick(
      Sender: TObject);
    procedure MenuUpdateTorrentAddBeforeRemoveNewClick(Sender: TObject);
    procedure MenuUpdateTorrentAddBeforeRemoveOriginalClick(Sender: TObject);
    procedure MenuUpdateTorrentSortClick(Sender: TObject);
    procedure MenuUpdateTorrentAddAfterKeepOriginalIntactAndRemoveNothingClick(
      Sender: TObject);
    procedure MenuUpdateRandomizeClick(Sender: TObject);

    //Menu online check
    procedure MenuItemOnlineCheckAppendStableTrackersClick(Sender: TObject);
    procedure MenuItemOnlineCheckDownloadNewTrackonClick(Sender: TObject);
  private
    { private declarations }

    FTrackerList: TTrackerList;
    FControllerTrackerListOnline: TControllerTrackerListOnline;
    FControllerTreeviewTorrentData: TControllerTreeviewTorrentData;
    FDownloadStatus: boolean;

    FngosangTrackerList: TngosangTrackerList;
    // is the present torrent file being process
    FDecodePresentTorrent: TDecodeTorrent;

    FDragAndDropStartUp, // user have start the program via Drag And Drop
    FConsoleMode, //user have start the program in console mode
    FFilePresentBanByUserList//There is a file 'remove_trackers.txt' detected
    : boolean;

    FFolderForTrackerListLoadAndSave: string;
    FControllerGridTorrentData: TControllerGridTorrentData;
    function CheckForAnnounce(const TrackerURL: utf8string): boolean;
    procedure AppendTrackersToMemoNewTrackers(TrackerList: TStringList);
    procedure AppendNgosangTrackersToMemoNewTrackers(TrackerList: TStringList);
    procedure ShowUserErrorMessage(ErrorText: string; const FormText: string = '');
    function TrackerWithURLAndAnnounce(const TrackerURL: utf8string): boolean;
    procedure UpdateTorrent;
    function ReadTorrentFileSettingList: TTorrentFileSettingArray;
    procedure ShowHourGlassCursor(HourGlass: boolean);
    procedure ViewUpdateBegin;
    procedure ViewUpdateOneTorrentFileDecoded;
    procedure ViewUpdateEnd;
    procedure ViewUpdateFormCaption;
    procedure ClearAllTorrentFilesNameAndTrackerInside;
    procedure ClearTorrentFilesView;
    procedure DragAndDropStartupMode;
    procedure UpdateViewRemoveTracker;
    function ReloadAllTorrentAndRefreshView: boolean;
    function AddTorrentFileList(TorrentFileNameStringList: TStringList): boolean;
    function LoadTorrentViaDir(const Dir: utf8string): boolean;
    function DecodeTorrentFile(const FileName: utf8string): boolean;
    procedure UpdateTrackerInsideFileList;
    procedure UpdateTorrentTrackerList;
    procedure ShowTrackerInsideFileList;

    procedure CheckedOnOffAllTrackers(Value: boolean);
    function CopyUserInputNewTrackersToList(Temporary_SkipAnnounceCheck: boolean =
      False): boolean;
    procedure LoadTrackersTextFileAddTrackers(Temporary_SkipAnnounceCheck: boolean);
  public
    { public declarations }
  end;

var
  FormTrackerModify: TFormTrackerModify;

implementation

uses LCLIntf, lazutf8, LazFileUtils, trackerlist_online, LCLVersion, fphttpclient;

const
  //program name and version (http://semver.org/)
  PROGRAM_VERSION = '1.33.1';

  FORM_CAPTION = 'Bittorrent tracker editor (' + PROGRAM_VERSION +
    '/LCL ' + lcl_version + '/FPC ' + {$I %FPCVERSION%} + ')';

  GROUPBOX_PRESENT_TRACKERS_CAPTION =
    'Present trackers in all torrent files. Select the one that you want to keep. And added to all torrent files.';

{$R *.lfm}

{ TFormTrackerModify }

//True when this run is the '-TEST_SSL' diagnostic (single parameter). Sets ExitCode on failure.
//GUI-only: the console CLI has no networking code and does not support this parameter.
function CheckTestSSLParameter: boolean;
begin
  Result := ParamCount = 1;
  if Result then
  begin
    // Check for the correct parameter.
    Result := UTF8Trim(ParamStr(1)) = '-TEST_SSL';
    if Result then
    begin
      // Check if there is SSL connection
      try
        TFPCustomHTTPClient.SimpleGet(
          'https://raw.githubusercontent.com/gerryferdinandus/bittorrent-tracker-editor/master/README.md');
      except
        //No SSL or no internet connection.
        System.ExitCode := 1;
      end;
    end;
  end;
end;

procedure TFormTrackerModify.FormCreate(Sender: TObject);
begin
  //Test the working for SSL connection
  if CheckTestSSLParameter then
  begin
    // shutdown the GUI program
    Application.terminate;
    Exit;
  end;

  FFolderForTrackerListLoadAndSave :=
    main_common.DetermineTrackerListFolder(Application.ExeName);

  //Two or more parameters is console mode. It runs fully headless, the form is never used.
  FConsoleMode := ParamCount >= 2;
  if FConsoleMode then
  begin
    if not main_common.RunConsoleMode(FFolderForTrackerListLoadAndSave) then
      System.ExitCode := 1;
    Application.terminate;
    Exit;
  end;

  //Create controller for StringGridTorrentData
  FControllerGridTorrentData := TControllerGridTorrentData.Create(StringGridTorrentData);

  //All the lists are created the same way as in console mode.
  main_common.CreateTrackerList(FTrackerList);

  //Decoding class for torrent.
  FDecodePresentTorrent := TDecodeTorrent.Create;

  //Create view for trackerURL with CheckBoxRemoveAllSourceTag
  FControllerTrackerListOnline :=
    TControllerTrackerListOnline.Create(StringGridTrackerOnline,
    FTrackerList.TrackerFromInsideTorrentFilesList, @TrackerWithURLAndAnnounce);

  //Create view for treeview data of all the torrent files
  FControllerTreeviewTorrentData :=
    TControllerTreeviewTorrentData.Create(TabSheetTorrentsContents);

  //start the program at minimum visual size. (this is optional)
  Width := Constraints.MinWidth;
  Height := Constraints.MinHeight;

  //One parameter is the drag and drop via shortcut in windows mode.
  FDragAndDropStartUp := ParamCount = 1;

  //Show the default trackers
  LoadTrackersTextFileAddTrackers(True);

  //Load the unwanted trackers list.
  if not main_common.LoadRemoveTrackers(FFolderForTrackerListLoadAndSave, FTrackerList,
    FFilePresentBanByUserList) then
    ShowUserErrorMessage('Can not read the file, no trackers will be removed via this file.',
      FFolderForTrackerListLoadAndSave + FILE_NAME_REMOVE_TRACKERS);

  //Create download for ngosang tracker list
  FngosangTrackerList := TngosangTrackerList.Create;

  //Start program in windows mode via shortcut with drag/drop
  if FDragAndDropStartUp then
  begin
    DragAndDropStartupMode;
  end;

  //There should be no more exception made for the drag and drop
  FDragAndDropStartUp := False;

  //Update some captions
  ViewUpdateFormCaption;
  GroupBoxPresentTracker.Caption := GROUPBOX_PRESENT_TRACKERS_CAPTION;
end;

procedure TFormTrackerModify.CheckBoxSkipAnnounceCheckChange(Sender: TObject);
var
  i: integer;
begin
  FTrackerList.SkipAnnounceCheck := CheckBoxSkipAnnounceCheck.Checked;

  //The trackers without /announce are unchecked by default, until -SAC is ticked.
  //Only those rows follow the new setting, the other choices of the user stay as they are.
  for i := 0 to FControllerTrackerListOnline.Count - 1 do
  begin
    if TrackerDependsOnSkipAnnounceCheck(FControllerTrackerListOnline.TrackerURL(i)) then
      FControllerTrackerListOnline.Checked[i] := FTrackerList.SkipAnnounceCheck;
  end;

  ViewUpdateFormCaption;
end;

procedure TFormTrackerModify.CheckBoxRemoveAllSourceTagChange(Sender: TObject);
begin
  FTrackerList.RemoveAllSourceTag := TCheckBox(Sender).Checked;
  LabeledEditInfoSource.Enabled := not FTrackerList.RemoveAllSourceTag;
end;

procedure TFormTrackerModify.FormDestroy(Sender: TObject);
begin
  //The program is being closed. Free all the memory.
  FngosangTrackerList.Free;
  main_common.FreeTrackerList(FTrackerList);
  FDecodePresentTorrent.Free;
  FControllerGridTorrentData.Free;
  FControllerTrackerListOnline.Free;
  FControllerTreeviewTorrentData.Free;
end;

procedure TFormTrackerModify.MenuFileTorrentFolderClick(Sender: TObject);
var
  NoTorrentFound: boolean;
begin
  NoTorrentFound := False;
  //User what to select one torrent file. Show the user dialog file selection.
  SelectDirectoryDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  //Cancel must keep the torrent files that are already loaded.
  if SelectDirectoryDialog1.Execute then
  begin
    ClearAllTorrentFilesNameAndTrackerInside;
    ViewUpdateBegin;
    ShowHourGlassCursor(True);
    //A failed decode is already reported by AddTorrentFileList.
    NoTorrentFound := LoadTorrentViaDir(SelectDirectoryDialog1.FileName) and
      (FTrackerList.TorrentFileNameList.Count = 0);
    ShowHourGlassCursor(False);
    ViewUpdateEnd;
  end;

  if NoTorrentFound then
    ShowUserErrorMessage('No torrent files found in this folder.',
      SelectDirectoryDialog1.FileName);
end;

procedure TFormTrackerModify.MenuHelpVisitWebsiteClick(Sender: TObject);
begin
  //There is no help file in this program. Show user main web site.
  OpenURL('https://github.com/GerryFerdinandus/bittorrent-tracker-editor');
end;

procedure TFormTrackerModify.MenuHelpVisitNgosangClick(Sender: TObject);
begin
  //newTrackon trackers is being used in this program.
  OpenURL('https://github.com/ngosang/trackerslist');
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllBestIpClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_Best_IP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllHttpClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_HTTP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllHttpsClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_HTTPS);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllIpClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_IP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllUdpClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_UDP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllWsClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_WS);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendBestClick(Sender: TObject);
begin
  AppendNgosangTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_Best);
end;

procedure TFormTrackerModify.MenuItemOnlineCheckSubmitNewTrackonClick(Sender: TObject);
var
  SendStatus: boolean;
  TrackerSendCount, PrivateCount: integer;
  PopupStr: string;
  SubmitList: TStringList;
begin
  SubmitList := TStringList.Create;
  try
    //The URL of a private torrent can have a passkey. It must not be sent to the internet.
    GetTrackersForOnlineSubmit(FTrackerList, SubmitList);
    PrivateCount := FTrackerList.TrackerFromInsideTorrentFilesList.Count -
      SubmitList.Count;

    try
      screen.Cursor := crHourGlass;
      SendStatus := FControllerTrackerListOnline.SubmitTrackers(SubmitList,
        TrackerSendCount);
    finally
      screen.Cursor := crDefault;
    end;
  finally
    SubmitList.Free;
  end;

  if SendStatus then
  begin
    //Successful upload
    PopupStr := format('Successful upload of %d unique tracker URL', [TrackerSendCount]);
    if PrivateCount > 0 then
      PopupStr := PopupStr + sLineBreak + format(
        '%d tracker URL from private torrents are not sent', [PrivateCount]);
    Application.MessageBox(
      PChar(@PopupStr[1]),
      '', MB_ICONINFORMATION + MB_OK);
  end
  else
  begin
    //something is wrong with uploading
    ShowUserErrorMessage('Can not uploading the tracker list');
  end;
end;

procedure TFormTrackerModify.AppendTrackersToMemoNewTrackers(TrackerList: TStringList);
var
  tracker: utf8string;
  PreviousText: string;
begin
  PreviousText := MemoNewTrackers.Text;

  //Append all the trackers to MemoNewTrackers
  MemoNewTrackers.Lines.BeginUpdate;
  for Tracker in TrackerList do
  begin
    MemoNewTrackers.Lines.Add(tracker);
  end;
  MemoNewTrackers.Lines.EndUpdate;

  //Check for error in tracker list. Keep what the user already had.
  if not CopyUserInputNewTrackersToList then
  begin
    MemoNewTrackers.Text := PreviousText;
  end;
end;

procedure TFormTrackerModify.AppendNgosangTrackersToMemoNewTrackers(
  TrackerList: TStringList);
begin
  //TrackerList is the result of the download, so LastDownloadFailed is already set.
  if FngosangTrackerList.LastDownloadFailed then
    ShowUserErrorMessage('Can not downloading the trackers from internet')
  else
    AppendTrackersToMemoNewTrackers(TrackerList);
end;

procedure TFormTrackerModify.MenuItemOnlineCheckAppendStableTrackersClick(
  Sender: TObject);
begin
  //User want to use the downloaded tracker list.

  //check if tracker is already downloaded
  if not FDownloadStatus then
  begin
    //Download it now.
    MenuItemOnlineCheckDownloadNewTrackonClick(nil);
  end;

  //Append all the trackers to MemoNewTrackers
  AppendTrackersToMemoNewTrackers(FControllerTrackerListOnline.StableTrackers);
end;

procedure TFormTrackerModify.MenuItemOnlineCheckDownloadNewTrackonClick(
  Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    FDownloadStatus := FControllerTrackerListOnline.DownloadTrackers_All_Live_Stable;
  finally
    screen.Cursor := crDefault;
  end;

  if not FDownloadStatus then
  begin
    //something is wrong with downloading
    ShowUserErrorMessage('Can not downloading the trackers from internet');
  end;
end;

function TFormTrackerModify.CheckForAnnounce(const TrackerURL: utf8string): boolean;
begin
  Result := (not FTrackerList.SkipAnnounceCheck) and
    (not WebTorrentTrackerURL(TrackerURL)) and (not FDragAndDropStartUp);
end;

procedure TFormTrackerModify.ShowUserErrorMessage(ErrorText: string;
  const FormText: string);
begin
  if FormText <> '' then
    ErrorText := FormText + sLineBreak + ErrorText;
  Application.MessageBox(PChar(@ErrorText[1]), '', MB_ICONERROR);
end;

function TFormTrackerModify.TrackerWithURLAndAnnounce(
  const TrackerURL: utf8string): boolean;
begin
  //Validate the begin of the URL
  Result := ValidTrackerURL(TrackerURL);
  if Result then
  begin
    if CheckForAnnounce(TrackerURL) then
    begin
      Result := TrackerURLWithAnnounce(TrackerURL);
    end;
  end;
end;

procedure TFormTrackerModify.MenuTrackersDeleteTrackersWithStatusClick(
  Sender: TObject);

  procedure UncheckTrackers(Value: TTrackerListOnlineStatus);
  var
    i: integer;
  begin
    if FControllerTrackerListOnline.Count > 0 then
    begin
      for i := 0 to FControllerTrackerListOnline.Count - 1 do
      begin
        if FControllerTrackerListOnline.TrackerStatus(i) = Value then
        begin
          FControllerTrackerListOnline.Checked[i] := False;
        end;
      end;
    end;
  end;

begin
  //check if tracker is already downloaded
  if not FDownloadStatus then
  begin
    MenuItemOnlineCheckDownloadNewTrackonClick(nil);

    //The error is already shown. Without the lists every tracker has the status
    //'unknown', so nothing may be unchecked.
    if not FDownloadStatus then
      Exit;
  end;

  //0 = Unstable
  //1 = Dead
  //2 = Unknown
  case TMenuItem(Sender).Tag of
    0: UncheckTrackers(tos_live_but_unstable);
    1: UncheckTrackers(tos_dead);
    2: UncheckTrackers(tos_unknown);
    else
      Assert(False, 'Unknown Menu item selection')
  end;
end;

procedure TFormTrackerModify.MenuUpdateRandomizeClick(Sender: TObject);
begin
  //User can select to randomize the tracker list
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloRandomize;
  UpdateTorrent;
end;

procedure TFormTrackerModify.UpdateTorrent;
var
  Reply, BoxStyle, CountTrackers: integer;
  PopUpMenuStr: string;
  AllFilesAreReadBackCorrectly, ExportFileIsWritten: boolean;
  UpdateResult: TUpdateTorrentResult;
begin
  //Update all the torrent files.

  //The StringGridTorrentData where the comment are place by user
  //    must be in sync again with FTrackerList.TorrentFileNameList.
  //Undo all possible sort column used by the user. Sort it back to 'begin state'
  FControllerGridTorrentData.ReorderGrid;

  //EditingDone may not have fired yet when a menu item is clicked while the edit has focus.
  FTrackerList.SourceTag := LabeledEditInfoSource.Text;

  //initial value is false, will be set to true if some file fails to write
  UpdateResult.SomeFilesCannotBeWritten := False;
  UpdateResult.SomeFilesAreReadOnly := False;
  UpdateResult.SomeFilesCanNotBeDecoded := False;
  ExportFileIsWritten := True;

  try

    //Warn user before updating the torrent
    BoxStyle := MB_ICONWARNING + MB_OKCANCEL;
    Reply := Application.MessageBox('Torrent files will be change!' +
      sLineBreak + 'Warning: There is no undo.', '', BoxStyle);
    if Reply <> idOk then
    begin
      //finally block already resets the cursor
      exit;
    end;

    //Must have some torrent selected
    if (FTrackerList.TorrentFileNameList.Count = 0) then
    begin
      ShowUserErrorMessage('ERROR: No torrent file selected');
      //finally block already resets the cursor
      exit;
    end;

    //User must wait for a while.
    ShowHourGlassCursor(True);

    //Copy the tracker list inside torrent -> FTrackerList.TrackerFromInsideTorrentFilesList
    UpdateTrackerInsideFileList;

    //Check for error in user tracker list -> FTrackerList.TrackerAddedByUserList
    if not CopyUserInputNewTrackersToList then
      Exit;

    //There are 5 list that must be combine.
    //Must use 'sort' for correct FTrackerFinalList.Count
    CombineFiveTrackerListToOne(tloSort, FTrackerList,
      FDecodePresentTorrent.TrackerList);

    //How many trackers must be put inside each torrent file.
    CountTrackers := FTrackerList.TrackerFinalList.Count;

    if CountTrackers = 0 then
    begin //Torrent without a tracker is possible. But is this what the user really want? a DHT torrent.
      BoxStyle := MB_ICONWARNING + MB_OKCANCEL;
      Reply := Application.MessageBox('There are no Trackers selected!' +
        sLineBreak + 'Warning: Create torrent file without any URL of the tracker?',
        '', BoxStyle);
      if Reply <> idOk then
      begin
        //finally block already resets the cursor
        exit;
      end;
      //The message box reset the cursor.
      ShowHourGlassCursor(True);
    end;

    //Write the new tracker list and the user settings into all the torrent files.
    UpdateResult := UpdateTorrentFileList(FTrackerList, FDecodePresentTorrent,
      ReadTorrentFileSettingList);
    CountTrackers := UpdateResult.TrackerCount;

    //Create tracker.txt file. The torrent files are already written: a failure here must not
    //stop the reload of the view and the summary.
    ExportFileIsWritten := main_common.TrySaveTrackerFinalListToFile(
      FFolderForTrackerListLoadAndSave, FTrackerList.TrackerFinalList);

    //Show/reload the just updated torrent files.
    AllFilesAreReadBackCorrectly := ReloadAllTorrentAndRefreshView;

    //make sure cursor is default again
  finally
    ShowHourGlassCursor(False);
    ViewUpdateFormCaption;
  end;

  case FTrackerList.TrackerListOrderForUpdatedTorrent of
    tloInsertNewBeforeAndKeepNewIntact,
    tloInsertNewBeforeAndKeepOriginalIntact,
    tloAppendNewAfterAndKeepNewIntact,
    tloAppendNewAfterAndKeepOriginalIntact,
    tloSort,
    tloRandomize:
    begin
      //Via popup show user how many trackers are inside the torrent after update.
      PopUpMenuStr := 'All torrent file(s) have now ' + IntToStr(CountTrackers) +
        ' trackers.';
    end;

    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing,
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing:
    begin
      //Via popup show user that all the torrent files are updated.
      PopUpMenuStr := 'All torrent file(s) are updated.';
    end;
    else
    begin
      Assert(False, 'case else: Should never been called. UpdateTorrent');
    end;

  end;//case


  //Check if there are some error that need to be notify to the end user.

  if not AllFilesAreReadBackCorrectly then
  begin
    //add warning if torrent files can not be read back again
    PopUpMenuStr := PopUpMenuStr +
      ' WARNING: Some torrent files can not be read back again after updating.';
  end;

  if UpdateResult.SomeFilesAreReadOnly then
  begin
    //add warning if read only files are detected.
    PopUpMenuStr := PopUpMenuStr +
      ' WARNING: Some torrent files are not updated because they are READ-ONLY files.';
  end;

  if UpdateResult.SomeFilesCannotBeWritten then
  begin
    //add warning if some files written are failed. Something is wrong with the disk.
    PopUpMenuStr := PopUpMenuStr +
      ' WARNING: Some torrent files are not updated because they failed at write.';
  end;

  if not ExportFileIsWritten then
  begin
    //The torrent files are updated, only the export file is missing.
    PopUpMenuStr := PopUpMenuStr + ' WARNING: Can not write the file ' +
      FILE_NAME_EXPORT_TRACKERS + ' in the folder ' + FFolderForTrackerListLoadAndSave;
  end;

  //Show the MessageBox
  Application.MessageBox(
    PChar(@PopUpMenuStr[1]),
    '', MB_ICONINFORMATION + MB_OK);

end;

function TFormTrackerModify.ReadTorrentFileSettingList: TTorrentFileSettingArray;
var
  i: integer;
begin
  //Collect the user settings of every torrent file from the view.
  //ReorderGrid must already be called to keep the grid in sync with the file list.
  SetLength(Result, FTrackerList.TorrentFileNameList.Count);
  for i := 0 to High(Result) do
  begin
    Result[i].PublicTorrent := CheckListBoxPublicPrivateTorrent.Checked[i];
    Result[i].Comment := FControllerGridTorrentData.ReadComment(i + 1);
  end;
end;


procedure TFormTrackerModify.DragAndDropStartupMode;
var
  FileNameOrDirStr: utf8string;
  StringList: TStringList;
  MustExitWithErrorCode: boolean;
begin
  //One parameter only. Program startup via DragAndDrop
  //    The first parameter[1] is path to file or dir. The window stays visible.

  //Will be set to True when error occurs.
  MustExitWithErrorCode := False;
  ViewUpdateBegin;

  try
    //Get the startup command lime parameters.
    if ConsoleModeDecodeParameter(FileNameOrDirStr, FTrackerList) then
    begin
      //There is no error. Proceed with reading the torrent files

      if PathIsTorrentFolder(FileNameOrDirStr) then
      begin //A folder. Its name may contain a dot.
        if LoadTorrentViaDir(FileNameOrDirStr) then
        begin
          //Show all the tracker inside the torrent files.
          ShowTrackerInsideFileList;
          //Some tracker must be removed. Console and windows mode.
          UpdateViewRemoveTracker;
        end
        else
        begin
          //failed to load the torrent via folders
          ShowUserErrorMessage('Can not load torrent via folder');
        end;

      end
      else //a single torrent file is selected?
      begin
        if PathIsTorrentFile(FileNameOrDirStr) then
        begin
          StringList := TStringList.Create;
          try
            //Convert Filenames to stringlist format.
            StringList.Add(FileNameOrDirStr);

            //Extract all the trackers inside the torrent file
            if AddTorrentFileList(StringList) then
            begin
              //Show all the tracker inside the torrent files.
              ShowTrackerInsideFileList;
              //Some tracker must be removed. Console and windows mode.
              UpdateViewRemoveTracker;
            end
            else
            begin
              //failed to load one torrent
              ShowUserErrorMessage('Can not load torrent file.');
            end;

          finally
            StringList.Free;
          end;
        end
        else
        begin //Error. this is not a torrent file
          ShowUserErrorMessage('ERROR: No torrent file selected.');
        end;
      end;
    end;

  except
    //Shutdown the console program.
    //This is needed or else the program will keep running forever.
    //exit with error code
    MustExitWithErrorCode := True;
  end;

  ViewUpdateEnd;

  if MustExitWithErrorCode then
  begin
    //exit with error code
    System.ExitCode := 1;
  end;
end;



procedure TFormTrackerModify.UpdateViewRemoveTracker;
var
  TrackerStr: utf8string;
  i: integer;
begin
  {
    Called when user load the torrent files.
    Trackers that are forbidden must be uncheck.
    Trackers add by user in the memo text filed must also be removed.
    This routine is also use in the console mode to remove trackers
  }

  //'Remove nothing' modes (-U5, -U6) must never remove or uncheck any tracker.
  if (FTrackerList.TrackerListOrderForUpdatedTorrent =
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing) or
    (FTrackerList.TrackerListOrderForUpdatedTorrent =
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing) then
  begin
    exit;
  end;

  //If file remove_trackers.txt is present but empty then remove all tracker inside torrent.
  if FFilePresentBanByUserList and
    (UTF8Trim(FTrackerList.TrackerBanByUserList.Text) = '') then
  begin
    CheckedOnOffAllTrackers(False);
  end;


  //reload the memo. This will sanitize the MemoNewTrackers.Lines.
  if not CopyUserInputNewTrackersToList then
    exit;

  //remove all the trackers that are ban.
  MemoNewTrackers.Lines.BeginUpdate;
  for TrackerStr in FTrackerList.TrackerBanByUserList do
  begin

    //uncheck tracker that are listed in FTrackerList.TrackerBanByUserList
    //the FTrackerList.TrackerFromInsideTorrentFilesList is use in the view
    i := FTrackerList.TrackerFromInsideTorrentFilesList.IndexOf(UTF8Trim(TrackerStr));
    if i >= 0 then //Found it.
    begin
      FControllerTrackerListOnline.Checked[i] := False;
    end;

    //remove tracker from user memo text that are listed in FTrackerList.TrackerBanByUserList
    //Find TrackerStr in MemoNewTrackers.Lines and remove it.
    i := MemoNewTrackers.Lines.IndexOf(UTF8Trim(TrackerStr));
    if i >= 0 then //Found it.
    begin
      MemoNewTrackers.Lines.Delete(i);
    end;
  end;
  MemoNewTrackers.Lines.EndUpdate;

  //reload the memo again.
  CopyUserInputNewTrackersToList;

end;




function TFormTrackerModify.DecodeTorrentFile(const FileName: utf8string): boolean;
begin
  //Called when user add torrent files
  //False if something is wrong with decoding torrent.
  Result := FDecodePresentTorrent.DecodeTorrent(FileName);
  if Result then
  begin
    //visual update this one torrent file.
    ViewUpdateOneTorrentFileDecoded;
  end;
end;

procedure TFormTrackerModify.UpdateTorrentTrackerList;
begin
  //Copy the trackers found in one torrent file to FTrackerList.TrackerFromInsideTorrentFilesList
  //The ones of a private torrent are also remembered, they are never sent online.
  AddTorrentFileTrackers(FDecodePresentTorrent.TrackerList,
    FDecodePresentTorrent.PrivateTorrent, FTrackerList);
end;

procedure TFormTrackerModify.ShowTrackerInsideFileList;
begin
  //Called after torrent is being loaded.
  FControllerTrackerListOnline.UpdateView;
end;


procedure TFormTrackerModify.CheckedOnOffAllTrackers(Value: boolean);
var
  i: integer;
begin
  //Set all the trackers CheckBoxRemoveAllSourceTag ON or OFF
  if FControllerTrackerListOnline.Count > 0 then
  begin
    for i := 0 to FControllerTrackerListOnline.Count - 1 do
    begin
      FControllerTrackerListOnline.Checked[i] := Value;
    end;
  end;
end;


function TFormTrackerModify.CopyUserInputNewTrackersToList(
  Temporary_SkipAnnounceCheck: boolean): boolean;
var
  TrackerStr, ErrorStr: utf8string;
begin
  {
   Called after 'update torrent' is selected.
   All the user entery from Memo text field will be add to FTrackerList.TrackerAddedByUserList.
  }
  //The drag and drop start up must not fail on the announce check.
  Result := ValidateNewTrackerLines(MemoNewTrackers.Lines,
    FTrackerList.SkipAnnounceCheck or FDragAndDropStartUp or Temporary_SkipAnnounceCheck,
    FTrackerList.TrackerAddedByUserList, ErrorStr, TrackerStr);

  if Result then
  begin
    //Show the torrent list we have just created.
    MemoNewTrackers.Text := FTrackerList.TrackerAddedByUserList.Text;
  end
  else
  begin
    //There is error. Show the error.
    ShowUserErrorMessage(ErrorStr, TrackerStr);
  end;
end;

procedure TFormTrackerModify.UpdateTrackerInsideFileList;
var
  i: integer;
begin
  //Collect data what the user want to keep
  //Copy items from FControllerTrackerListOnline to FTrackerList.TrackerFromInsideTorrentFilesList
  //Copy items from FControllerTrackerListOnline to FTrackerList.TrackerManuallyDeselectedByUserList

  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  FTrackerList.TrackerManuallyDeselectedByUserList.Clear;

  if FControllerTrackerListOnline.Count > 0 then
  begin
    for i := 0 to FControllerTrackerListOnline.Count - 1 do
    begin

      if FControllerTrackerListOnline.Checked[i] then
      begin
        //Selected by user
        AddButIgnoreDuplicates(FTrackerList.TrackerFromInsideTorrentFilesList,
          FControllerTrackerListOnline.TrackerURL(i)
          );
      end
      else
      begin
        //Deselected by user
        AddButIgnoreDuplicates(
          FTrackerList.TrackerManuallyDeselectedByUserList,
          FControllerTrackerListOnline.TrackerURL(i)
          );
      end;

    end;
  end;

end;

procedure TFormTrackerModify.LoadTrackersTextFileAddTrackers(
  Temporary_SkipAnnounceCheck: boolean);
begin
  //Called at the start of the program. Load a trackers list from file,
  //or the default tracker list if no file is found.
  main_common.LoadAddTrackersRaw(FFolderForTrackerListLoadAndSave, MemoNewTrackers.Lines);

  //Check for error in tracker list
  if not CopyUserInputNewTrackersToList(Temporary_SkipAnnounceCheck) then
  begin
    MemoNewTrackers.Lines.Clear;
  end;
end;


procedure TFormTrackerModify.MenuOpenTorrentFileClick(Sender: TObject);
var
  StringList: TStringList;
begin
  //User what to select a torrent file. Show the user dialog.
  OpenDialog.Title := 'Select a torrent file';
  OpenDialog.Filter := 'torrent|*.torrent';
  //Cancel must keep the torrent files that are already loaded.
  if OpenDialog.Execute then
  begin
    ClearAllTorrentFilesNameAndTrackerInside;
    ViewUpdateBegin;
    ShowHourGlassCursor(True);
    StringList := TStringList.Create;
    try
      StringList.Add(UTF8Trim(OpenDialog.FileName));
      AddTorrentFileList(StringList);
    finally
      StringList.Free;
      ShowHourGlassCursor(False);
    end;
    ViewUpdateEnd;
  end;

end;

procedure TFormTrackerModify.MenuTrackersAllTorrentArePublicPrivateClick(
  Sender: TObject);
var
  i: integer;
begin
  //Warn user about torrent Hash.
  if Application.MessageBox('Are you sure!' + sLineBreak +
    'Warning: Changing the public/private torrent flag will change the info hash.',
    '', MB_ICONWARNING + MB_OKCANCEL) <> idOk then
    exit;

  //Set all the trackers public/private CheckBoxRemoveAllSourceTag ON or OFF
  if CheckListBoxPublicPrivateTorrent.Count > 0 then
  begin
    for i := 0 to CheckListBoxPublicPrivateTorrent.Count - 1 do
    begin
      CheckListBoxPublicPrivateTorrent.Checked[i] := TMenuItem(Sender).Tag = 1;
    end;
  end;
end;


procedure TFormTrackerModify.MenuFileOpenTrackerListClick(Sender: TObject);
var
  PreviousText: string;
begin
  //User what to select a tracker file. Show the user dialog.
  //Cancel or an unreadable file must keep the present list.
  OpenDialog.Title := 'Select a tracker list file';
  OpenDialog.Filter := 'tracker text file|*.txt';
  if OpenDialog.Execute then
  begin
    PreviousText := MemoNewTrackers.Text;
    if not main_common.ReadAddTrackersFile(OpenDialog.FileName, MemoNewTrackers.Lines) then
      ShowUserErrorMessage('Can not read the tracker list file', OpenDialog.FileName)
    else if not CopyUserInputNewTrackersToList then
      //The error is already shown. Keep the list the user had.
      MemoNewTrackers.Text := PreviousText;
  end;
end;

procedure TFormTrackerModify.MenuHelpReportingIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/GerryFerdinandus/bittorrent-tracker-editor/issues');
end;

procedure TFormTrackerModify.MenuHelpVisitNewTrackonClick(Sender: TObject);
begin
  //newTrackon trackers is being used in this program.
  //User should have direct link to the website for the status of the trackers.
  OpenURL('https://newtrackon.com/');
end;


procedure TFormTrackerModify.MenuTrackersKeepOrDeleteAllTrackersClick(Sender: TObject);
begin
  CheckedOnOffAllTrackers(TMenuItem(Sender).Tag = 1);
end;

procedure TFormTrackerModify.
MenuUpdateTorrentAddAfterKeepOriginalIntactAndRemoveNothingClick(Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing;
  UpdateTorrent;
end;

procedure TFormTrackerModify.MenuUpdateTorrentAddAfterRemoveNewClick(Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloAppendNewAfterAndKeepOriginalIntact;
  UpdateTorrent;
end;

procedure TFormTrackerModify.MenuUpdateTorrentAddAfterRemoveOriginalClick(
  Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloAppendNewAfterAndKeepNewIntact;
  UpdateTorrent;
end;

procedure TFormTrackerModify.
MenuUpdateTorrentAddBeforeKeepOriginalIntactAndRemoveNothingClick(Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing;
  UpdateTorrent;
end;

procedure TFormTrackerModify.MenuUpdateTorrentAddBeforeRemoveNewClick(Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent :=
    tloInsertNewBeforeAndKeepOriginalIntact;
  UpdateTorrent;
end;

procedure TFormTrackerModify.MenuUpdateTorrentAddBeforeRemoveOriginalClick(
  Sender: TObject);
begin
  //User have selected to add new tracker.
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloInsertNewBeforeAndKeepNewIntact;
  UpdateTorrent;
end;

procedure TFormTrackerModify.MenuUpdateTorrentSortClick(Sender: TObject);
begin
  //User can select to add new tracker as sorted.
  FTrackerList.TrackerListOrderForUpdatedTorrent := tloSort;
  UpdateTorrent;
end;



function TFormTrackerModify.LoadTorrentViaDir(const Dir: utf8string): boolean;
var
  TorrentFilesNameStringList: TStringList;
begin
  //place all the torrent file name in TorrentFilesNameStringList
  TorrentFilesNameStringList := TStringList.Create;
  try
    torrent_miscellaneous.LoadTorrentViaDir(Dir, TorrentFilesNameStringList);
    //add the torrent file name to AddTorrentFileList()
    Result := AddTorrentFileList(TorrentFilesNameStringList);
  finally
    //Free all the list we temporary created.
    TorrentFilesNameStringList.Free;
  end;
end;


procedure TFormTrackerModify.FormDropFiles(Sender: TObject;
  const FileNames: array of utf8string);
var
  Count: integer;
  TorrentFileNameStringList, //for the torrent files
  TrackerFileNameStringList //for the trackers files
  : TStringList;

  TorrentFileSelectionDetected,

  //ViewUpdateBegin must be called one time. Keep track of it.
  ViewUpdateBeginActiveOneTimeOnly: boolean;

  FileNameOrDirStr: utf8string;
  PreviousMemoText: string;
begin
  //Drag and drop a folder or files?

  //Restored when the dropped tracker lists are not valid.
  PreviousMemoText := MemoNewTrackers.Text;

  //Change cursor
  ShowHourGlassCursor(True);

  // Always clear the previous torrent files selection.
  // keep track if torrent file is detected in drag/drop
  // need this to call ClearAllTorrentFilesNameAndTrackerInside()
  //    this will clear the previous torrent loaded.
  TorrentFileSelectionDetected := False;


  ViewUpdateBeginActiveOneTimeOnly := False;

  //Remember every file names from drag and drop.
  //It can be mix *.torrent + trackers.txt files
  TorrentFileNameStringList := TStringList.Create;
  TrackerFileNameStringList := TStringList.Create;

  try

    //process all the files and/or directory that is drop by user.
    for Count := low(FileNames) to High(FileNames) do
    begin
      FileNameOrDirStr := UTF8Trim(FileNames[Count]);


      //a folder, its name may contain a dot. Must be checked before the file extensions.
      if PathIsTorrentFolder(FileNameOrDirStr) then
      begin

        //if first time a torrent detected then ClearAllTorrentFilesNameAndTrackerInside
        if not TorrentFileSelectionDetected then
        begin
          TorrentFileSelectionDetected := True;
          ClearAllTorrentFilesNameAndTrackerInside;
        end;

        if not ViewUpdateBeginActiveOneTimeOnly then
        begin
          ViewUpdateBeginActiveOneTimeOnly := True;
          ViewUpdateBegin;
        end;

        LoadTorrentViaDir(FileNameOrDirStr);

      end
      //if '.torrent' then add to TorrentFileNameStringList
      else if PathIsTorrentFile(FileNameOrDirStr) then
      begin

        //if first time a torrent detected then ClearAllTorrentFilesNameAndTrackerInside
        if not TorrentFileSelectionDetected then
        begin
          TorrentFileSelectionDetected := True;
          ClearAllTorrentFilesNameAndTrackerInside;
        end;

        TorrentFileNameStringList.Add(FileNameOrDirStr);
      end
      //if '.txt' then it must be a tracker list.
      else if PathIsTrackerListFile(FileNameOrDirStr) then
      begin
        try
          TrackerFileNameStringList.LoadFromFile(FileNameOrDirStr);
          //Remove comments after the URL. Else the validation fails and the memo is cleared.
          SanitizeTrackerList(TrackerFileNameStringList);
          MemoNewTrackers.Append(UTF8Trim(TrackerFileNameStringList.Text));
        except
          //suppress any error in loading the file
          FileNameOrDirStr := FileNameOrDirStr;
        end;
      end;

    end;//for


    //Check for error in tracker list
    if not CopyUserInputNewTrackersToList then
    begin //When error restore the tracker list the user already had.
      MemoNewTrackers.Text := PreviousMemoText;
    end;

    //the torrent files we have collected here must be add to AddTorrentFileList()
    if TorrentFileNameStringList.Count > 0 then
    begin

      if not ViewUpdateBeginActiveOneTimeOnly then
      begin
        ViewUpdateBeginActiveOneTimeOnly := True;
        ViewUpdateBegin;
      end;

      AddTorrentFileList(TorrentFileNameStringList);

    end;

  finally
    //Free all the list we temporary created.
    TorrentFileNameStringList.Free;
    TrackerFileNameStringList.Free;
    ShowHourGlassCursor(False);
  end;




  //if ViewUpdateBegin is called then ViewUpdateEnd must also be called.
  if ViewUpdateBeginActiveOneTimeOnly then
    ViewUpdateEnd;

end;

procedure TFormTrackerModify.FormShow(Sender: TObject);
begin
  //In console mode do not show the program.
  if FConsoleMode then
    Visible := False;
end;

procedure TFormTrackerModify.LabeledEditInfoSourceEditingDone(Sender: TObject);
begin
  FTrackerList.SourceTag := TLabeledEdit(Sender).Text;
end;


function TFormTrackerModify.AddTorrentFileList(TorrentFileNameStringList:
  TStringList): boolean;
  //This called from 'add folder' or 'drag and drop'
var
  Count: integer;
  TorrentFileNameStr: utf8string;
begin
{ Every torrent file must be decoded for the tracker list inside.
  This torrent tracker list is add to FTrackerList.TrackerFromInsideTorrentFilesList.
  All the torrent files name are added to FTrackerList.TorrentFileNameList.

  Called when user do drag and drop, File open torrent file/dir
}
  if TorrentFileNameStringList.Count > 0 then
  begin
    for Count := 0 to TorrentFileNameStringList.Count - 1 do
    begin
      //process one torrent file name for each loop.
      TorrentFileNameStr := TorrentFileNameStringList[Count];

      if DecodeTorrentFile(TorrentFileNameStr) then
      begin
        //This torrent have announce list(trackers) decoded.
        //Now add all this torrent trackers to the 'general' list of trackers.
        UpdateTorrentTrackerList;
        //Add this torrent file to the 'general' list of torrent file names
        FTrackerList.TorrentFileNameList.Add(TorrentFileNameStr);
      end
      else
      begin
        //Something is wrong. Can not decode torrent tracker item.
        //Cancel everything. The view rows must go too, they are index-matched to TorrentFileNameList.
        FTrackerList.TorrentFileNameList.Clear;
        FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
        FTrackerList.TrackerFromPrivateTorrentsList.Clear;
        ClearTorrentFilesView;
        ShowUserErrorMessage('Error: Can not read torrent.', TorrentFileNameStr);
        Result := False;
        exit;
      end;
    end;
  end;
  Result := True;
end;


function TFormTrackerModify.ReloadAllTorrentAndRefreshView: boolean;
var
  i: integer;
begin
{
  This is called after updating the torrent.
  We want to re-read the all torrent files.
  And show that everything is updated and OK
}

  //will be set to False if error occurs
  Result := True;

  ViewUpdateBegin;
  //Copy all the trackers in inside the torrent files to FTrackerList.TrackerFromInsideTorrentFilesList
  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  FTrackerList.TrackerFromPrivateTorrentsList.Clear;
  i := 0;
  while i < FTrackerList.TorrentFileNameList.Count do
  begin
    if DecodeTorrentFile(FTrackerList.TorrentFileNameList[i]) then
    begin
      UpdateTorrentTrackerList;
      Inc(i);
    end
    else
    begin
      //some files can not be read/decoded
      Result := False;
      //No view row is made for it. Drop it, or the rows no longer match TorrentFileNameList.
      FTrackerList.TorrentFileNameList.Delete(i);
    end;
  end;

  //refresh the view
  ViewUpdateEnd;

end;

procedure TFormTrackerModify.ClearAllTorrentFilesNameAndTrackerInside;
begin
  FTrackerList.TorrentFileNameList.Clear;
  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  FTrackerList.TrackerFromPrivateTorrentsList.Clear;
  //  Caption := FORM_CAPTION;
  //  ShowTorrentFilesAfterBeingLoaded;
end;

procedure TFormTrackerModify.ClearTorrentFilesView;
begin
  //Same clearing as ViewUpdateBegin, but safe to call between ViewUpdateBegin and ViewUpdateEnd.
  CheckListBoxPublicPrivateTorrent.Clear;
  StringGridTorrentData.Clear;
  FControllerGridTorrentData.ClearAllImageIndex;
  StringGridTorrentData.RowCount := 1;
  FControllerTreeviewTorrentData.Clear;
end;


procedure TFormTrackerModify.ViewUpdateBegin;
begin
  //Called before loading torrent file.

  FControllerTreeviewTorrentData.BeginUpdate;

  //Do not show being updating till finish updating data.
  StringGridTorrentData.BeginUpdate;
  CheckListBoxPublicPrivateTorrent.Items.BeginUpdate;


  //Clear all the user data 'View' elements. This will be filled with new data.
  CheckListBoxPublicPrivateTorrent.Clear; //Use in update torrent!
  StringGridTorrentData.Clear;
  FControllerGridTorrentData.ClearAllImageIndex;
  //RowCount is 0 after Clear. But must be 1 to make it work.
  StringGridTorrentData.RowCount := 1;

end;

procedure TFormTrackerModify.ViewUpdateOneTorrentFileDecoded;
var
  RowIndex: integer;
  TorrentFileNameStr, PrivateStr: utf8string;
  DateTimeStr: string;
begin
  //Called after loading torrent file.
  //There are 3 tab pages that need to be filled with new one torrent file data.

  TorrentFileNameStr := ExtractFileName(FDecodePresentTorrent.FilenameTorrent);

  //---------------------  Fill the Tree view with new torrent data
  FControllerTreeviewTorrentData.AddOneTorrentFileDecoded(FDecodePresentTorrent);

  //---------------------   Add it to the checklist box Public/private torrent
  RowIndex := CheckListBoxPublicPrivateTorrent.Items.Add(TorrentFileNameStr);
  //Check it for public/private flag
  CheckListBoxPublicPrivateTorrent.Checked[RowIndex] :=
    not FDecodePresentTorrent.PrivateTorrent;

  //---------------------  Fill the Grid Torrent Data/Info
  //date time in iso format
  if FDecodePresentTorrent.CreatedDate <> 0 then
    DateTimeToString(DateTimeStr, 'yyyy-MM-dd hh:nn:ss',
      FDecodePresentTorrent.CreatedDate)
  else //some torrent does not have CreatedDate
    DateTimeStr := '';

  //private or public torrent
  if FDecodePresentTorrent.PrivateTorrent then
    PrivateStr := 'yes'
  else
    PrivateStr := 'no';

  //Copy all the torrent info to the grid column.
  FControllerGridTorrentData.TorrentFile := TorrentFileNameStr;
  FControllerGridTorrentData.InfoFileName := FDecodePresentTorrent.Name;
  FControllerGridTorrentData.TorrentVersion :=
    FDecodePresentTorrent.TorrentVersionToString;
  case FDecodePresentTorrent.TorrentVersion of
    tv_V1:
    begin
      FControllerGridTorrentData.InfoHash := 'V1: ' + FDecodePresentTorrent.InfoHash_V1;
    end;
    tv_V2:
    begin
      FControllerGridTorrentData.InfoHash := 'V2: ' + FDecodePresentTorrent.InfoHash_V2;
    end;
    tv_Hybrid:
    begin // Show only V2 hash. No space for both V1 and V2
      FControllerGridTorrentData.InfoHash := 'V2: ' + FDecodePresentTorrent.InfoHash_V2;
    end;
    else
      FControllerGridTorrentData.InfoHash := 'N/A'
  end;
  FControllerGridTorrentData.Padding := FDecodePresentTorrent.PaddingToString;
  FControllerGridTorrentData.CreatedOn := DateTimeStr;
  FControllerGridTorrentData.CreatedBy := FDecodePresentTorrent.CreatedBy;
  FControllerGridTorrentData.Comment := FDecodePresentTorrent.Comment;
  FControllerGridTorrentData.PrivateTorrent := PrivateStr;
  FControllerGridTorrentData.InfoSource := FDecodePresentTorrent.InfoSource;
  FControllerGridTorrentData.PieceLength :=
    format('%6d', [FDecodePresentTorrent.PieceLength div 1024]); //Show as KiBytes
  FControllerGridTorrentData.TotalSize :=
    format('%9d', [FDecodePresentTorrent.TotalFileSize div 1024]); //Show as KiBytes
  FControllerGridTorrentData.IndexOrder :=
    format('%6d', [StringGridTorrentData.RowCount - 1]);
  //Must keep track of order when sorted back

  //All the string data are filed. Copy it now to the grid
  FControllerGridTorrentData.AppendRow;

end;



procedure TFormTrackerModify.ViewUpdateEnd;
begin
  //Called after finish all torrent file loading.

  //Show what we have updated.
  FControllerTreeviewTorrentData.EndUpdate;
  StringGridTorrentData.EndUpdate;
  CheckListBoxPublicPrivateTorrent.Items.EndUpdate;


  GroupBoxPresentTracker.Caption :=
    GROUPBOX_PRESENT_TRACKERS_CAPTION + ' (List count: ' +
    IntToStr(FTrackerList.TrackerFromInsideTorrentFilesList.Count) + ' )';

  //Show all the tracker inside the torrent files.
  ShowTrackerInsideFileList;
  //Some tracker must be removed. Console and windows mode.
  UpdateViewRemoveTracker;

  //Show user how many files are loaded
  ViewUpdateFormCaption;

end;

procedure TFormTrackerModify.ViewUpdateFormCaption;
begin
  //Called when user load the torrent + update the torrent.

  //Show user how many files are loaded
  Caption := FORM_CAPTION + '( Torrent files: ' +
    IntToStr(FTrackerList.TorrentFileNameList.Count) + ' )';

  if CheckBoxSkipAnnounceCheck.Checked then
  begin
    Caption := Caption + '(-SAC)';
  end;
end;

procedure TFormTrackerModify.ShowHourGlassCursor(HourGlass: boolean);
begin
  if HourGlass then
    screen.Cursor := crHourGlass
  else
    screen.Cursor := crDefault;
end;

end.
