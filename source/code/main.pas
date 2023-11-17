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
  Grids, controlergridtorrentdata, torrent_miscellaneous,
  controler_trackerlist_online, controler_treeview_torrent_data, ngosang_trackerslist;

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
    MenuHelpSeperator1: TMenuItem;
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
    MenuTrackersSeperator2: TMenuItem;
    MenuTrackersSeperator1: TMenuItem;
    MenuItemOnlineCheckDownloadNewTrackon: TMenuItem;
    MenuOnlineCheck: TMenuItem;
    MenuUpdateRandomize: TMenuItem;
    MenuUpdateTorrentAddBeforeKeepOriginalInstactAndRemoveNothing: TMenuItem;
    MenuUpdateTorrentAddAfterKeepOriginalInstactAndRemoveNothing: TMenuItem;
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
    procedure MenuUpdateTorrentAddBeforeKeepOriginalInstactAndRemoveNothingClick(
      Sender: TObject);
    procedure MenuUpdateTorrentAddBeforeRemoveNewClick(Sender: TObject);
    procedure MenuUpdateTorrentAddBeforeRemoveOriginalClick(Sender: TObject);
    procedure MenuUpdateTorrentSortClick(Sender: TObject);
    procedure MenuUpdateTorrentAddAfterKeepOriginalInstactAndRemoveNothingClick(
      Sender: TObject);
    procedure MenuUpdateRandomizeClick(Sender: TObject);

    //Menu online check
    procedure MenuItemOnlineCheckAppendStableTrackersClick(Sender: TObject);
    procedure MenuItemOnlineCheckDownloadNewTrackonClick(Sender: TObject);
  private
    { private declarations }

    FTrackerList: TTrackerList;
    FControlerTrackerListOnline: TControlerTrackerListOnline;
    Fcontroler_treeview_torrent_data: Tcontroler_treeview_torrent_data;
    FDownloadStatus: boolean;

    FngosangTrackerList: TngosangTrackerList;
    // is the present torrent file being process
    FDecodePresentTorrent: TDecodeTorrent;

    FDragAndDropStartUp, // user have start the program via Drag And Drop
    FConsoleMode, //user have start the program in console mode
    FFilePresentBanByUserList//There is a file 'remove_trackers.txt' detected
    : boolean;

    FFolderForTrackerListLoadAndSave: string;
    FLogFile, FTrackerFile: TextFile;
    FProcessTimeStart, FProcessTimeTotal: TDateTime;
    FControlerGridTorrentData: TControlerGridTorrentData;
    function CheckForAnnounce(const TrackerURL: utf8string): boolean;
    procedure AppendTrackersToMemoNewTrackers(TrackerList: TStringList);
    procedure ShowUserErrorMessage(const ErrorText: string; const FormText: string = '');
    function TrackerWithURLAndAnnounce(const TrackerURL: utf8string): boolean;
    procedure UpdateTorrent;
    procedure ShowHourGlassCursor(HourGlass: boolean);
    procedure ViewUpdateBegin;
    procedure ViewUpdateOneTorrentFileDecoded;
    procedure ViewUpdateEnd;
    procedure ViewUpdateFormCaption;
    procedure ClearAllTorrentFilesNameAndTrackerInside;
    procedure SaveTrackerFinalListToFile;
    procedure ConsoleModeOrDragAndDropStartupMode;
    procedure UpdateViewRemoveTracker;
    function ReloadAllTorrentAndRefreshView: boolean;
    function AddTorrentFileList(TorrentFileNameStringList: TStringList): boolean;
    function ReadAddTrackerFileFromUser(const FileName: utf8string): boolean;
    function LoadTorrentViaDir(const Dir: utf8string): boolean;
    function DecodeTorrentFile(const FileName: utf8string): boolean;
    procedure UpdateTrackerInsideFileList;
    procedure UpdateTorrentTrackerList;
    procedure ShowTrackerInsideFileList;
    function TestConnectionSSL: boolean;

    procedure CheckedOnOffAllTrackers(Value: boolean);
    function CopyUserInputNewTrackersToList(Temporary_SkipAnnounceCheck: boolean =
      False): boolean;
    procedure LoadTrackersTextFileAddTrackers(Temporary_SkipAnnounceCheck: boolean);
    procedure LoadTrackersTextFileRemoveTrackers;
  public
    { public declarations }
  end;

var
  FormTrackerModify: TFormTrackerModify;

implementation

uses fphttpclient, LCLIntf, lazutf8, LazFileUtils, trackerlist_online, LCLVersion;

const
  RECOMENDED_TRACKERS: array[0..2] of utf8string =
    (
    'udp://tracker.coppersurfer.tk:6969/announce',
    'udp://tracker.opentrackr.org:1337/announce',
    'wss://tracker.openwebtorrent.com'
    );

  //program name and version (http://semver.org/)
  FORM_CAPTION = 'Bittorrent tracker editor (1.33.0/LCL ' +
    lcl_version + '/FPC ' + {$I %FPCVERSION%} + ')';

  GROUPBOX_PRESENT_TRACKERS_CAPTION =
    'Present trackers in all torrent files. Select the one that you want to keep. And added to all torrent files.';

{$R *.lfm}

{ TFormTrackerModify }

procedure TFormTrackerModify.FormCreate(Sender: TObject);
begin
  //Test the working for SSL connection
  if TestConnectionSSL then
  begin
    // shutdown the GUI program
    Application.terminate;
    Exit;
  end;

  {$IFDEF LINUX}
  // if a ubuntu snap program then save it to other place
   FFolderForTrackerListLoadAndSave := GetEnvironmentVariable('SNAP_USER_COMMON');
   if FFolderForTrackerListLoadAndSave = '' then
   begin
     // NOT a snap program
     FFolderForTrackerListLoadAndSave := ExtractFilePath(Application.ExeName);
   end
   else
   begin
     // A snap program
     FFolderForTrackerListLoadAndSave := AppendPathDelim(FFolderForTrackerListLoadAndSave);
   end;
  {$ELSE}
  // Save at the same place as the application file
  FFolderForTrackerListLoadAndSave := ExtractFilePath(Application.ExeName);
  {$ENDIF}

  //Create controler for StringGridTorrentData
  FControlerGridTorrentData := TControlerGridTorrentData.Create(StringGridTorrentData);

  // Default there is an announce check
  FTrackerList.SkipAnnounceCheck := False;

  // Default do not add source tag to the info.
  FTrackerList.SourceTag := '';

  //Log file output string List.
  FTrackerList.LogStringList := TStringList.Create;

  //Create filename list for all the torrent files.
  FTrackerList.TorrentFileNameList := TStringList.Create;
  FTrackerList.TorrentFileNameList.Duplicates := dupIgnore;
  //Must NOT be sorted. Must in sync with CheckListBoxPublicPrivateTorrent.
  FTrackerList.TorrentFileNameList.Sorted := False;

  //Create ban tracker list where the user can manualy add items to it.
  FTrackerList.TrackerBanByUserList := TStringList.Create;
  FTrackerList.TrackerBanByUserList.Duplicates := dupIgnore;
  FTrackerList.TrackerBanByUserList.Sorted := False;

  //Create deselect tracker list where the user select via user interface CheckBoxRemoveAllSourceTag
  FTrackerList.TrackerManualyDeselectedByUserList := TStringList.Create;
  FTrackerList.TrackerManualyDeselectedByUserList.Duplicates := dupIgnore;
  FTrackerList.TrackerManualyDeselectedByUserList.Sorted := False;

  //Create tracker list where the user can manualy add items to it
  FTrackerList.TrackerAddedByUserList := TStringList.Create;
  FTrackerList.TrackerAddedByUserList.Duplicates := dupIgnore;
  //Trackers List added by user must keep in the same order.
  FTrackerList.TrackerAddedByUserList.Sorted := False;

  //drag and drop tracker list will accept duplicates in memo text, if false. Need to check out why.

  //Create tracker list where all the trackers from all the torrent files are collected
  FTrackerList.TrackerFromInsideTorrentFilesList := TStringList.Create;
  FTrackerList.TrackerFromInsideTorrentFilesList.Duplicates := dupIgnore;
  //Must be sorted. is visible to user. In tracker list tab page.
  FTrackerList.TrackerFromInsideTorrentFilesList.Sorted := True;

  //Create tracker list that combine all other together.
  FTrackerList.TrackerFinalList := TStringList.Create;
  FTrackerList.TrackerFinalList.Duplicates := dupIgnore;
  //must NOT be sorted. Must keep the original order intact.
  FTrackerList.TrackerFinalList.Sorted := False;

  //Decoding class for torrent.
  FDecodePresentTorrent := TDecodeTorrent.Create;

  //Create view for trackerURL with CheckBoxRemoveAllSourceTag
  FControlerTrackerListOnline :=
    TControlerTrackerListOnline.Create(StringGridTrackerOnline,
    FTrackerList.TrackerFromInsideTorrentFilesList, @TrackerWithURLAndAnnounce);

  //Create view for treeview data of all the torrent files
  Fcontroler_treeview_torrent_data :=
    Tcontroler_treeview_torrent_data.Create(TabSheetTorrentsContents);

  //start the program at mimimum visual size. (this is optional)
  Width := Constraints.MinWidth;
  Height := Constraints.MinHeight;

  //there must be two command line or more for a console mode.
  //one is for drag and drop via shortcut in windows mode.
  FConsoleMode := ParamCount >= 2;
  FDragAndDropStartUp := ParamCount = 1;

  //Show the default trackers
  LoadTrackersTextFileAddTrackers(True);

  //Load the unwanted trackers list.
  LoadTrackersTextFileRemoveTrackers;

  //Create download for ngosang tracker list
  FngosangTrackerList := TngosangTrackerList.Create;

  //Start program in console mode ( >= 2)
  //or in windows mode via shortcut with drag/drop ( = 1)
  if ParamCount > 0 then
  begin
    ConsoleModeOrDragAndDropStartupMode;
  end;

  //There should be no more exception made for the drag and drop
  FDragAndDropStartUp := False;

  //Update some captions
  ViewUpdateFormCaption;
  GroupBoxPresentTracker.Caption := GROUPBOX_PRESENT_TRACKERS_CAPTION;
end;

procedure TFormTrackerModify.CheckBoxSkipAnnounceCheckChange(Sender: TObject);
begin
  FTrackerList.SkipAnnounceCheck := CheckBoxSkipAnnounceCheck.Checked;
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
  FTrackerList.LogStringList.Free;
  FTrackerList.TrackerFinalList.Free;
  FDecodePresentTorrent.Free;
  FTrackerList.TrackerAddedByUserList.Free;
  FTrackerList.TrackerBanByUserList.Free;
  FTrackerList.TrackerFromInsideTorrentFilesList.Free;
  FTrackerList.TorrentFileNameList.Free;
  FControlerGridTorrentData.Free;
  FTrackerList.TrackerManualyDeselectedByUserList.Free;
  FControlerTrackerListOnline.Free;
end;

procedure TFormTrackerModify.MenuFileTorrentFolderClick(Sender: TObject);
begin
  ClearAllTorrentFilesNameAndTrackerInside;
  ViewUpdateBegin;
  //User what to select one torrent file. Show the user dialog file selection.
  SelectDirectoryDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  if SelectDirectoryDialog1.Execute then
  begin
    ShowHourGlassCursor(True);
    LoadTorrentViaDir(SelectDirectoryDialog1.FileName);
    ShowHourGlassCursor(False);
  end;
  ViewUpdateEnd;
end;

procedure TFormTrackerModify.MenuHelpVisitWebsiteClick(Sender: TObject);
begin
  //There is no help file in this progam. Show user main web site.
  OpenURL('https://github.com/GerryFerdinandus/bittorrent-tracker-editor');
end;

procedure TFormTrackerModify.MenuHelpVisitNgosangClick(Sender: TObject);
begin
  //newTrackon trackers is being used in this program.
  OpenURL('https://github.com/ngosang/trackerslist');
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllBestIpClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_Best_IP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllHttpClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_HTTP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllHttpsClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_HTTPS);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllIpClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_IP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllUdpClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_UDP);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendAllWsClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_All_WS);
end;

procedure TFormTrackerModify.MenuItemNgosangAppendBestClick(Sender: TObject);
begin
  AppendTrackersToMemoNewTrackers(FngosangTrackerList.TrackerList_Best);
end;

procedure TFormTrackerModify.MenuItemOnlineCheckSubmitNewTrackonClick(Sender: TObject);
var
  SendStatus: boolean;
  TrackerSendCount: integer;
  PopupStr: string;
begin
  try
    screen.Cursor := crHourGlass;
    SendStatus := FControlerTrackerListOnline.SubmitTrackers(
      FTrackerList.TrackerFromInsideTorrentFilesList, TrackerSendCount);
  finally
    screen.Cursor := crDefault;
  end;

  if SendStatus then
  begin
    //Succesful upload
    PopupStr := format('Successful upload of %d unique tracker URL', [TrackerSendCount]);
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
begin
  //Append all the trackers to MemoNewTrackers
  MemoNewTrackers.Lines.BeginUpdate;
  for Tracker in TrackerList do
  begin
    MemoNewTrackers.Lines.Add(tracker);
  end;
  MemoNewTrackers.Lines.EndUpdate;

  //Check for error in tracker list
  if not CopyUserInputNewTrackersToList then
  begin
    MemoNewTrackers.Lines.Clear;
  end;
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
  AppendTrackersToMemoNewTrackers(FControlerTrackerListOnline.StableTrackers);
end;

procedure TFormTrackerModify.MenuItemOnlineCheckDownloadNewTrackonClick(
  Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    FDownloadStatus := FControlerTrackerListOnline.DownloadTrackers_All_Live_Stable;
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

procedure TFormTrackerModify.ShowUserErrorMessage(const ErrorText: string;
  const FormText: string);
begin
  if FConsoleMode then
  begin
    if FormText = '' then
      FTrackerList.LogStringList.Add(ErrorText)
    else
      FTrackerList.LogStringList.Add(FormText + ' : ' + ErrorText);
  end
  else
  begin
    if FormText = '' then
      Application.MessageBox(PChar(@ErrorText[1]), '', MB_ICONERROR)
    else
      Application.MessageBox(PChar(@ErrorText[1]), PChar(@FormText[1]), MB_ICONERROR);
  end;
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
    if FControlerTrackerListOnline.Count > 0 then
    begin
      for i := 0 to FControlerTrackerListOnline.Count - 1 do
      begin
        if FControlerTrackerListOnline.TrackerStatus(i) = Value then
        begin
          FControlerTrackerListOnline.Checked[i] := False;
        end;
      end;
    end;
  end;

begin
  //check if tracker is already downloaded
  if not FDownloadStatus then
  begin
    MenuItemOnlineCheckDownloadNewTrackonClick(nil);
  end;

  //0 = Unstable
  //1 = Dead
  //2 = Unknown
  case TMenuItem(Sender).Tag of
    0: UncheckTrackers(tos_live_but_unstable);
    1: UncheckTrackers(tos_dead);
    2: UncheckTrackers(tos_unknown);
    else
      Assert(True, 'Unknown Menu item selection')
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
  Reply, BoxStyle, i, CountTrackers: integer;
  PopUpMenuStr: string;
  SomeFilesCannotBeWriten, SomeFilesAreReadOnly, AllFilesAreReadBackCorrectly: boolean;

begin
  //Update all the torrent files.

  //The StringGridTorrentData where the comment are place by user
  //    must be in sync again with FTrackerList.TorrentFileNameList.
  //Undo all posible sort column used by the user. Sort it back to 'begin state'
  FControlerGridTorrentData.ReorderGrid;

  //initial value is false, will be set to true if some file fails to write
  SomeFilesCannotBeWriten := False;

  try

    if not FConsoleMode then
    begin
      //Warn user before updating the torrent
      BoxStyle := MB_ICONWARNING + MB_OKCANCEL;
      Reply := Application.MessageBox('Warning: There is no undo.',
        'Torrent files will be change!', BoxStyle);
      if Reply <> idOk then
      begin
        ShowHourGlassCursor(True);
        exit;
      end;
    end;


    //Must have some torrent selected
    if (FTrackerList.TorrentFileNameList.Count = 0) then
    begin
      ShowUserErrorMessage('ERROR: No torrent file selected');
      ShowHourGlassCursor(True);
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

    //In console mode we can ignore this warning
    if not FConsoleMode and (CountTrackers = 0) then
    begin //Torrent without a tracker is posible. But is this what the user realy want? a DHT torrent.
      BoxStyle := MB_ICONWARNING + MB_OKCANCEL;
      Reply := Application.MessageBox(
        'Warning: Create torrent file without any URL of the tracker?',
        'There are no Trackers selected!', BoxStyle);
      if Reply <> idOk then
      begin
        ShowHourGlassCursor(False);
        exit;
      end;
      //Reset process timer
      ShowHourGlassCursor(True);
    end;

    //initial value is false, will be set to true if read only files are found
    SomeFilesAreReadOnly := False;

    if FTrackerList.TrackerListOrderForUpdatedTorrent = tloRandomize then
    begin
      Randomize;
    end;

    //process all the files one by one.
    //FTrackerList.TorrentFileNameList is not sorted it is still in sync with CheckListBoxPublicPrivateTorrent
    for i := 0 to FTrackerList.TorrentFileNameList.Count - 1 do
    begin //read the torrent file in FDecodePresentTorrent and modify it.

      //check for read only files. It can not be updated by tracker editor
      if (FileGetAttr(FTrackerList.TorrentFileNameList[i]) and faReadOnly) <> 0 then
      begin
        SomeFilesAreReadOnly := True;
        Continue;
      end;

      //read one torrent file. If error then skip it. (continue)
      if not FDecodePresentTorrent.DecodeTorrent(
        FTrackerList.TorrentFileNameList[i]) then
      begin
        Continue;
      end;

      //tloSort it is already process. But if not tloSort then process it.
      if FTrackerList.TrackerListOrderForUpdatedTorrent <> tloSort then
      begin
        //Add the new tracker before of after the original trackers inside the torrent.
        CombineFiveTrackerListToOne(FTrackerList.TrackerListOrderForUpdatedTorrent,
          FTrackerList, FDecodePresentTorrent.TrackerList);

        //How many trackers must be put inside each torrent file
        CountTrackers := FTrackerList.TrackerFinalList.Count;
      end;

      case CountTrackers of
        0://if no tracker selected then delete 'announce' and 'announce-list'
        begin
          FDecodePresentTorrent.RemoveAnnounce;
          FDecodePresentTorrent.RemoveAnnounceList;
        end;
        1://if one tracker selected then delete 'announce-list'
        begin
          //Announce use the only tracker present in the FTrackerFinalList. index 0
          FDecodePresentTorrent.ChangeAnnounce(FTrackerList.TrackerFinalList[0]);
          FDecodePresentTorrent.RemoveAnnounceList;
        end;
        else//More than 1 trackers selected. Create 'announce-list'
        begin
          //Announce use the first tracker from the list. index 0
          FDecodePresentTorrent.ChangeAnnounce(FTrackerList.TrackerFinalList[0]);
          FDecodePresentTorrent.ChangeAnnounceList(FTrackerList.TrackerFinalList);
        end;
      end;

      //update the torrent public/private flag
      if CheckListBoxPublicPrivateTorrent.Checked[i] then
      begin
        //Create a public torrent
        //if private torrent then make it public torrent by removing the private flag.
        if FDecodePresentTorrent.PrivateTorrent then
          FDecodePresentTorrent.RemovePrivateTorrentFlag;
      end
      else
      begin
        //Create a private torrent
        FDecodePresentTorrent.AddPrivateTorrentFlag;
      end;

      //update the comment item
      FDecodePresentTorrent.Comment := FControlerGridTorrentData.ReadComment(i + 1);

      //Update the source tag for private trackers
      if FTrackerList.RemoveAllSourceTag then
      begin
        // This will delete info:source item
        FDecodePresentTorrent.InfoSourceRemove;
      end
      else
      begin
        // Copy the new source tag, but it must not be empty.
        // Empty FTrackerList.SourceTag is the same as do not change anything.
        if FTrackerList.SourceTag <> '' then
        begin
          FDecodePresentTorrent.InfoSourceAdd(FTrackerList.SourceTag);
        end;
      end;

      //save the torrent file.
      if not FDecodePresentTorrent.SaveTorrent(FTrackerList.TorrentFileNameList[i]) then
      begin
        SomeFilesCannotBeWriten := True;
      end;

    end;//for

    // Can not create a file inside the app
    {$IFNDEF DARWIN}
    //Create tracker.txt file
    SaveTrackerFinalListToFile;
    {$ENDIF}

    //Show/reload the just updated torrent files.
    AllFilesAreReadBackCorrectly := ReloadAllTorrentAndRefreshView;

    //make sure cursor is default again
  finally
    ShowHourGlassCursor(False);
    ViewUpdateFormCaption;
  end;


  if FConsoleMode then
  begin
    //When succesfull the log file shows, 3 lines,
    //     OK + Count torrent files  + Count Trackers

    //if there is already a items inside there there must be something wrong.
    //Do not add 'OK'
    if FTrackerList.LogStringList.Count = 0 then
    begin
      FTrackerList.LogStringList.Add(CONSOLE_SUCCESS_STATUS);
      FTrackerList.LogStringList.Add(IntToStr(FTrackerList.TorrentFileNameList.Count));
      FTrackerList.LogStringList.Add(IntToStr(CountTrackers));
    end;
  end
  else
  begin

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
        Assert(True, 'case else: Should never been called. UpdateTorrent');
      end;

    end;//case


    //Check if there are some error that need to be notify to the end user.

    if not AllFilesAreReadBackCorrectly then
    begin
      //add warning if torrent files can not be read back again
      PopUpMenuStr := PopUpMenuStr +
        ' WARNING: Some torrent files can not be read back again after updating.';
    end;

    if SomeFilesAreReadOnly then
    begin
      //add warning if read only files are detected.
      PopUpMenuStr := PopUpMenuStr +
        ' WARNING: Some torrent files are not updated bacause they are READ-ONLY files.';
    end;

    if SomeFilesCannotBeWriten then
    begin
      //add warning if some files writen are failed. Someting is wrong with the disk.
      PopUpMenuStr := PopUpMenuStr +
        ' WARNING: Some torrent files are not updated bacause they failed at write.';
    end;

    //Show the MessageBox
    Application.MessageBox(
      PChar(@PopUpMenuStr[1]),
      '', MB_ICONINFORMATION + MB_OK);

  end;

end;


procedure TFormTrackerModify.SaveTrackerFinalListToFile;
var
  TrackerStr: utf8string;
begin
  //Create the tracker text file. The old one will be overwritten
  AssignFile(FTrackerFile, FFolderForTrackerListLoadAndSave + FILE_NAME_EXPORT_TRACKERS);
  ReWrite(FTrackerFile);
  for TrackerStr in FTrackerList.TrackerFinalList do
  begin
    WriteLn(FTrackerFile, TrackerStr);

    //Must create an empty line betwean trackers.
    //Every tracker must be a seperate tracker group.
    //This is what the user probably want.
    //The file content can then be copy/pasted to uTorrent etc.
    WriteLn(FTrackerFile, '');
  end;
  CloseFile(FTrackerFile);
end;

procedure TFormTrackerModify.ConsoleModeOrDragAndDropStartupMode;
var
  FileNameOrDirStr: utf8string;
  StringList: TStringList;
  MustExitWithErrorCode: boolean;
begin
  // There are two options
  //-
  // One parameter only. Program startup via DragAndDrop
  //    The first parameter[1] is path to file or dir.

  //-
  // Two parameter version. Always console mode.
  //    This is later version where there is more selection about the tracker list.

  //Will be set to True when error occure.
  MustExitWithErrorCode := False;
  ViewUpdateBegin;

  try
    if FConsoleMode then
    begin
      //Create the log file. The old one will be overwritten
      AssignFile(FLogFile, FFolderForTrackerListLoadAndSave + FILE_NAME_CONSOLE_LOG);
      ReWrite(FLogFile);
    end;

    //Get the startup command lime parameters.
    if ConsoleModeDecodeParameter(FileNameOrDirStr, FTrackerList) then
    begin
      //There is no error. Proceed with reading the torrent files

      if ExtractFileExt(FileNameOrDirStr) = '' then
      begin //There is no file extention. It must be a folder.
        if LoadTorrentViaDir(FileNameOrDirStr) then
        begin
          //Show all the tracker inside the torrent files.
          ShowTrackerInsideFileList;
          //Some tracker must be removed. Console and windows mode.
          UpdateViewRemoveTracker;

          if FConsoleMode then
          begin
            //update torrent
            UpdateTorrent;
          end;

        end
        else
        begin
          //failed to load the torrent via folders
          ShowUserErrorMessage('Can not load torrent via folder');
        end;

      end
      else //a single torrent file is selected?
      begin
        if ExtractFileExt(FileNameOrDirStr) = '.torrent' then
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

              if FConsoleMode then
              begin
                //update torrent
                UpdateTorrent;
              end;

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

    if FConsoleMode then
    begin
      //Write to log file. And close the file.
      WriteLn(FLogFile, FTrackerList.LogStringList.Text);
      CloseFile(FLogFile);

      //check if log data is success full
      //if (no data) or (not CONSOLE_SUCCESS_STATUS) then error
      MustExitWithErrorCode := FTrackerList.LogStringList.Count = 0;
      if not MustExitWithErrorCode then
      begin
        MustExitWithErrorCode := FTrackerList.LogStringList[0] <> CONSOLE_SUCCESS_STATUS;
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

  if FConsoleMode then
  begin
    //Always shutdown the program when in console mode.
    Application.terminate;
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
      FControlerTrackerListOnline.Checked[i] := False;
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
var
  TrackerStr: utf8string;
begin
  //Copy the trackers found in one torrent file to FTrackerList.TrackerFromInsideTorrentFilesList
  for TrackerStr in FDecodePresentTorrent.TrackerList do
  begin
    AddButIngnoreDuplicates(FTrackerList.TrackerFromInsideTorrentFilesList, TrackerStr);
  end;
end;

procedure TFormTrackerModify.ShowTrackerInsideFileList;
begin
  //Called after torrent is being loaded.
  FControlerTrackerListOnline.UpdateView;
end;


procedure TFormTrackerModify.CheckedOnOffAllTrackers(Value: boolean);
var
  i: integer;
begin
  //Set all the trackers CheckBoxRemoveAllSourceTag ON or OFF
  if FControlerTrackerListOnline.Count > 0 then
  begin
    for i := 0 to FControlerTrackerListOnline.Count - 1 do
    begin
      FControlerTrackerListOnline.Checked[i] := Value;
    end;
  end;
end;


function TFormTrackerModify.CopyUserInputNewTrackersToList(
  Temporary_SkipAnnounceCheck: boolean): boolean;
var
  TrackerStrLoop, TrackerStr, ErrorStr: utf8string;
begin
  {
   Called after 'update torrent' is selected.
   All the user entery from Memo text field will be add to FTrackerList.TrackerAddedByUserList.
  }
  FTrackerList.TrackerAddedByUserList.Clear;

  //Will set to false when error is detected
  Result := True;

  for TrackerStrLoop in MemoNewTrackers.Lines do
  begin
    TrackerStr := UTF8trim(TrackerStrLoop);

    //Skip empty line
    if TrackerStr = '' then
      continue;

    Result := ValidTrackerURL(TrackerStr);
    if Result then
    begin
      if CheckForAnnounce(TrackerStr) and (not Temporary_SkipAnnounceCheck) then
      begin
        Result := TrackerURLWithAnnounce(TrackerStr);
        if not Result then
        begin
          ErrorStr := 'ERROR: Tracker URL must end with /announce or /announce.php';
        end;
      end;
    end
    else
    begin
      ErrorStr := 'ERROR: Tracker URL must begin with http://, http:// or udp://';
    end;

    if Result then
    begin
      AddButIngnoreDuplicates(FTrackerList.TrackerAddedByUserList, TrackerStr);
    end
    else
    begin
      //do not continue the for loop
      break;
    end;

  end;//for loop

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
  //Copy items from FControlerTrackerListOnline to FTrackerList.TrackerFromInsideTorrentFilesList
  //Copy items from FControlerTrackerListOnline to FTrackerList.TrackerManualyDeselectedByUserList

  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  FTrackerList.TrackerManualyDeselectedByUserList.Clear;

  if FControlerTrackerListOnline.Count > 0 then
  begin
    for i := 0 to FControlerTrackerListOnline.Count - 1 do
    begin

      if FControlerTrackerListOnline.Checked[i] then
      begin
        //Selected by user
        AddButIngnoreDuplicates(FTrackerList.TrackerFromInsideTorrentFilesList,
          FControlerTrackerListOnline.TrackerURL(i)
          );
      end
      else
      begin
        //Delected by user
        AddButIngnoreDuplicates(
          FTrackerList.TrackerManualyDeselectedByUserList,
          FControlerTrackerListOnline.TrackerURL(i)
          );
      end;

    end;
  end;

end;

procedure TFormTrackerModify.LoadTrackersTextFileAddTrackers(
  Temporary_SkipAnnounceCheck: boolean);
var
  i: integer;
begin
  //Called at the start of the program. Load a trackers list from file

  //if no file is found the use the default tracker list.
  if not ReadAddTrackerFileFromUser(FFolderForTrackerListLoadAndSave +
    FILE_NAME_ADD_TRACKERS) then
  begin
    MemoNewTrackers.Lines.BeginUpdate;
    for i := low(RECOMENDED_TRACKERS) to high(RECOMENDED_TRACKERS) do
    begin
      MemoNewTrackers.Lines.Add(RECOMENDED_TRACKERS[i]);
    end;
    MemoNewTrackers.Lines.EndUpdate;
  end;

  //Check for error in tracker list
  if not CopyUserInputNewTrackersToList(Temporary_SkipAnnounceCheck) then
  begin
    MemoNewTrackers.Lines.Clear;
  end;
end;

procedure TFormTrackerModify.LoadTrackersTextFileRemoveTrackers;
var
  filename: utf8string;
begin
  filename := FFolderForTrackerListLoadAndSave + FILE_NAME_REMOVE_TRACKERS;
  try
    FFilePresentBanByUserList := FileExistsUTF8(fileName);
    if FFilePresentBanByUserList then
    begin
      FTrackerList.TrackerBanByUserList.LoadFromFile(fileName);
    end;
  except
    FFilePresentBanByUserList := False;
  end;

  SanatizeTrackerList(FTrackerList.TrackerBanByUserList);

end;


procedure TFormTrackerModify.MenuOpenTorrentFileClick(Sender: TObject);
var
  StringList: TStringList;
begin
  ClearAllTorrentFilesNameAndTrackerInside;
  ViewUpdateBegin;

  //User what to select a torrent file. Show the user dialog.
  OpenDialog.Title := 'Select a torrent file';
  OpenDialog.Filter := 'torrent|*.torrent';
  if OpenDialog.Execute then
  begin
    ShowHourGlassCursor(True);
    StringList := TStringList.Create;
    try
      StringList.Add(UTF8Trim(OpenDialog.FileName));
      AddTorrentFileList(StringList);
    finally
      StringList.Free;
      ShowHourGlassCursor(False);
    end;
  end;
  ViewUpdateEnd;

end;

procedure TFormTrackerModify.MenuTrackersAllTorrentArePublicPrivateClick(
  Sender: TObject);
var
  i: integer;
begin
  //Warn user about torrent Hash.
  if Application.MessageBox(
    'Warning: Changing the public/private torrent flag will change the info hash.',
    'Are you sure!', MB_ICONWARNING + MB_OKCANCEL) <> idOk then
    exit;

  //Set all the trackers publick/private CheckBoxRemoveAllSourceTag ON or OFF
  if CheckListBoxPublicPrivateTorrent.Count > 0 then
  begin
    for i := 0 to CheckListBoxPublicPrivateTorrent.Count - 1 do
    begin
      CheckListBoxPublicPrivateTorrent.Checked[i] := TMenuItem(Sender).Tag = 1;
    end;
  end;
end;


procedure TFormTrackerModify.MenuFileOpenTrackerListClick(Sender: TObject);
begin
  //Clear the present list
  MemoNewTrackers.Lines.Clear;
  //User what to select a tracker file. Show the user dialog.

  OpenDialog.Title := 'Select a tracker list file';
  OpenDialog.Filter := 'tracker text file|*.txt';
  if OpenDialog.Execute then
  begin
    ReadAddTrackerFileFromUser(OpenDialog.FileName);
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


function TFormTrackerModify.ReadAddTrackerFileFromUser(
  const FileName: utf8string): boolean;
var
  TrackerFileList: TStringList;
begin
  //read the file and show it to the user.
  TrackerFileList := TStringList.Create;
  try
    TrackerFileList.LoadFromFile(FileName);
    SanatizeTrackerList(TrackerFileList);
    MemoNewTrackers.Text := UTF8Trim(TrackerFileList.Text);
    Result := True;
  except
    Result := False;
    //suppres all error in reading the file.
  end;
  TrackerFileList.Free;

  // It can be simpler, but does this suport UTF8?
  //  MemoNewTrackers.Lines.LoadFromFile(FileName);
end;

procedure TFormTrackerModify.MenuTrackersKeepOrDeleteAllTrackersClick(Sender: TObject);
begin
  CheckedOnOffAllTrackers(TMenuItem(Sender).Tag = 1);
end;

procedure TFormTrackerModify.
MenuUpdateTorrentAddAfterKeepOriginalInstactAndRemoveNothingClick(Sender: TObject);
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
MenuUpdateTorrentAddBeforeKeepOriginalInstactAndRemoveNothingClick(Sender: TObject);
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
begin
  //Drag and drop a folder or files?

  //Change cursor
  ShowHourGlassCursor(True);

  // Always clear the previeus torrent files selection.
  // keep track if torrent file is detected in drag/drop
  // need this to call ClearAllTorrentFilesNameAndTrackerInside()
  //    this will clear the previeuse torrent loaded.
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


      //if '.torrent' then add to TorrentFileNameStringList
      if ExtractFileExt(FileNameOrDirStr) = '.torrent' then
      begin

        //if first time a torrent detected then ClearAllTorrentFilesNameAndTrackerInside
        if not TorrentFileSelectionDetected then
        begin
          TorrentFileSelectionDetected := True;
          ClearAllTorrentFilesNameAndTrackerInside;
        end;

        TorrentFileNameStringList.Add(FileNameOrDirStr);
      end;

      //if '.txt' then it must be a tracker list.
      if ExtractFileExt(FileNameOrDirStr) = '.txt' then
      begin
        try
          TrackerFileNameStringList.LoadFromFile(FileNameOrDirStr);
          MemoNewTrackers.Append(UTF8Trim(TrackerFileNameStringList.Text));
        except
          //supress any error in loading the file
        end;
      end;

      //if there is no file extention. It must be a torrent folder.
      if ExtractFileExt(FileNameOrDirStr) = '' then
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

      end;

    end;//for


    //Check for error in tracker list
    if not CopyUserInputNewTrackersToList then
    begin //When error clear tracker list.
      MemoNewTrackers.Lines.Clear;
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
        //Someting is wrong. Can not decode torrent tracker item.
        //Cancel everything.
        FTrackerList.TorrentFileNameList.Clear;
        FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
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
  TorrentFileStr: utf8string;
begin
{
  This is called after updating the torrent.
  We want to re-read the all torrent files.
  And show that everything is updated and OK
}

  //will be set to False if error occure
  Result := True;

  ViewUpdateBegin;
  //Copy all the trackers in inside the torrent files to FTrackerList.TrackerFromInsideTorrentFilesList
  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  for TorrentFileStr in FTrackerList.TorrentFileNameList do
  begin
    if DecodeTorrentFile(TorrentFileStr) then
    begin
      UpdateTorrentTrackerList;
    end
    else
    begin
      //some files can not be read/decoded
      Result := False;
    end;
  end;

  //refresh the view
  ViewUpdateEnd;

end;

procedure TFormTrackerModify.ClearAllTorrentFilesNameAndTrackerInside;
begin
  FTrackerList.TorrentFileNameList.Clear;
  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  //  Caption := FORM_CAPTION;
  //  ShowTorrentFilesAfterBeingLoaded;
end;


procedure TFormTrackerModify.ViewUpdateBegin;
begin
  //Called before loading torrent file.

  Fcontroler_treeview_torrent_data.BeginUpdate;

  //Do not show being updating till finish updating data.
  StringGridTorrentData.BeginUpdate;
  CheckListBoxPublicPrivateTorrent.Items.BeginUpdate;


  //Clear all the user data 'View' elements. This will be filled with new data.
  CheckListBoxPublicPrivateTorrent.Clear; //Use in update torrent!
  StringGridTorrentData.Clear;
  FControlerGridTorrentData.ClearAllImageIndex;
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
  Fcontroler_treeview_torrent_data.AddOneTorrentFileDecoded(FDecodePresentTorrent);

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
  FControlerGridTorrentData.TorrentFile := TorrentFileNameStr;
  FControlerGridTorrentData.InfoFileName := FDecodePresentTorrent.Name;
  FControlerGridTorrentData.InfoHash := FDecodePresentTorrent.InfoHash;
  FControlerGridTorrentData.CreatedOn := DateTimeStr;
  FControlerGridTorrentData.CreatedBy := FDecodePresentTorrent.CreatedBy;
  FControlerGridTorrentData.Comment := FDecodePresentTorrent.Comment;
  FControlerGridTorrentData.PrivateTorrent := PrivateStr;
  FControlerGridTorrentData.InfoSource := FDecodePresentTorrent.InfoSource;
  FControlerGridTorrentData.PieceLength :=
    format('%6d', [FDecodePresentTorrent.PieceLenght div 1024]); //Show as KiBytes
  FControlerGridTorrentData.TotaSize :=
    format('%9d', [FDecodePresentTorrent.TotalFileSize div 1024]); //Show as KiBytes
  FControlerGridTorrentData.IndexOrder :=
    format('%6d', [StringGridTorrentData.RowCount - 1]);
  //Must keep track of order when sorted back

  //All the string data are filed. Copy it now to the grid
  FControlerGridTorrentData.AppendRow;

end;



procedure TFormTrackerModify.ViewUpdateEnd;
begin
  //Called after finish all torrent file loading.

  //Show what we have updated.
  Fcontroler_treeview_torrent_data.EndUpdate;
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
//var
//ProcessTimeStr: string;
//  Hour, Minute, Second, MilliSecond: word;
begin
  //Called when user load the torrent + update the torrent.

{ //for performance debugging.
  DecodeTime(FProcessTimeTotal, Hour, Minute, Second, MilliSecond);
  ProcessTimeStr := IntToStr((Second * 1000) + MilliSecond) + ' mSec';
}
  //Show user how many files are loaded
  Caption := FORM_CAPTION + '( Torrent files: ' +
    IntToStr(FTrackerList.TorrentFileNameList.Count) + ' )';

  if CheckBoxSkipAnnounceCheck.Checked then
  begin
    Caption := Caption + '(-SAC)';
  end;


  //  + ' (Process Time: ' +  ProcessTimeStr + ' )'; //for debug purpose.
end;

procedure TFormTrackerModify.ShowHourGlassCursor(HourGlass: boolean);
begin
  if HourGlass then
  begin
    screen.Cursor := crHourGlass;
    FProcessTimeStart := now;
  end
  else
  begin
    screen.Cursor := crDefault;
    FProcessTimeTotal := now - FProcessTimeStart;
  end;

end;

function TFormTrackerModify.TestConnectionSSL: boolean;
begin
  Result := ParamCount = 1;
  if Result then
  begin
    // Check for the correct parameter.
    Result := UTF8Trim(ParamStr(1)) = '-TEST_SSL';
    if Result then
    begin
      // Check if there is SLL connection
      try
        TFPCustomHTTPClient.SimpleGet(
          'https://raw.githubusercontent.com/gerryferdinandus/bittorrent-tracker-editor/master/README.md');
      except
        //No SLL or no internet connection.
        System.ExitCode := 1;
      end;
    end;
  end;
end;

end.
