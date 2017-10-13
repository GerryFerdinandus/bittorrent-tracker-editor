{ MIT licence
  Copyright (c) Gerry Ferdinandus
}

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
  Grids, controlergridtorrentdata, torrent_miscellaneous, controler_trackerlist_online;

type


  { TFormTrackerModify }

  TFormTrackerModify = class(TForm)
    CheckListBoxPublicPrivateTorrent: TCheckListBox;
    GroupBoxTorrentContents: TGroupBox;
    GroupBoxPublicPrivateTorrent: TGroupBox;
    GroupBoxNewTracker: TGroupBox;
    GroupBoxPresentTracker: TGroupBox;
    MainMenu: TMainMenu;
    MemoNewTrackers: TMemo;
    MenuFile: TMenuItem;
    MenuFileTorrentFolder: TMenuItem;
    MenuFileOpenTrackerList: TMenuItem;
    MenuHelpReportingIssue: TMenuItem;
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
    MenuItemTorrentFilesTreeHideAll: TMenuItem;
    MenuItemTorrentFilesTreeShowTrackers: TMenuItem;
    MenuItemTorrentFilesTreeShowInfo: TMenuItem;
    MenuItemTorrentFilesTreeShowAll: TMenuItem;
    MenuItemTorrentFilesTreeShowFiles: TMenuItem;
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
    PopupMenuTorrentFilesContent: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    StringGridTrackerOnline: TStringGrid;
    StringGridTorrentData: TStringGrid;
    TabSheetTorrentsContents: TTabSheet;
    TabSheetTorrentData: TTabSheet;
    TabSheetTrackersList: TTabSheet;
    TabSheetPublicPrivateTorrent: TTabSheet;
    TreeViewFileContents: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //Drag and drop '*.torrent' files/directory or 'tracker.txt'
    procedure FormDropFiles(Sender: TObject; const FileNames: array of UTF8String);

    //At start of the program the form will be show/hide
    procedure FormShow(Sender: TObject);
    procedure MenuHelpReportingIssueClick(Sender: TObject);
    procedure MenuHelpVisitWebsiteClick(Sender: TObject);

    //Popup menu in treeview show all/hide all/ individual items selection.
    procedure MenuItemTorrentFilesTreeShowAllClick(Sender: TObject);
    procedure MenuItemTorrentFilesTreeHideAllClick(Sender: TObject);
    procedure MenuItemTorrentFilesTreeShowOrHideItemClick(Sender: TObject);

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
    FDownloadStatus: boolean;


    // is the present torrent file being process
    FDecodePresentTorrent: TDecodeTorrent;

    FConcoleMode, //user have start the program in console mode
    FFilePresentBanByUserList//There is a file 'remove_trackers.txt' detected
    : boolean;


    FLogFile, FTrackerFile: TextFile;
    FTotalFileInsideTorrent: integer;
    FTotalFileSizeInsideTorrent: int64;
    FProcessTimeStart, FProcessTimeTotal: TDateTime;
    FTreeNodeRoot: TTreeNode;
    FControlerGridTorrentData: TControlerGridTorrentData;

    procedure UpdateTorrent;
    procedure ShowHourGlassCursor(HourGlass: boolean);
    procedure ViewUpdateBegin(ClearView: boolean = True);
    procedure ViewUpdateOneTorrentFileDecoded;
    procedure ViewUpdateEnd;
    procedure ViewUpdateFormCaption;
    procedure ClearAllTorrentFilesNameAndTrackerInside;

    procedure MenuItemTorrentFilesTreeSyncWithPopupMenu;
    procedure SaveTrackerFinalListToFile;
    procedure ConsoleMode;


    procedure UpdateViewRemoveTracker;


    procedure ReloadAllTorrentAndRefreshView;
    function AddTorrentFileList(TorrentFileNameStringList: TStringList): boolean;
    function ReadAddTrackerFileFromUser(const FileName: UTF8String): boolean;
    function LoadTorrentViaDir(const Dir: UTF8String): boolean;
    function DecodeTorrentFile(const FileName: UTF8String): boolean;
    procedure UpdateTrackerInsideFileList;
    procedure UpdateTorrentTrackerList;
    procedure ShowTrackerInsideFileList;

    procedure CheckedOnOffAllTrackers(Value: boolean);
    function CopyUserInputNewTrackersToList: boolean;
    procedure LoadTrackersTextFileAddTrackers;
    procedure LoadTrackersTextFileRemoveTrackers;
  public
    { public declarations }
  end;

var
  FormTrackerModify: TFormTrackerModify;

implementation

uses LCLIntf, lazutf8, LazFileUtils, trackerlist_online;

const
  RECOMENDED_TRACKERS: array[0..2] of UTF8String =
    (
    'udp://tracker.coppersurfer.tk:6969/announce',
    'udp://tracker.leechers-paradise.org:6969/announce',
    'udp://tracker.opentrackr.org:1337/announce'
    );
  //program name and version (http://semver.org/)
  FORM_CAPTION = 'Bittorrent tracker editor (1.33.0.beta.1)';
  TORRENT_FILES_CONTENTS_FORM_CAPTION =
    'Show all the files inside the torrents. (Use right mouse for popup menu.)';

  GROUPBOX_PRESENT_TRACKERS_CAPTION =
    'Present trackers in all torrent files. Select the one that you want to keep. And added to all torrent files.';


  //'add trackers' text file must be place in the same directory as the program.
  ADD_TRACKERS_FILE_NAME = 'add_trackers.txt';

  //'remove trackers' text file must be place in the same directory as the program.
  REMOVE_TRACKERS_FILE_NAME = 'remove_trackers.txt';

  //'export trackers' text file wil be created in the same directory as the program.
  EXPORT_TRACKERS_FILE_NAME = 'export_trackers.txt';

  //'log' text file will be saved in the same directory as the program
  // only in the console mode.
  LOG_FILE_NAME = 'console_log.txt';

{$R *.lfm}

{ TFormTrackerModify }

procedure TFormTrackerModify.FormCreate(Sender: TObject);
begin

  //Create controler for StringGridTorrentData
  FControlerGridTorrentData := TControlerGridTorrentData.Create(StringGridTorrentData);

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

  //Create deselect tracker list where the user select via user interface checkbox
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

  //Create view for trackerURL with checkbox
  FControlerTrackerListOnline :=
    TControlerTrackerListOnline.Create(StringGridTrackerOnline,
    FTrackerList.TrackerFromInsideTorrentFilesList);


  //start the program at mimimum visual size. (this is optional)
  Width := Constraints.MinWidth;
  Height := Constraints.MinHeight;

  //Show the default trackers
  LoadTrackersTextFileAddTrackers;

  //Load the unwanted trackers list.
  LoadTrackersTextFileRemoveTrackers;



  //Check is program is started as console
  ConsoleMode;

  //Update some captions
  Caption := FORM_CAPTION;
  GroupBoxTorrentContents.Caption := TORRENT_FILES_CONTENTS_FORM_CAPTION;
  GroupBoxPresentTracker.Caption := GROUPBOX_PRESENT_TRACKERS_CAPTION;

end;

procedure TFormTrackerModify.FormDestroy(Sender: TObject);
begin
  //The program is being closed. Free all the memory.
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

procedure TFormTrackerModify.MenuItemOnlineCheckAppendStableTrackersClick(
  Sender: TObject);
var
  tracker: UTF8String;
begin
  //User want to use the downloaded tracker list.

  //check if tracker is already downloaded
  if not FDownloadStatus then
  begin
    //Download it now.
    MenuItemOnlineCheckDownloadNewTrackonClick(nil);
  end;

  //Append all the trackers to MemoNewTrackers
  MemoNewTrackers.Lines.BeginUpdate;
  for Tracker in FControlerTrackerListOnline.StableTrackers do
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

procedure TFormTrackerModify.MenuItemOnlineCheckDownloadNewTrackonClick(
  Sender: TObject);
begin
  try
    screen.Cursor := crHourGlass;
    FDownloadStatus := FControlerTrackerListOnline.DownloadTrackers;
  finally
    screen.Cursor := crDefault;
  end;

  if not FDownloadStatus then
  begin
    //something is wrong with downloading
    Application.MessageBox(
      PChar('Can not downloading the trackers from internet'),
      '', MB_ICONINFORMATION + MB_OK);
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

procedure TFormTrackerModify.MenuItemTorrentFilesTreeHideAllClick(Sender: TObject);
var
  i, CountTorrents: integer;
begin
  //Show only torrent file names

  //user what to hide all the items.
  //All the popup menu item must first be unchecked.
  MenuItemTorrentFilesTreeShowInfo.Checked := False;
  MenuItemTorrentFilesTreeShowFiles.Checked := False;
  MenuItemTorrentFilesTreeShowTrackers.Checked := False;
  //Update the TorrentFilesTree
  //  MenuItemTorrentFilesTreeSyncWithPopupMenu;

  if not assigned(FTreeNodeRoot) then
    exit;

  //how many torrent files are there.
  CountTorrents := FTreeNodeRoot.Count;
  if CountTorrents = 0 then
    exit;

  //Show the torrent files names only.
  for i := 0 to CountTorrents - 1 do
  begin
    FTreeNodeRoot.Items[i].Collapse(True);
  end;

end;

procedure TFormTrackerModify.MenuItemTorrentFilesTreeShowAllClick(Sender: TObject);
begin
  //show everything
  if assigned(FTreeNodeRoot) then
    FTreeNodeRoot.Expand(True);

  //user what to see all the items.
  //All the popup menu item must first be checked.
  MenuItemTorrentFilesTreeShowInfo.Checked := True;
  MenuItemTorrentFilesTreeShowFiles.Checked := True;
  MenuItemTorrentFilesTreeShowTrackers.Checked := True;
  //Update the TorrentFilesTree
  //  MenuItemTorrentFilesTreeSyncWithPopupMenu;
end;

procedure TFormTrackerModify.MenuItemTorrentFilesTreeShowOrHideItemClick(
  Sender: TObject);
var
  i, CountTorrents, itemsNr: integer;
  ShowNode: boolean;
begin
  //Show or hide all the items below the torrent files.

  //Get the top node.
  if not assigned(FTreeNodeRoot) then
    exit;

  //how many torrent files are there.
  CountTorrents := FTreeNodeRoot.Count;
  if CountTorrents = 0 then
    exit;

  //The tag number define if it is for files, trackers or info items
  itemsNr := TMenuItem(Sender).tag;

  //Must show or hide the items
  ShowNode := TMenuItem(Sender).Checked;

  //process all the torrent files one by one.
  for i := 0 to CountTorrents - 1 do
  begin
    if ShowNode then
    begin
      FTreeNodeRoot.Items[i].Expand(False); //Show the torrent name + child
      FTreeNodeRoot.Items[i].Items[itemsNr].Expand(False); //expand child
    end
    else
    begin
      FTreeNodeRoot.Items[i].Items[itemsNr].Collapse(False);
    end;
  end;
end;

procedure TFormTrackerModify.UpdateTorrent;
var
  Reply, BoxStyle, i, CountTrackers: integer;
  PopUpMenuStr: string;
  SomeFilesCannotBeWriten, SomeFilesAreReadOnly: boolean;

begin
  //Update all the torrent files.

  //The StringGridTorrentData where the comment are place by user
  //    must be in sync again with FTrackerList.TorrentFileNameList.
  //Undo all posible sort column used by the user. Sort it back to 'begin state'
  FControlerGridTorrentData.ReorderGrid;

  //initial value is false, will be set to true if some file fails to write
  SomeFilesCannotBeWriten := False;

  try

    if not FConcoleMode then
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
      if FConcoleMode then
      begin
        FTrackerList.LogStringList.Add('ERROR: No torrent file selected');
      end
      else
      begin
        Application.MessageBox('No torrent file selected',
          '', MB_ICONERROR);
      end;
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
    if not FConcoleMode and (CountTrackers = 0) then
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
        //if private torrent then make it public torrent by removing the private flag.
        if FDecodePresentTorrent.PrivateTorrent then
          FDecodePresentTorrent.RemovePrivateTorrentFlag;
      end
      else
      begin
        FDecodePresentTorrent.AddPrivateTorrentFlag;
      end;

      //update the comment item
      FDecodePresentTorrent.Comment := FControlerGridTorrentData.ReadComment(i + 1);

      //save the torrent file.
      if not FDecodePresentTorrent.SaveTorrent(FTrackerList.TorrentFileNameList[i]) then
      begin
        SomeFilesCannotBeWriten := True;
      end;

    end;//for

    //Create tracker.txt file
    SaveTrackerFinalListToFile;

    //Show/reload the just updated torrent files.
    ReloadAllTorrentAndRefreshView;

    //make sure cursor is default again
  finally
    ShowHourGlassCursor(False);
    ViewUpdateFormCaption;
  end;


  if FConcoleMode then
  begin
    //When succesfull the log file shows, 3 lines,
    //     OK + Count torrent files  + Count Trackers
    FTrackerList.LogStringList.Add('OK');
    FTrackerList.LogStringList.Add(IntToStr(FTrackerList.TorrentFileNameList.Count));
    FTrackerList.LogStringList.Add(IntToStr(CountTrackers));
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

procedure TFormTrackerModify.MenuItemTorrentFilesTreeSyncWithPopupMenu;
begin
  MenuItemTorrentFilesTreeShowOrHideItemClick(MenuItemTorrentFilesTreeShowTrackers);
  MenuItemTorrentFilesTreeShowOrHideItemClick(MenuItemTorrentFilesTreeShowInfo);
  MenuItemTorrentFilesTreeShowOrHideItemClick(MenuItemTorrentFilesTreeShowFiles);
end;


procedure TFormTrackerModify.SaveTrackerFinalListToFile;
var
  TrackerStr: UTF8String;
begin
  //Create the tracker text file. The old one will be overwritten
  AssignFile(FTrackerFile, ExtractFilePath(Application.ExeName) +
    EXPORT_TRACKERS_FILE_NAME);
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

procedure TFormTrackerModify.ConsoleMode;
var
  FileNameOrDirStr: UTF8String;
  StringList: TStringList;
begin
  //The first parameter[1] is path to file or dir.
  //if program is started with two parameter then in must be a console mode.

  //update the torrent via console mode if there is a parameter detected.
  if ParamCount > 0 then
  begin
    //there must be 2 command line items for console mode
    FConcoleMode := ParamCount = 2;

    try
      if FConcoleMode then
      begin
        //Create the log file. The old one will be overwritten
        AssignFile(FLogFile, ExtractFilePath(Application.ExeName) + LOG_FILE_NAME);
        ReWrite(FLogFile);
      end;

      //Get the startup command lime parameters.
      //ConsoleModeDecodeParameter(FileNameOrDirStr, FTrackerList);

      //If FTrackerList.LogStringList empty then there is no error.
      //    if FTrackerList.LogStringList.Text = '' then
      if ConsoleModeDecodeParameter(FileNameOrDirStr, FTrackerList) then
      begin
        //There is no error. Proceed with reading the torrent files

        if ExtractFileExt(FileNameOrDirStr) = '' then
        begin //There is no file extention. It must be a folder.
          if LoadTorrentViaDir(FileNameOrDirStr) then
          begin
            //Show all the tracker inside the torrent files.
            ShowTrackerInsideFileList;
            //Mark all trackers as selected
            CheckedOnOffAllTrackers(True);
            //Some tracker must be removed. Console and windows mode.
            UpdateViewRemoveTracker;

            if FConcoleMode then
            begin
              //update torrent
              UpdateTorrent;
            end;

          end;
        end
        else //a torrent file is selected?
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
                //Mark all trackers as selected
                CheckedOnOffAllTrackers(True);
                //Some tracker must be removed. Console and windows mode.
                UpdateViewRemoveTracker;

                if FConcoleMode then
                begin
                  //update torrent
                  UpdateTorrent;
                end;

              end;

            finally
              StringList.Free;
            end;
          end
          else
          begin //Error. this is not a torrent file
            FTrackerList.LogStringList.Add('ERROR: No torrent file selected.');
          end;
        end;
      end;

      if FConcoleMode then
      begin
        //Write to log file. And close the file.
        WriteLn(FLogFile, FTrackerList.LogStringList.Text);
        CloseFile(FLogFile);

        //Shutdown the console program
        Application.terminate;
      end;


    except
      if FConcoleMode then
      begin
        //Shutdown the console program.
        //This is needed or else the program will keep running forever.
        //exit with error code
        System.ExitCode := 1;
        Application.terminate;
      end
      else
      begin
        //show to the end user that something is wrong
        raise;
      end;

    end;
  end
  else
  begin //the program
    FConcoleMode := False;
  end;

end;



procedure TFormTrackerModify.UpdateViewRemoveTracker;
var
  TrackerStr: UTF8String;
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




function TFormTrackerModify.DecodeTorrentFile(const FileName: UTF8String): boolean;
begin
  //Called when user add torrent files
  //False if something is wrong with decoding torrent.
  Result := FDecodePresentTorrent.DecodeTorrent(FileName);
  if Result then
  begin
    ViewUpdateOneTorrentFileDecoded;
  end;
end;

procedure TFormTrackerModify.UpdateTorrentTrackerList;
var
  TrackerStr: UTF8String;
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
  //Set all the trackers checkbox ON or OFF
  if FControlerTrackerListOnline.Count > 0 then
  begin
    for i := 0 to FControlerTrackerListOnline.Count - 1 do
    begin
      FControlerTrackerListOnline.Checked[i] := Value;
    end;
  end;
end;


function TFormTrackerModify.CopyUserInputNewTrackersToList: boolean;
var
  TrackerStrLoop, TrackerStr: UTF8String;
begin
  {
   Called after 'update torrent' is selected.
   All the user entery from Memo text field will be add to FTrackerList.TrackerAddedByUserList.
  }
  FTrackerList.TrackerAddedByUserList.Clear;

  for TrackerStrLoop in MemoNewTrackers.Lines do
  begin
    TrackerStr := UTF8trim(TrackerStrLoop);

    //Skip empty line
    if TrackerStr = '' then
      continue;

    //All the tracker must begin with 'http(s)://' or 'udp://'
    if ValidTrackerURL(TrackerStr) then
    begin
      AddButIngnoreDuplicates(FTrackerList.TrackerAddedByUserList, TrackerStr);
    end
    else
    begin
      //There is error. Show the error and do not continue.
      if FConcoleMode then
      begin
        FTrackerList.LogStringList.Add(
          'ERROR: Tracker URL must begin with http:// or udp://');
      end
      else
      begin
        //Show error
        Application.MessageBox(PChar(@TrackerStr[1]),
          'Error: Tracker URL must begin with http(s):// or udp://', MB_ICONERROR);
      end;
      //do not continue with error.
      Result := False;
      exit;
    end;

  end;

  Result := True; //no error

  //Show the torrent list we have just created.
  MemoNewTrackers.Text := FTrackerList.TrackerAddedByUserList.Text;

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

procedure TFormTrackerModify.LoadTrackersTextFileAddTrackers;
var
  i: integer;
begin
  //Called at the start of the program. Load a trackers list from file

  //if no file is found the use the default tracker list.
  if not ReadAddTrackerFileFromUser(ExtractFilePath(Application.ExeName) +
    ADD_TRACKERS_FILE_NAME) then
  begin
    MemoNewTrackers.Lines.BeginUpdate;
    for i := low(RECOMENDED_TRACKERS) to high(RECOMENDED_TRACKERS) do
    begin
      MemoNewTrackers.Lines.Add(RECOMENDED_TRACKERS[i]);
    end;
    MemoNewTrackers.Lines.EndUpdate;
  end;

  //Check for error in tracker list
  if not CopyUserInputNewTrackersToList then
  begin
    MemoNewTrackers.Lines.Clear;
  end;
end;

procedure TFormTrackerModify.LoadTrackersTextFileRemoveTrackers;
var
  filename: UTF8String;
begin
  filename := ExtractFilePath(Application.ExeName) + REMOVE_TRACKERS_FILE_NAME;
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

  //Set all the trackers publick/private checkbox ON or OFF
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


function TFormTrackerModify.ReadAddTrackerFileFromUser(
  const FileName: UTF8String): boolean;
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



function TFormTrackerModify.LoadTorrentViaDir(const Dir: UTF8String): boolean;
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
  const FileNames: array of UTF8String);
var
  Count: integer;
  TorrentFileNameStringList, //for the torrent files
  TrackerFileNameStringList //for the trackers files
  : TStringList;

  TorrentFileSelectionDetected,

  //ViewUpdateBegin must be called one time. Keep track of it.
  ViewUpdateBeginActiveOneTimeOnly: boolean;

  FileNameOrDirStr: UTF8String;
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
  if FConcoleMode then
    Visible := False;
end;


function TFormTrackerModify.AddTorrentFileList(TorrentFileNameStringList:
  TStringList): boolean;
  //This called from 'add folder' or 'drag and drop'
var
  Count: integer;
  TorrentFileNameStr: UTF8String;
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
        if FConcoleMode then
        begin
          FTrackerList.LogStringList.Add('Error: Can not read torrent. ' +
            TorrentFileNameStr);
        end
        else
        begin
          Application.MessageBox(PChar(@TorrentFileNameStr[1]),
            'Error: Can not read torrent.', MB_ICONERROR);
        end;
        Result := False;
        exit;
      end;
    end;
  end;
  Result := True;
end;


procedure TFormTrackerModify.ReloadAllTorrentAndRefreshView;
var
  TorrentFileStr: UTF8String;
begin
{
  This is called after updating the torrent.
  We want to re-read the all torrent files.
  And show that everything is updated and OK
}

  ViewUpdateBegin;
  //Copy all the trackers in inside the torrent files to FTrackerList.TrackerFromInsideTorrentFilesList
  FTrackerList.TrackerFromInsideTorrentFilesList.Clear;
  for TorrentFileStr in FTrackerList.TorrentFileNameList do
  begin
    if DecodeTorrentFile(TorrentFileStr) then
    begin
      UpdateTorrentTrackerList;
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


procedure TFormTrackerModify.ViewUpdateBegin(ClearView: boolean);
begin
  //Called before loading torrent file.
  FTotalFileInsideTorrent := 0;
  FTotalFileSizeInsideTorrent := 0;

  //Do not show being updating till finish updating data.
  StringGridTorrentData.BeginUpdate;
  TreeViewFileContents.BeginUpdate;
  CheckListBoxPublicPrivateTorrent.Items.BeginUpdate;

  if ClearView then
  begin
    //Clear all the user data 'View' elements. This will be filled with new data.
    TreeViewFileContents.Items.Clear;
    CheckListBoxPublicPrivateTorrent.Clear; //Use in update torrent!
    StringGridTorrentData.Clear;
    FControlerGridTorrentData.ClearAllImageIndex;
    //RowCount is 0 after Clear. But must be 1 to make it work.
    StringGridTorrentData.RowCount := 1;
  end;

  //root is 'Torrent Files'
  FTreeNodeRoot := TreeViewFileContents.Items.Add(nil, 'Torrent Files');

end;

procedure TFormTrackerModify.ViewUpdateOneTorrentFileDecoded;
var
  RowIndex, CountFiles: integer;
  TorrentFileNameStr, TrackerStr, PrivateStr: UTF8String;
  DateTimeStr: string;
  TreeNodeTorrent, TreeNodeFiles, TreeNodeTrackers, TreeNodeInfo: TTreeNode;
begin
  //Called after loading torrent file.


  TorrentFileNameStr := ExtractFileName(FDecodePresentTorrent.FilenameTorrent);

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
  FControlerGridTorrentData.PieceLength :=
    format('%6d', [FDecodePresentTorrent.PieceLenght div 1024]); //Show as KiBytes
  FControlerGridTorrentData.TotaSize :=
    format('%9d', [FDecodePresentTorrent.TotalFileSize div 1024]); //Show as KiBytes
  FControlerGridTorrentData.IndexOrder :=
    format('%6d', [StringGridTorrentData.RowCount - 1]);
  //Must keep track of order when sorted back

  //All the string data are filed. Copy it now to the grid
  FControlerGridTorrentData.AppendRow;

  //---------------------  Fill the treeview with torrent files

  //Add the torrent file name + size of all the files combined.
  TorrentFileNameStr := TorrentFileNameStr + '     SIZE: ' +
    ByteSizeToBiggerSizeFormatStr(FDecodePresentTorrent.TotalFileSize) +
    '  Files: ' + IntToStr(FDecodePresentTorrent.InfoFilesCount) +
    '' + '  Tracker: ' + IntToStr(FDecodePresentTorrent.TrackerList.Count) + '';


  TreeNodeTorrent := TreeViewFileContents.Items.AddChild(FTreeNodeRoot,
    //FTrackerList.TorrentFileNameList[RowIndex]); //With directory path
    TorrentFileNameStr);  //Without directory  path

  TreeNodeFiles := TreeViewFileContents.Items.AddChild(TreeNodeTorrent, 'Files');
  TreeNodeTrackers := TreeViewFileContents.Items.AddChild(TreeNodeTorrent,
    'Trackers');
  TreeNodeInfo := TreeViewFileContents.Items.AddChild(TreeNodeTorrent, 'Info');

  //Show all the files inside the torrent
  if FDecodePresentTorrent.InfoFilesCount > 0 then
  begin
    for CountFiles := 0 to FDecodePresentTorrent.InfoFilesCount - 1 do
    begin
      TreeViewFileContents.Items.AddChild(TreeNodeFiles,
        FDecodePresentTorrent.InfoFilesNameIndex(CountFiles) +
        '     SIZE: ' + ByteSizeToBiggerSizeFormatStr(
        FDecodePresentTorrent.InfoFilesLengthIndex(CountFiles)));
    end;
  end;

  //Show a how many files are there
  TreeNodeFiles.Text := TreeNodeFiles.Text + ' (' + IntToStr(TreeNodeFiles.Count) + ')';

  //Show all the trackers inside the torrent
  for TrackerStr in FDecodePresentTorrent.TrackerList do
  begin
    TreeViewFileContents.Items.AddChild(TreeNodeTrackers, TrackerStr);
  end;

  //Show a how many trackers are there
  TreeNodeTrackers.Text := TreeNodeTrackers.Text + ' (' +
    IntToStr(TreeNodeTrackers.Count) + ')';


  //Show all the info of torrent
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Name: ' +
    FDecodePresentTorrent.Name);
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Comment: ' +
    FDecodePresentTorrent.Comment);
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Info Hash: ' +
    FDecodePresentTorrent.InfoHash);
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Created On: ' + DateTimeStr);
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Created By: ' +
    FDecodePresentTorrent.CreatedBy);
  TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Piece Lenght: ' +
    IntToStr(FDecodePresentTorrent.PieceLenght div 1024) + ' KiB');
  if FDecodePresentTorrent.PrivateTorrent then
  begin
    TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Private: yes');
  end
  else
  begin
    TreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Private: no');
  end;

  //All the files count inside the torrent must be added to FTotalFileInsideTorrent
  Inc(FTotalFileInsideTorrent, FDecodePresentTorrent.InfoFilesCount);

  //The file size of all files inside the torrent must be added to FTotalFileSizeInsideTorrent
  Inc(FTotalFileSizeInsideTorrent, FDecodePresentTorrent.TotalFileSize);

end;



procedure TFormTrackerModify.ViewUpdateEnd;
begin

  //Called after loading torrent file
  //Sync the popup menu with show/hide items.
  MenuItemTorrentFilesTreeSyncWithPopupMenu;


  //Show what we have updated.
  TreeViewFileContents.EndUpdate;
  StringGridTorrentData.EndUpdate;
  CheckListBoxPublicPrivateTorrent.Items.EndUpdate;


  //Show the size of all the files inside the torrent
  //http://en.wikipedia.org/wiki/Gigabyte
  GroupBoxTorrentContents.Caption :=
    TORRENT_FILES_CONTENTS_FORM_CAPTION + ' (Files count: ' +
    IntToStr(FTotalFileInsideTorrent) + ') Files sizes: ' +
    ByteSizeToBiggerSizeFormatStr(FTotalFileSizeInsideTorrent) + '';


  GroupBoxPresentTracker.Caption :=
    GROUPBOX_PRESENT_TRACKERS_CAPTION + ' (List count: ' +
    IntToStr(FTrackerList.TrackerFromInsideTorrentFilesList.Count) + ' )';



  //Show all the tracker inside the torrent files.
  ShowTrackerInsideFileList;
  //Mark all trackers as selected
  CheckedOnOffAllTrackers(True);
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

end.
