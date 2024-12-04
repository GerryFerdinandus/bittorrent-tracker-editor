// SPDX-License-Identifier: MIT
unit controler_treeview_torrent_data;

{
 Show the torrent files content in a tree view format.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Menus, StdCtrls, DecodeTorrent;

type

  { Tcontroler_treeview_torrent_data }
  Tcontroler_treeview_torrent_data_items = (
    ctv_ShowAllFiles = 0,
    ctv_ShowAllTrackers = 1,
    ctv_ShowAllInfo = 2,
    ctv_ShowEverything,
    ctv_HideAll
    );

  Tcontroler_treeview_torrent_data = class
  private
    FTotalFileInsideTorrent: integer;
    FTotalFileSizeInsideTorrent: int64;

    FGroupBoxTorrentContents: TGroupBox;
    FPopupMenuTorrentFilesContent: TPopupMenu;
    FOwner: TWinControl;
    FTreeViewFileContents: TTreeView;
    FTreeNodeRoot: TTreeNode;
    FMenuItemTorrentFilesTreeHideAll: TMenuItem;
    FMenuItemTorrentFilesTreeShowTrackers: TMenuItem;
    FMenuItemTorrentFilesTreeShowInfo: TMenuItem;
    FMenuItemTorrentFilesTreeShowAll: TMenuItem;
    FMenuItemTorrentFilesTreeShowFiles: TMenuItem;
    procedure FillThePopupMenu;
    procedure AddMenuItem(var MenuItem: TMenuItem; Onclick: TNotifyEvent;
      const Caption: string; tag: integer);
    procedure MenuItemTorrentFilesTreeShowOrHideItemClick(Sender: TObject);
    procedure MenuItemTorrentFilesTreeHideAllClick(Sender: TObject);
    procedure MenuItemTorrentFilesTreeShowAllClick(Sender: TObject);
    procedure Clear;
    procedure MenuItemTorrentFilesTreeSyncWithPopupMenu;
  public
    //called before AddOneTorrentFileDecoded
    procedure BeginUpdate;

    //called after AddOneTorrentFileDecoded
    procedure EndUpdate;

    //Add every torrent one by one to the list
    procedure AddOneTorrentFileDecoded(DecodeTorrent: TDecodeTorrent);

    constructor Create(Owner: TWinControl);

    destructor Destroy; override;
  end;



implementation

uses torrent_miscellaneous;

const
  TORRENT_FILES_CONTENTS_FORM_CAPTION =
    'Show all the files inside the torrents. (Use right mouse for popup menu.)';

  { Tcontroler_treeview_torrent_data }

procedure Tcontroler_treeview_torrent_data.FillThePopupMenu;
begin
  FPopupMenuTorrentFilesContent := TPopupMenu.Create(FTreeViewFileContents);
  FTreeViewFileContents.PopupMenu := FPopupMenuTorrentFilesContent;

  //There are 5 menu items the user can select/click on
  //Create all these 5 menu items.

  //'Show Everything'
  AddMenuItem(FMenuItemTorrentFilesTreeShowAll, @MenuItemTorrentFilesTreeShowAllClick,
    'Show Everything', Ord(ctv_ShowEverything));//-1

  //'Hide All'
  AddMenuItem(FMenuItemTorrentFilesTreeHideAll, @MenuItemTorrentFilesTreeHideAllClick,
    'Hide All', Ord(ctv_HideAll));//0

  //'Show All Files'
  AddMenuItem(FMenuItemTorrentFilesTreeShowFiles,
    @MenuItemTorrentFilesTreeShowOrHideItemClick, 'Show All Files',
    Ord(ctv_ShowAllFiles));
  FMenuItemTorrentFilesTreeShowFiles.AutoCheck := True;
  FMenuItemTorrentFilesTreeShowFiles.Checked := True;

  //'Show All Trackers'
  AddMenuItem(FMenuItemTorrentFilesTreeShowTrackers,
    @MenuItemTorrentFilesTreeShowOrHideItemClick, 'Show All Trackers',
    Ord(ctv_ShowAllTrackers));
  FMenuItemTorrentFilesTreeShowTrackers.AutoCheck := True;
  FMenuItemTorrentFilesTreeShowTrackers.Checked := True;

  //'Show All Info'
  AddMenuItem(FMenuItemTorrentFilesTreeShowInfo,
    @MenuItemTorrentFilesTreeShowOrHideItemClick, 'Show All Info', Ord(ctv_ShowAllInfo));
  FMenuItemTorrentFilesTreeShowInfo.AutoCheck := True;
  FMenuItemTorrentFilesTreeShowInfo.Checked := True;

end;

procedure Tcontroler_treeview_torrent_data.AddMenuItem(var MenuItem: TMenuItem;
  Onclick: TNotifyEvent; const Caption: string; tag: integer);
begin
  MenuItem := TMenuItem.Create(FPopupMenuTorrentFilesContent);
  MenuItem.OnClick := Onclick;
  MenuItem.Caption := Caption;
  MenuItem.Tag := tag;
  FPopupMenuTorrentFilesContent.Items.Add(MenuItem);
end;

constructor Tcontroler_treeview_torrent_data.Create(Owner: TWinControl);
begin
  inherited Create;
  FOwner := Owner;

  //create TGroupBox
  FGroupBoxTorrentContents := TGroupBox.Create(FOwner);
  FGroupBoxTorrentContents.Parent := FOwner;
  FGroupBoxTorrentContents.Align := alClient;
  FGroupBoxTorrentContents.Caption := TORRENT_FILES_CONTENTS_FORM_CAPTION;

  //create TTreeView
  FTreeViewFileContents := TTreeView.Create(FGroupBoxTorrentContents);
  FTreeViewFileContents.Parent := FGroupBoxTorrentContents;
  FTreeViewFileContents.Align := alClient;
  FTreeViewFileContents.ReadOnly := True;
  FTreeViewFileContents.ShowRoot := False;
  FTreeViewFileContents.Options :=
    FTreeViewFileContents.Options + [tvoAutoExpand, tvoReadOnly] - [tvoShowRoot];

  //create popupmenu
  FillThePopupMenu;

end;

destructor Tcontroler_treeview_torrent_data.Destroy;
begin
  inherited Destroy;
end;


procedure Tcontroler_treeview_torrent_data.MenuItemTorrentFilesTreeHideAllClick(
  Sender: TObject);
var
  i, CountTorrents: integer;
begin
  //hide all -> Show only torrent file names

  //user what to hide all the items.
  //All the popup menu item must first be unchecked.
  FMenuItemTorrentFilesTreeShowInfo.Checked := False;
  FMenuItemTorrentFilesTreeShowFiles.Checked := False;
  FMenuItemTorrentFilesTreeShowTrackers.Checked := False;
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

procedure Tcontroler_treeview_torrent_data.MenuItemTorrentFilesTreeShowAllClick(
  Sender: TObject);
begin
  //show everything
  if assigned(FTreeNodeRoot) then
    FTreeNodeRoot.Expand(True);

  //user what to see all the items.
  //All the popup menu item must first be checked.
  FMenuItemTorrentFilesTreeShowInfo.Checked := True;
  FMenuItemTorrentFilesTreeShowFiles.Checked := True;
  FMenuItemTorrentFilesTreeShowTrackers.Checked := True;
  //Update the TorrentFilesTree
  //  MenuItemTorrentFilesTreeSyncWithPopupMenu;
end;

procedure Tcontroler_treeview_torrent_data.Clear;
begin
  FTreeViewFileContents.Items.Clear;
  FTreeNodeRoot := FTreeViewFileContents.Items.Add(nil, 'Torrent Files');
  FTotalFileInsideTorrent := 0;
  FTotalFileSizeInsideTorrent := 0;
end;

procedure Tcontroler_treeview_torrent_data.MenuItemTorrentFilesTreeSyncWithPopupMenu;
begin
  MenuItemTorrentFilesTreeShowOrHideItemClick(FMenuItemTorrentFilesTreeShowTrackers);
  MenuItemTorrentFilesTreeShowOrHideItemClick(FMenuItemTorrentFilesTreeShowInfo);
  MenuItemTorrentFilesTreeShowOrHideItemClick(FMenuItemTorrentFilesTreeShowFiles);
end;

procedure Tcontroler_treeview_torrent_data.BeginUpdate;
begin
  //Called before loading torrent file.

  //do not update the view
  FTreeViewFileContents.BeginUpdate;

  //Clear everything before adding new torrent files
  Clear;
end;

procedure Tcontroler_treeview_torrent_data.EndUpdate;
begin
  //The view can be updated again
  FTreeViewFileContents.EndUpdate;

  //Update the text about howmany files are there etc.
  //Show the size of all the files inside the torrent
  //http://en.wikipedia.org/wiki/Gigabyte
  FGroupBoxTorrentContents.Caption :=
    TORRENT_FILES_CONTENTS_FORM_CAPTION + ' (Files count: ' +
    IntToStr(FTotalFileInsideTorrent) + ') Files sizes: ' +
    ByteSizeToBiggerSizeFormatStr(FTotalFileSizeInsideTorrent) + '';

  //Sync the popup menu with show/hide items.
  MenuItemTorrentFilesTreeSyncWithPopupMenu;

end;


procedure Tcontroler_treeview_torrent_data.MenuItemTorrentFilesTreeShowOrHideItemClick(
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

procedure Tcontroler_treeview_torrent_data.AddOneTorrentFileDecoded(
  DecodeTorrent: TDecodeTorrent);
var
  CountFiles: integer;
  TorrentFileNameStr, TrackerStr: utf8string;
  TreeNodeTorrent, TreeNodeFiles, TreeNodeTrackers, TreeNodeInfo: TTreeNode;
begin
  //---------------------  Fill the treeview with torrent files

  TorrentFileNameStr := ExtractFileName(DecodeTorrent.FilenameTorrent);

  //Add the torrent file name + size of all the files combined.
  TorrentFileNameStr := TorrentFileNameStr + '  (Version: ' +
    DecodeTorrent.TorrentVersionToString + ') SIZE: ' +
    ByteSizeToBiggerSizeFormatStr(DecodeTorrent.TotalFileSize) +
    '  Files: ' + IntToStr(DecodeTorrent.InfoFilesCount) + '' +
    '  Tracker: ' + IntToStr(DecodeTorrent.TrackerList.Count) + '';


  TreeNodeTorrent := FTreeViewFileContents.Items.AddChild(FTreeNodeRoot,
    //FTrackerList.TorrentFileNameList[RowIndex]); //With directory path
    TorrentFileNameStr);  //Without directory  path

  //must be in this order (Files, Trackers, Info)
  TreeNodeFiles := FTreeViewFileContents.Items.AddChild(TreeNodeTorrent,
    'Files V' + IntToStr(DecodeTorrent.InfoFilesVersion));

  TreeNodeTrackers := FTreeViewFileContents.Items.AddChild(TreeNodeTorrent,
    'Trackers');
  TreeNodeInfo := FTreeViewFileContents.Items.AddChild(TreeNodeTorrent, 'Info');

  //Show all the files inside the torrent
  if DecodeTorrent.InfoFilesCount > 0 then
  begin
    for CountFiles := 0 to DecodeTorrent.InfoFilesCount - 1 do
    begin
      FTreeViewFileContents.Items.AddChild(TreeNodeFiles,
        DecodeTorrent.InfoFilesNameIndex(CountFiles) + '     SIZE: ' +
        ByteSizeToBiggerSizeFormatStr(DecodeTorrent.InfoFilesLengthIndex(CountFiles)));
    end;
  end;

  //Show a how many files are there
  TreeNodeFiles.Text := TreeNodeFiles.Text + ' (' + IntToStr(TreeNodeFiles.Count) + ')';

  //Show all the trackers inside the torrent
  for TrackerStr in DecodeTorrent.TrackerList do
  begin
    FTreeViewFileContents.Items.AddChild(TreeNodeTrackers, TrackerStr);
  end;

  //Show a how many trackers are there
  TreeNodeTrackers.Text := TreeNodeTrackers.Text + ' (' +
    IntToStr(TreeNodeTrackers.Count) + ')';


  //Show all the info of torrent
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Name: ' + DecodeTorrent.Name);

  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Comment: ' +
    DecodeTorrent.Comment);
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Info Hash V1: ' +
    DecodeTorrent.InfoHash_V1);
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Info Hash V2: ' +
    DecodeTorrent.InfoHash_V2);
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Created On: ' +
    DateTimeToStr(DecodeTorrent.CreatedDate));
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Created By: ' +
    DecodeTorrent.CreatedBy);
  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Piece Lenght: ' +
    IntToStr(DecodeTorrent.PieceLenght div 1024) + ' KiB');
  if DecodeTorrent.PrivateTorrent then
  begin
    FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Private: yes');
  end
  else
  begin
    FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Private: no');
  end;
  // some private torrent have info:source
  if DecodeTorrent.InfoSource <> '' then
  begin
    FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Source: ' +
      DecodeTorrent.InfoSource);
  end;

  if DecodeTorrent.MetaVersion > 0 then
  begin // 'meta version'is in torrent file present
    FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Meta Version: ' +
      IntToStr(DecodeTorrent.MetaVersion));
  end;

  FTreeViewFileContents.Items.AddChild(TreeNodeInfo, 'Padding (beb 47): ' +
    DecodeTorrent.PaddingToString);

  //All the files count inside the torrent must be added to FTotalFileInsideTorrent
  Inc(FTotalFileInsideTorrent, DecodeTorrent.InfoFilesCount);

  //The file size of all files inside the torrent must be added to FTotalFileSizeInsideTorrent
  Inc(FTotalFileSizeInsideTorrent, DecodeTorrent.TotalFileSize);

end;



end.
