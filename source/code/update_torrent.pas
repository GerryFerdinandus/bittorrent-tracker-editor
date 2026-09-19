// SPDX-License-Identifier: MIT
unit update_torrent;

{
 Write the new tracker list and the other user settings into the torrent files.

 This is the part of 'update torrent' that has no user interface. Everything the
 user can change per torrent file is handed over as TTorrentFileSettingArray, so
 this unit can be tested without a form.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DecodeTorrent, torrent_miscellaneous;

type
  //The settings the user can change for one single torrent file.
  TTorrentFileSetting = record
    //False will add the 'info.private' flag to the torrent.
    PublicTorrent: boolean;
    Comment: UTF8String;
  end;

  //One item for every item in TTrackerList.TorrentFileNameList, same order.
  TTorrentFileSettingArray = array of TTorrentFileSetting;

  TUpdateTorrentResult = record
    //Trackers written into the last updated torrent file.
    TrackerCount: integer;

    //Torrent files that are saved without error.
    FilesUpdated: integer;

    SomeFilesAreReadOnly: boolean;
    SomeFilesCannotBeWritten: boolean;
    SomeFilesCanNotBeDecoded: boolean;
  end;

{
 Update every torrent file in TrackerList.TorrentFileNameList.

 TrackerList.TrackerFinalList must already be combined by the caller with tloSort.
 Its count is used as tracker count when no torrent file is updated at all.
}
function UpdateTorrentFileList(var TrackerList: TTrackerList;
  DecodeTorrent: TDecodeTorrent;
  const FileSettingList: TTorrentFileSettingArray): TUpdateTorrentResult;

implementation

procedure WriteAnnounce(DecodeTorrent: TDecodeTorrent; TrackerFinalList: TStringList);
begin
  case TrackerFinalList.Count of
    0://if no tracker selected then delete 'announce' and 'announce-list'
    begin
      DecodeTorrent.RemoveAnnounce;
      DecodeTorrent.RemoveAnnounceList;
    end;
    1://if one tracker selected then delete 'announce-list'
    begin
      //Announce use the only tracker present in the TrackerFinalList. index 0
      DecodeTorrent.ChangeAnnounce(TrackerFinalList[0]);
      DecodeTorrent.RemoveAnnounceList;
    end;
    else//More than 1 trackers selected. Create 'announce-list'
    begin
      //Announce use the first tracker from the list. index 0
      DecodeTorrent.ChangeAnnounce(TrackerFinalList[0]);
      DecodeTorrent.ChangeAnnounceList(TrackerFinalList);
    end;
  end;
end;

procedure WritePrivateTorrentFlag(DecodeTorrent: TDecodeTorrent;
  PublicTorrent: boolean);
begin
  if PublicTorrent then
  begin
    //if private torrent then make it public torrent by removing the private flag.
    if DecodeTorrent.PrivateTorrent then
      DecodeTorrent.RemovePrivateTorrentFlag;
  end
  else
  begin
    DecodeTorrent.AddPrivateTorrentFlag;
  end;
end;

procedure WriteInfoSource(DecodeTorrent: TDecodeTorrent;
  const TrackerList: TTrackerList);
begin
  //Update the source tag for private trackers
  if TrackerList.RemoveAllSourceTag then
  begin
    // This will delete info:source item
    DecodeTorrent.InfoSourceRemove;
  end
  else
  begin
    // Copy the new source tag, but it must not be empty.
    // Empty TrackerList.SourceTag is the same as do not change anything.
    if TrackerList.SourceTag <> '' then
      DecodeTorrent.InfoSourceAdd(TrackerList.SourceTag);
  end;
end;

function UpdateTorrentFileList(var TrackerList: TTrackerList;
  DecodeTorrent: TDecodeTorrent;
  const FileSettingList: TTorrentFileSettingArray): TUpdateTorrentResult;
var
  i: integer;
begin
  //The count of the tloSort combined list, used when no torrent file is updated.
  Result.TrackerCount := TrackerList.TrackerFinalList.Count;
  Result.FilesUpdated := 0;
  Result.SomeFilesAreReadOnly := False;
  Result.SomeFilesCannotBeWritten := False;
  Result.SomeFilesCanNotBeDecoded := False;

  Assert(Length(FileSettingList) = TrackerList.TorrentFileNameList.Count,
    'Every torrent file must have one TTorrentFileSetting');

  if TrackerList.TrackerListOrderForUpdatedTorrent = tloRandomize then
    Randomize;

  for i := 0 to TrackerList.TorrentFileNameList.Count - 1 do
  begin
    //check for read only files. It can not be updated by tracker editor
    if (FileGetAttr(TrackerList.TorrentFileNameList[i]) and faReadOnly) <> 0 then
    begin
      Result.SomeFilesAreReadOnly := True;
      Continue;
    end;

    //read one torrent file. If error then skip it, but report it.
    if not DecodeTorrent.DecodeTorrent(TrackerList.TorrentFileNameList[i]) then
    begin
      Result.SomeFilesCanNotBeDecoded := True;
      Continue;
    end;

    //tloSort is already combined by the caller. All other modes are per torrent file.
    if TrackerList.TrackerListOrderForUpdatedTorrent <> tloSort then
    begin
      //Add the new tracker before or after the original trackers inside the torrent.
      CombineFiveTrackerListToOne(TrackerList.TrackerListOrderForUpdatedTorrent,
        TrackerList, DecodeTorrent.TrackerList);

      Result.TrackerCount := TrackerList.TrackerFinalList.Count;
    end;

    WriteAnnounce(DecodeTorrent, TrackerList.TrackerFinalList);
    WritePrivateTorrentFlag(DecodeTorrent, FileSettingList[i].PublicTorrent);
    DecodeTorrent.Comment := FileSettingList[i].Comment;
    WriteInfoSource(DecodeTorrent, TrackerList);

    if DecodeTorrent.SaveTorrent(TrackerList.TorrentFileNameList[i]) then
      Inc(Result.FilesUpdated)
    else
      Result.SomeFilesCannotBeWritten := True;
  end;
end;

end.
