// SPDX-License-Identifier: MIT
unit main_common;

{
 Headless console-mode engine, shared by the GUI (trackereditor, console-mode branch)
 and the console-only program (trackereditor_cli). No Forms/Controls/Grids allowed here.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DecodeTorrent, torrent_miscellaneous, update_torrent;

const
  //Used when add_trackers.txt is missing or can not be read.
  RECOMMENDED_TRACKERS: array[0..2] of UTF8String =
    (
    'udp://tracker.coppersurfer.tk:6969/announce',
    'udp://tracker.opentrackr.org:1337/announce',
    'wss://tracker.openwebtorrent.com'
    );

//Resolve the folder used to load/save add_trackers.txt, remove_trackers.txt, etc.
//Mirrors the Snap/Flatpak/AppImage/macOS/default rules from the GUI's FormCreate.
function DetermineTrackerListFolder(const ExeFileName: string): string;

//Creates all the lists of TrackerList. Release them with FreeTrackerList.
procedure CreateTrackerList(out TrackerList: TTrackerList);
procedure FreeTrackerList(var TrackerList: TTrackerList);

//Loads a tracker list text file into Lines. False, and Lines unchanged, when the file is unreadable.
function ReadAddTrackersFile(const FileName: string; Lines: TStrings): boolean;

//Loads add_trackers.txt from Folder into Lines, or the recommended trackers when there is no file.
procedure LoadAddTrackersRaw(const Folder: string; Lines: TStrings);

//Loads remove_trackers.txt from Folder into TrackerList.TrackerBanByUserList.
//False when the file exists but can not be read. It is then treated as not present.
//A missing file is not an error. Invalid URLs inside the file are not an error either (by design).
function LoadRemoveTrackers(const Folder: string; var TrackerList: TTrackerList;
  out FilePresentBanByUserList: boolean): boolean;

//Writes the export_trackers.txt file, one tracker group per URL.
procedure SaveTrackerFinalListToFile(const Folder: string; TrackerFinalList: TStringList);

//Same as SaveTrackerFinalListToFile, but False instead of an exception when the file can not be
//written (e.g. a read only program folder).
function TrySaveTrackerFinalListToFile(const Folder: string;
  TrackerFinalList: TStringList): boolean;

//Runs the full console pipeline (decodes ParamStr/ParamCount itself). Writes console_log.txt
//and export_trackers.txt into FolderForTrackerListLoadAndSave. Returns True on success.
function RunConsoleMode(const FolderForTrackerListLoadAndSave: string): boolean;

implementation

uses LazUTF8, LazFileUtils;

function DetermineTrackerListFolder(const ExeFileName: string): string;
begin
  Result := '';

  {$IFDEF LINUX}
  // If it is a Ubuntu snap program, save it a special folder
  Result := GetEnvironmentVariable('SNAP_USER_COMMON');
  // If it is a flatpak program, save it in a special folder
  if GetEnvironmentVariable('container') = 'flatpak' then
  begin
    Result := GetEnvironmentVariable('XDG_DATA_HOME');
  end;
  // If it is a appimage program, save it in a present folder.
  if GetEnvironmentVariable('APPIMAGE') <> '' then
  begin // OWD = Path to working directory at the time the AppImage is called
    Result := GetEnvironmentVariable('OWD');
  end;
  {$ENDIF LINUX}

  {$IFDEF DARWIN}
  // PATH: ~/.config/trackereditor/
  Result := GetAppConfigDir(False);
  if not DirectoryExists(Result) then
    ForceDirectoriesUTF8(Result); // ~/.config may not exist yet
  {$ENDIF DARWIN}

  if Result = '' then
  begin
    // Default is to use the same place as the application file
    Result := ExtractFilePath(ExeFileName);
  end;

  // variable must have PathDelim
  Result := AppendPathDelim(Result);
end;

procedure CreateTrackerList(out TrackerList: TTrackerList);
begin
  TrackerList.TrackerListOrderForUpdatedTorrent := tloSort;

  TrackerList.TrackerAddedByUserList := TStringList.Create;
  TrackerList.TrackerAddedByUserList.Duplicates := dupIgnore;
  TrackerList.TrackerAddedByUserList.Sorted := False;

  TrackerList.TrackerBanByUserList := TStringList.Create;
  TrackerList.TrackerBanByUserList.Duplicates := dupIgnore;
  TrackerList.TrackerBanByUserList.Sorted := False;

  TrackerList.TrackerManuallyDeselectedByUserList := TStringList.Create;
  TrackerList.TrackerManuallyDeselectedByUserList.Duplicates := dupIgnore;
  TrackerList.TrackerManuallyDeselectedByUserList.Sorted := False;

  TrackerList.TrackerFromInsideTorrentFilesList := TStringList.Create;
  TrackerList.TrackerFromInsideTorrentFilesList.Duplicates := dupIgnore;
  TrackerList.TrackerFromInsideTorrentFilesList.Sorted := True;

  TrackerList.TrackerFromPrivateTorrentsList := TStringList.Create;
  TrackerList.TrackerFromPrivateTorrentsList.Duplicates := dupIgnore;
  TrackerList.TrackerFromPrivateTorrentsList.Sorted := True;

  TrackerList.TrackerFinalList := TStringList.Create;
  TrackerList.TrackerFinalList.Duplicates := dupIgnore;
  TrackerList.TrackerFinalList.Sorted := False;

  TrackerList.TorrentFileNameList := TStringList.Create;
  TrackerList.TorrentFileNameList.Duplicates := dupIgnore;
  TrackerList.TorrentFileNameList.Sorted := False;

  TrackerList.LogStringList := TStringList.Create;

  TrackerList.SkipAnnounceCheck := False;
  TrackerList.SourceTag := '';
  TrackerList.RemoveAllSourceTag := False;
end;

procedure FreeTrackerList(var TrackerList: TTrackerList);
begin
  TrackerList.TrackerAddedByUserList.Free;
  TrackerList.TrackerBanByUserList.Free;
  TrackerList.TrackerManuallyDeselectedByUserList.Free;
  TrackerList.TrackerFromInsideTorrentFilesList.Free;
  TrackerList.TrackerFromPrivateTorrentsList.Free;
  TrackerList.TrackerFinalList.Free;
  TrackerList.TorrentFileNameList.Free;
  TrackerList.LogStringList.Free;
end;

procedure LogConsoleError(var TrackerList: TTrackerList; const ErrorText: string;
  const FormText: string = '');
begin
  if FormText = '' then
    TrackerList.LogStringList.Add(ErrorText)
  else
    TrackerList.LogStringList.Add(FormText + ' : ' + ErrorText);
end;

//Validates RawLines (the console equivalent of the GUI's MemoNewTrackers.Lines) and rebuilds
//TrackerList.TrackerAddedByUserList from it. On success RawLines is rewritten sanitized.
function ValidateAndSanitizeTrackers(RawLines: TStringList; var TrackerList: TTrackerList;
  Temporary_SkipAnnounceCheck: boolean): boolean;
var
  ErrorStr, FailedTracker: UTF8String;
begin
  Result := ValidateNewTrackerLines(RawLines,
    TrackerList.SkipAnnounceCheck or Temporary_SkipAnnounceCheck,
    TrackerList.TrackerAddedByUserList, ErrorStr, FailedTracker);

  if Result then
    RawLines.Text := TrackerList.TrackerAddedByUserList.Text
  else
    LogConsoleError(TrackerList, ErrorStr, FailedTracker);
end;

function ReadAddTrackersFile(const FileName: string; Lines: TStrings): boolean;
var
  TrackerFileList: TStringList;
begin
  TrackerFileList := TStringList.Create;
  try
    try
      TrackerFileList.LoadFromFile(FileName);
      SanitizeTrackerList(TrackerFileList);
      Lines.Text := UTF8Trim(TrackerFileList.Text);
      Result := True;
    except
      //No file found, or unreadable.
      Result := False;
    end;
  finally
    TrackerFileList.Free;
  end;
end;

procedure LoadAddTrackersRaw(const Folder: string; Lines: TStrings);
var
  i: integer;
begin
  if not ReadAddTrackersFile(Folder + FILE_NAME_ADD_TRACKERS, Lines) then
  begin
    //Fall back to the recommended trackers.
    Lines.Clear;
    for i := low(RECOMMENDED_TRACKERS) to high(RECOMMENDED_TRACKERS) do
      Lines.Add(RECOMMENDED_TRACKERS[i]);
  end;
end;

function LoadRemoveTrackers(const Folder: string; var TrackerList: TTrackerList;
  out FilePresentBanByUserList: boolean): boolean;
var
  FileName: UTF8String;
begin
  Result := True;
  FileName := Folder + FILE_NAME_REMOVE_TRACKERS;
  try
    FilePresentBanByUserList := FileExistsUTF8(FileName);
    if FilePresentBanByUserList then
      TrackerList.TrackerBanByUserList.LoadFromFile(FileName);
  except
    //A half loaded or unreadable file must not remove anything.
    Result := False;
    FilePresentBanByUserList := False;
    TrackerList.TrackerBanByUserList.Clear;
  end;

  //By design the lines are NOT validated as tracker URLs, unlike add_trackers.txt.
  //A "remove" line only has to match a tracker, and the user may want to ban a malformed or
  //obsolete URL that is still inside a torrent. Rejecting the file because of one bad line
  //would block the valid removals. A line that matches nothing is silently skipped.
  SanitizeTrackerList(TrackerList.TrackerBanByUserList);
end;

//Decodes every torrent file in TorrentFileNameStringList, collecting the trackers found inside
//and one TTorrentFileSetting (public/private + comment) per file, same order as the file list.
function ConsoleDecodeTorrentFiles(TorrentFileNameStringList: TStringList;
  var TrackerList: TTrackerList; DecodeTorrentObj: TDecodeTorrent;
  var FileSettingList: TTorrentFileSettingArray): boolean;
var
  Count, SettingIndex: integer;
  TorrentFileNameStr: UTF8String;
begin
  Result := True;

  for Count := 0 to TorrentFileNameStringList.Count - 1 do
  begin
    TorrentFileNameStr := TorrentFileNameStringList[Count];

    if DecodeTorrentObj.DecodeTorrent(TorrentFileNameStr) then
    begin
      AddTorrentFileTrackers(DecodeTorrentObj.TrackerList, DecodeTorrentObj.PrivateTorrent,
        TrackerList);

      TrackerList.TorrentFileNameList.Add(TorrentFileNameStr);

      SettingIndex := Length(FileSettingList);
      SetLength(FileSettingList, SettingIndex + 1);
      //Public/private and comment are never edited in console mode - use the decoded originals.
      FileSettingList[SettingIndex].PublicTorrent := not DecodeTorrentObj.PrivateTorrent;
      FileSettingList[SettingIndex].Comment := DecodeTorrentObj.Comment;
    end
    else
    begin
      //Something is wrong. Can not decode torrent tracker item. Cancel everything.
      TrackerList.TorrentFileNameList.Clear;
      TrackerList.TrackerFromInsideTorrentFilesList.Clear;
      TrackerList.TrackerFromPrivateTorrentsList.Clear;
      SetLength(FileSettingList, 0);
      LogConsoleError(TrackerList, 'Error: Can not read torrent.', TorrentFileNameStr);
      Result := False;
      exit;
    end;
  end;
end;

function ConsoleDecodeTorrentFolder(const Dir: UTF8String; var TrackerList: TTrackerList;
  DecodeTorrentObj: TDecodeTorrent; var FileSettingList: TTorrentFileSettingArray): boolean;
var
  TorrentFilesNameStringList: TStringList;
begin
  TorrentFilesNameStringList := TStringList.Create;
  try
    torrent_miscellaneous.LoadTorrentViaDir(Dir, TorrentFilesNameStringList);
    Result := ConsoleDecodeTorrentFiles(TorrentFilesNameStringList, TrackerList,
      DecodeTorrentObj, FileSettingList);
  finally
    TorrentFilesNameStringList.Free;
  end;
end;

function ConsoleDecodeSingleTorrentFile(const FileName: UTF8String;
  var TrackerList: TTrackerList; DecodeTorrentObj: TDecodeTorrent;
  var FileSettingList: TTorrentFileSettingArray): boolean;
var
  TorrentFilesNameStringList: TStringList;
begin
  TorrentFilesNameStringList := TStringList.Create;
  try
    TorrentFilesNameStringList.Add(FileName);
    Result := ConsoleDecodeTorrentFiles(TorrentFilesNameStringList, TrackerList,
      DecodeTorrentObj, FileSettingList);
  finally
    TorrentFilesNameStringList.Free;
  end;
end;

//Console equivalent of the GUI's UpdateViewRemoveTracker, without any per-tracker Checked grid:
//CombineFiveTrackerListToOne already removes TrackerManuallyDeselectedByUserList unconditionally,
//so "remove everything already inside the torrent" only needs a straight list copy.
//False when the add list is rejected. The error is then already in the log.
function ApplyBanListRemoval(var TrackerList: TTrackerList; AddedTrackersRawList: TStringList;
  FilePresentBanByUserList: boolean): boolean;
begin
  Result := True;

  //'Remove nothing' modes (-U5, -U6) must never remove or uncheck any tracker.
  if (TrackerList.TrackerListOrderForUpdatedTorrent =
    tloInsertNewBeforeAndKeepOriginalIntactAndRemoveNothing) or
    (TrackerList.TrackerListOrderForUpdatedTorrent =
    tloAppendNewAfterAndKeepOriginalIntactAndRemoveNothing) then
    exit;

  //If file remove_trackers.txt is present but empty then remove all tracker inside torrent.
  if FilePresentBanByUserList and (UTF8Trim(TrackerList.TrackerBanByUserList.Text) = '') then
    TrackerList.TrackerManuallyDeselectedByUserList.Assign(
      TrackerList.TrackerFromInsideTorrentFilesList);

  if not ValidateAndSanitizeTrackers(AddedTrackersRawList, TrackerList, False) then
  begin
    Result := False;
    exit;
  end;

  //remove all the trackers that are ban from the user's 'add' list.
  RemoveTrackersFromList(TrackerList.TrackerBanByUserList, AddedTrackersRawList);

  Result := ValidateAndSanitizeTrackers(AddedTrackersRawList, TrackerList, False);
end;

procedure SaveTrackerFinalListToFile(const Folder: string; TrackerFinalList: TStringList);
var
  TrackerFile: TextFile;
  TrackerStr: UTF8String;
begin
  AssignFile(TrackerFile, Folder + FILE_NAME_EXPORT_TRACKERS);
  ReWrite(TrackerFile);
  try
    for TrackerStr in TrackerFinalList do
    begin
      WriteLn(TrackerFile, TrackerStr);
      //Every tracker must be a separate tracker group, one empty line between each.
      WriteLn(TrackerFile, '');
    end;
  finally
    CloseFile(TrackerFile);
  end;
end;

function TrySaveTrackerFinalListToFile(const Folder: string;
  TrackerFinalList: TStringList): boolean;
begin
  try
    SaveTrackerFinalListToFile(Folder, TrackerFinalList);
    Result := True;
  except
    Result := False;
  end;
end;

//Console equivalent of the GUI's UpdateTorrent (minus confirmation dialogs/view refresh).
procedure RunUpdateTorrentPipeline(var TrackerList: TTrackerList; DecodeTorrentObj: TDecodeTorrent;
  const FileSettingList: TTorrentFileSettingArray; AddedTrackersRawList: TStringList;
  const FolderForTrackerListLoadAndSave: string);
var
  CountTrackers: integer;
  UpdateResult: TUpdateTorrentResult;
begin
  if TrackerList.TorrentFileNameList.Count = 0 then
  begin
    LogConsoleError(TrackerList, 'ERROR: No torrent file selected');
    exit;
  end;

  //Must revalidate unconditionally: ApplyBanListRemoval skips validation for -U5/-U6.
  if not ValidateAndSanitizeTrackers(AddedTrackersRawList, TrackerList, False) then
    exit;

  //Must use 'sort' for correct initial FTrackerFinalList.Count
  CombineFiveTrackerListToOne(tloSort, TrackerList, DecodeTorrentObj.TrackerList);
  CountTrackers := TrackerList.TrackerFinalList.Count;

  UpdateResult := UpdateTorrentFileList(TrackerList, DecodeTorrentObj, FileSettingList);
  CountTrackers := UpdateResult.TrackerCount;

  SaveTrackerFinalListToFile(FolderForTrackerListLoadAndSave, TrackerList.TrackerFinalList);

  //Partial failures must not be reported as success.
  if UpdateResult.SomeFilesAreReadOnly then
    LogConsoleError(TrackerList, 'ERROR: Some torrent files are READ-ONLY and were not updated.');
  if UpdateResult.SomeFilesCannotBeWritten then
    LogConsoleError(TrackerList,
      'ERROR: Some torrent files failed to write and were not updated.');
  if UpdateResult.SomeFilesCanNotBeDecoded then
    LogConsoleError(TrackerList,
      'ERROR: Some torrent files could not be decoded and were skipped.');

  //if there is already an item inside there then there must be something wrong. Do not add 'OK'
  if TrackerList.LogStringList.Count = 0 then
  begin
    TrackerList.LogStringList.Add(CONSOLE_SUCCESS_STATUS);
    TrackerList.LogStringList.Add(IntToStr(TrackerList.TorrentFileNameList.Count));
    TrackerList.LogStringList.Add(IntToStr(CountTrackers));
  end;
end;

function RunConsoleMode(const FolderForTrackerListLoadAndSave: string): boolean;
var
  TrackerList: TTrackerList;
  DecodeTorrentObj: TDecodeTorrent;
  FileSettingList: TTorrentFileSettingArray;
  AddedTrackersRawList: TStringList;
  LogFile: TextFile;
  FileNameOrDirStr: UTF8String;
  FilePresentBanByUserList: boolean;
  MustExitWithErrorCode, LogFileIsOpen, AddTrackersFileIsValid,
    RemoveTrackersFileIsReadable: boolean;
begin
  CreateTrackerList(TrackerList);
  DecodeTorrentObj := TDecodeTorrent.Create;
  AddedTrackersRawList := TStringList.Create;
  FileSettingList := nil;
  MustExitWithErrorCode := False;
  LogFileIsOpen := False;

  try
    try
      LoadAddTrackersRaw(FolderForTrackerListLoadAndSave, AddedTrackersRawList);
      //Initial load must never fail on the announce check, only on a malformed URL scheme.
      //The error is in the log. Continuing with an empty add list would still change the
      //torrent files, and remove every tracker if remove_trackers.txt is empty.
      AddTrackersFileIsValid :=
        ValidateAndSanitizeTrackers(AddedTrackersRawList, TrackerList, True);
      if not AddTrackersFileIsValid then
        AddedTrackersRawList.Clear;

      //An unreadable remove_trackers.txt must not be ignored: the torrent files would be updated
      //without the removals the user asked for.
      RemoveTrackersFileIsReadable :=
        LoadRemoveTrackers(FolderForTrackerListLoadAndSave, TrackerList,
        FilePresentBanByUserList);
      if not RemoveTrackersFileIsReadable then
        LogConsoleError(TrackerList, 'ERROR: Can not read ' + FILE_NAME_REMOVE_TRACKERS);

      //Create the log file. The old one will be overwritten
      AssignFile(LogFile, FolderForTrackerListLoadAndSave + FILE_NAME_CONSOLE_LOG);
      ReWrite(LogFile);
      LogFileIsOpen := True;

      if AddTrackersFileIsValid and RemoveTrackersFileIsReadable and
        ConsoleModeDecodeParameter(FileNameOrDirStr, TrackerList) then
      begin
        if PathIsTorrentFolder(FileNameOrDirStr) then
        begin //A folder. Its name may contain a dot.
          if ConsoleDecodeTorrentFolder(FileNameOrDirStr, TrackerList, DecodeTorrentObj,
            FileSettingList) then
          begin
            if ApplyBanListRemoval(TrackerList, AddedTrackersRawList,
              FilePresentBanByUserList) then
              RunUpdateTorrentPipeline(TrackerList, DecodeTorrentObj, FileSettingList,
                AddedTrackersRawList, FolderForTrackerListLoadAndSave);
          end
          else
            LogConsoleError(TrackerList, 'Can not load torrent via folder');
        end
        else if PathIsTorrentFile(FileNameOrDirStr) then
        begin
          if ConsoleDecodeSingleTorrentFile(FileNameOrDirStr, TrackerList, DecodeTorrentObj,
            FileSettingList) then
          begin
            if ApplyBanListRemoval(TrackerList, AddedTrackersRawList,
              FilePresentBanByUserList) then
              RunUpdateTorrentPipeline(TrackerList, DecodeTorrentObj, FileSettingList,
                AddedTrackersRawList, FolderForTrackerListLoadAndSave);
          end
          else
            LogConsoleError(TrackerList, 'Can not load torrent file.');
        end
        else
          LogConsoleError(TrackerList, 'ERROR: No torrent file selected.');
      end;

      //if (no data) or (not CONSOLE_SUCCESS_STATUS) then error
      MustExitWithErrorCode := TrackerList.LogStringList.Count = 0;
      if not MustExitWithErrorCode then
        MustExitWithErrorCode := TrackerList.LogStringList[0] <> CONSOLE_SUCCESS_STATUS;

    except
      //This is needed or else the program will keep running forever.
      on E: Exception do
      begin
        MustExitWithErrorCode := True;
        //First line: an 'OK' that was already logged must not make this look like a success.
        TrackerList.LogStringList.Insert(0, 'ERROR: ' + E.Message);
      end;
    end;

    //Write to log file. And close the file, also after an exception.
    if LogFileIsOpen then
    begin
      try
        try
          WriteLn(LogFile, TrackerList.LogStringList.Text);
        finally
          CloseFile(LogFile);
        end;
      except
        MustExitWithErrorCode := True;
      end;
    end;

  finally
    AddedTrackersRawList.Free;
    DecodeTorrentObj.Free;
    FreeTrackerList(TrackerList);
  end;

  Result := not MustExitWithErrorCode;
end;

end.
