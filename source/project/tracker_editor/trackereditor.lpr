program trackereditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Interfaces, // this includes the LCL widgetset
  Forms, DCPsha256, DCPconst, DCPcrypt2, main, bencode, decodetorrent,
  controllergridtorrentdata, controller_trackerlist_online, trackerlist_online,
  controller_treeview_torrent_data, update_torrent, fix_openssl;

{$R *.res}

begin
  {$IFDEF HEAPTRC_ENABLED}
  //Console mode has no attached console: without this, heaptrc's exit report shows as a
  //blocking MessageBox on Windows instead of being written out, hanging headless/automated runs.
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + 'heaptrc.log');
  {$ENDIF}
//  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormTrackerModify, FormTrackerModify);
  Application.Run;
end.

