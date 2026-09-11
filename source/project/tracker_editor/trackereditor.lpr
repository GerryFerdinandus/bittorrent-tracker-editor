program trackereditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DCPsha256, DCPconst, DCPcrypt2, main, bencode, decodetorrent,
  controllergridtorrentdata, controller_trackerlist_online, trackerlist_online,
  controller_treeview_torrent_data, fix_openssl;

{$R *.res}

begin
//  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormTrackerModify, FormTrackerModify);
  Application.Run;
end.

