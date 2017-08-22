unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbUnit, MsgBoardUnit, DbBrowserFrame;

type

  { TFormMain }

  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public
    DbDriver: TDbDriver;
    MsgBoardList: TMsgBoardList;
    FrameDbBrowser: TFrameDbBrowser;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DbDriver := TDbDriverCSV.Create();

  MsgBoardList := TMsgBoardList.Create(DbDriver);
  MsgBoardList.FileName := 'Msg_Board.lst';
  MsgBoardList.LoadList();

  FrameDbBrowser := TFrameDbBrowser.Create(Self);
  FrameDbBrowser.Parent := Self;
  FrameDbBrowser.Align := alClient;
  FrameDbBrowser.DbDriver := DbDriver;
end;

end.

