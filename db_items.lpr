program db_items;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MdEditForm, DbDriverSqlite, MdUnit, DataStorage, EditTableFieldFrame,
  EditTableFrame
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMdEditor, FormMdEditor);
  Application.Run;
end.

