unit EditTableFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbUnit, MdUnit;

type

  { TFrameEditTable }

  TFrameEditTable = class(TFrame)
    btnOK: TButton;
    cbKeyFieldName: TComboBox;
    edTableDesc: TEdit;
    edTableName: TEdit;
    lbKeyFieldName: TLabel;
    lbTableNameFull: TLabel;
    lbTableName: TLabel;
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
    FDbTableInfo: TDbTableInfo;
    FOnItemRename: TNotifyEvent;
    procedure SetDbTableInfo(AValue: TDbTableInfo);
  public
    { public declarations }
    MdStorage: TMdStorage;
    property DbTableInfo: TDbTableInfo read FDbTableInfo write SetDbTableInfo;
    property OnItemRename: TNotifyEvent read FOnItemRename write FOnItemRename;
    procedure ReadItem();
    procedure WriteItem();
  end;

implementation

{$R *.lfm}

{ TFrameEditTable }

procedure TFrameEditTable.btnOKClick(Sender: TObject);
begin
  WriteItem();
end;

procedure TFrameEditTable.SetDbTableInfo(AValue: TDbTableInfo);
begin
  FDbTableInfo:=AValue;
  ReadItem();
end;

procedure TFrameEditTable.ReadItem();
var
  i: Integer;
  TmpField: TDbFieldInfo;
begin
  if not Assigned(DbTableInfo) then Exit;

  Self.Visible:=False;
  edTableName.Text:=DbTableInfo.TableName;
  edTableDesc.Text:=DbTableInfo.TableDescription;

  lbKeyFieldName.Enabled:=False;
  cbKeyFieldName.Enabled:=False;
  cbKeyFieldName.Text:='';

  // fill key fields list
  for i:=0 to DbTableInfo.FieldsCount-1 do
  begin
    TmpField:=DbTableInfo.Fields[i];
    if TmpField.FieldType='I' then
    begin
      cbKeyFieldName.AddItem(TmpField.FieldName, TmpField);
    end;
  end;

  Self.Visible:=True;
end;

procedure TFrameEditTable.WriteItem();
begin
  if not Assigned(DbTableInfo) then Exit;
  if Trim(edTableName.Text)='' then Exit;

  DbTableInfo.TableName:=Trim(edTableName.Text);
  DbTableInfo.TableDescription:=Trim(edTableDesc.Text);
  DbTableInfo.KeyFieldName:=Trim(cbKeyFieldName.Text);

  if Assigned(OnItemRename) then OnItemRename(Self);
end;

end.

