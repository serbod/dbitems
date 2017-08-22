unit EditTableFieldFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbUnit, MdUnit;

type

  { TFrameEditTableField }

  TFrameEditTableField = class(TFrame)
    btnOK: TButton;
    cbFieldType: TComboBox;
    cbLinkedTable: TComboBox;
    edFieldName: TEdit;
    edFieldNameFull: TEdit;
    lbFieldName: TLabel;
    lbFieldNameFull: TLabel;
    lbFieldLink: TLabel;
    lbFieldType: TLabel;
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
    FDbFieldInfo: TDbFieldInfo;
    FOnItemRename: TNotifyEvent;
    procedure SetDbFieldInfo(AValue: TDbFieldInfo);
  public
    { public declarations }
    MdStorage: TMdStorage;
    property DbFieldInfo: TDbFieldInfo read FDbFieldInfo write SetDbFieldInfo;
    property OnItemRename: TNotifyEvent read FOnItemRename write FOnItemRename;
    procedure ReadItem();
    procedure WriteItem();
  end;

implementation

{$R *.lfm}

{ TFrameEditTableField }

procedure TFrameEditTableField.btnOKClick(Sender: TObject);
begin
  WriteItem();
end;

procedure TFrameEditTableField.SetDbFieldInfo(AValue: TDbFieldInfo);
begin
  FDbFieldInfo := AValue;
  ReadItem();
end;

procedure TFrameEditTableField.ReadItem();
var
  sFt: string;
  i: Integer;
  TmpTableInfo: TDbTableInfo;
begin
  if not Assigned(DbFieldInfo) then
    Exit;

  Self.Visible := False;
  edFieldName.Text := DbFieldInfo.FieldName;
  edFieldNameFull.Text := DbFieldInfo.FieldDescription;

  lbFieldLink.Enabled := False;
  cbLinkedTable.Enabled := False;
  cbLinkedTable.Text := '';

  // fill tables list
  if Assigned(MdStorage) then
  begin
    for i := 0 to MdStorage.DbTableInfoList.Count - 1 do
    begin
      TmpTableInfo := (MdStorage.DbTableInfoList.Objects[i] as TDbTableInfo);
      cbLinkedTable.AddItem(TmpTableInfo.TableName, TmpTableInfo);
    end;
  end;

  // fill field types
  cbFieldType.Items.Clear();
  cbFieldType.Items.Append('Integer [I]');
  cbFieldType.Items.Append('String [S]');
  cbFieldType.Items.Append('Numeric [N]');
  cbFieldType.Items.Append('Date, time [T]');
  cbFieldType.Items.Append('Binary data [B]');
  cbFieldType.Items.Append('Link [L]');

  // set field type
  sFt := Copy(DbFieldInfo.FieldType, 1, 1);
  if sFt = 'I' then
    cbFieldType.ItemIndex := 0
  else if sFt = 'S' then
    cbFieldType.ItemIndex := 1
  else if sFt = 'N' then
    cbFieldType.ItemIndex := 2
  else if sFt = 'T' then
    cbFieldType.ItemIndex := 3
  else if sFt = 'B' then
    cbFieldType.ItemIndex := 4
  else if sFt = 'L' then
  begin
    cbFieldType.ItemIndex := 5;
    lbFieldLink.Enabled := True;
    cbLinkedTable.Enabled := True;
    cbLinkedTable.Text := Copy(DbFieldInfo.FieldType, 3, 9999);
  end;
  Self.Visible := True;
end;

procedure TFrameEditTableField.WriteItem();
var
  sFt: string;
begin
  if not Assigned(DbFieldInfo) then
    Exit;
  if Trim(edFieldName.Text) = '' then
    Exit;

  // set field type
  sFt := '';
  if cbFieldType.ItemIndex = 0 then
    sFt := 'I'
  else if cbFieldType.ItemIndex = 1 then
    sFt := 'S'
  else if cbFieldType.ItemIndex = 2 then
    sFt := 'N'
  else if cbFieldType.ItemIndex = 3 then
    sFt := 'T'
  else if cbFieldType.ItemIndex = 4 then
    sFt := 'B'
  else if cbFieldType.ItemIndex = 5 then
  begin
    sFt := 'L';
    if Trim(cbLinkedTable.Text) <> '' then
      sFt := sFt + '~' + Trim(cbLinkedTable.Text);
  end;

  //DbTableInfo.ModifyField(FieldIndex, Trim(edFieldName.Text), sFt);
  DbFieldInfo.FieldName := Trim(edFieldName.Text);
  DbFieldInfo.FieldDescription := Trim(edFieldNameFull.Text);
  DbFieldInfo.FieldType := sFt;

  if Assigned(OnItemRename) then
    OnItemRename(Self);
end;

end.
