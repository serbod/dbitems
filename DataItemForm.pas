unit DataItemForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, ExtCtrls,
  Buttons, DbUnit, RFUtils, Grids;

type

  { TFormDataItem }

  TFormDataItem = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    panBottom: TPanel;
    vleItem: TValueListEditor;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure vleItemButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure vleItemEditingDone(Sender: TObject);
  private
    FDbItem: TDbItem;
    procedure SetDbItem(AValue: TDbItem);
    procedure FormFromItem();
    procedure FormToItem();
  public
    property DbItem: TDbItem read FDbItem write SetDbItem;
  end;

var
  FormDataItem: TFormDataItem;

implementation

{$R *.lfm}

{ TFormDataItem }

procedure TFormDataItem.btnCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TFormDataItem.btnOKClick(Sender: TObject);
begin
  FormToItem();
  GlobalDbManager.SetDBItem(DbItem);
  Close();
end;

procedure TFormDataItem.vleItemButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  TableInfo: TDbTableInfo;
  ItemProp: TItemProp;
  FieldInfo: TDbFieldInfo;
  TmpItem: TDbItem;
begin
  if not Assigned(DbItem) then Exit;
  TableInfo := DbItem.DbTableInfo;
  ItemProp := vleItem.ItemProps[aRow-1];
  if not Assigned(ItemProp) then Exit;
  FieldInfo := TableInfo.GetFieldByName(ItemProp.KeyDesc);
  if Assigned(FieldInfo.MasterTable) then
  begin
    TmpItem := nil;
    GlobalDbManager.SelectItemFromList(FieldInfo.MasterTable.ItemsCache, TmpItem);
    if Assigned(TmpItem) then
      vleItem.Values[FieldInfo.FieldDescription] := TmpItem.GetName();
  end;
end;

procedure TFormDataItem.vleItemEditingDone(Sender: TObject);
begin
  btnOK.Enabled := True;
end;

procedure TFormDataItem.SetDbItem(AValue: TDbItem);
begin
  if FDbItem = AValue then Exit;
  FDbItem := AValue;
  FormFromItem();
end;

procedure TFormDataItem.FormFromItem;
var
  TableInfo: TDbTableInfo;
  i, n: Integer;
  sFieldName, sFieldDesc, sFieldValue: string;
  ItemProp: TItemProp;
begin
  if not Assigned(DbItem) then Exit;

  TableInfo := DbItem.DbTableInfo;
  vleItem.BeginUpdate();
  vleItem.Clear;

  // Columns
  for i := 0 to TableInfo.FieldsCount - 1 do
  begin
    if TableInfo.Fields[i].Width = 0 then
      Continue;

    sFieldName := TableInfo.Fields[i].FieldName;
    sFieldDesc := TableInfo.Fields[i].FieldDescription;
    if sFieldDesc = '' then
      sFieldDesc := sFieldName;
    sFieldValue := DbItem.GetValueText(sFieldName);

    n := vleItem.InsertRow(sFieldDesc, sFieldValue, True);
    ItemProp := vleItem.ItemProps[n];
    ItemProp.KeyDesc := sFieldName;
    if sFieldName = 'id' then
      ItemProp.ReadOnly := True;
    if TableInfo.Fields[i].MasterTable <> nil then
      ItemProp.EditStyle := esEllipsis
    else
    if (TableInfo.Fields[i].FieldType = DB_FIELD_TYPE_LINK)
    or (TableInfo.Fields[i].FieldType = DB_FIELD_TYPE_DATETIME) then
      ItemProp.EditStyle := esEllipsis;
  end;
  vleItem.EndUpdate();
  btnOK.Enabled := False;
end;

procedure TFormDataItem.FormToItem;
var
  TableInfo: TDbTableInfo;
  i, n: Integer;
  sFieldName, sFieldDesc, sOldValue, sNewValue: string;
  ItemProp: TItemProp;
begin
  if not Assigned(DbItem) then Exit;

  TableInfo := DbItem.DbTableInfo;

  // Columns (header at [0] _
  for i := 1 to vleItem.RowCount - 1 do
  begin
    ItemProp := vleItem.ItemProps[i-1];

    sFieldDesc := vleItem.Keys[i];
    sFieldName := ItemProp.KeyDesc;
    if sFieldName = 'id' then
      Continue;

    sOldValue := DbItem.GetValueText(sFieldName);
    sNewValue := vleItem.Values[sFieldDesc];
    if sOldValue = sNewValue then
      Continue;

    if ItemProp.EditStyle = esEllipsis then
    begin
      // todo
    end
    else
      DbItem.SetValue(sFieldName, sNewValue);
  end;
  btnOK.Enabled := False;
end;

end.

