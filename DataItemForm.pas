unit DataItemForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, ExtCtrls,
  Buttons, DbUnit, RFUtils, Grids, ExtDlgs;

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
  sFieldName, sValue: string;
  FieldInfo: TDbFieldInfo;
  TmpItem: TDbItem;
  cd: TCalendarDialog;
  nn: Int64;
begin
  if not Assigned(DbItem) then Exit;
  TableInfo := DbItem.DbTableInfo;
  ItemProp := vleItem.ItemProps[aRow-1];
  if not Assigned(ItemProp) then Exit;
  sFieldName := ItemProp.KeyDesc;
  sValue := '';
  // name:value
  if Pos(':', sFieldName) > 0 then
  begin
    sValue := sFieldName;
    sFieldName := ExtractFirstWord(sValue, ':');
  end;
  FieldInfo := TableInfo.GetFieldByName(sFieldName);
  if Assigned(FieldInfo) and Assigned(FieldInfo.MasterTable) then
  begin
    TmpItem := nil; // todo: find existing item from cache
    GlobalDbManager.SelectItemFromList(FieldInfo.MasterTable.ItemsCache, TmpItem);
    if Assigned(TmpItem) then
    begin
      vleItem.Values[FieldInfo.FieldDescription] := TmpItem.GetName();
      ItemProp.KeyDesc := sFieldName + ':' + TmpItem.GetIDStr();
    end;
  end
  else
  if Assigned(FieldInfo) and (FieldInfo.FieldType = DB_FIELD_TYPE_DATETIME) then
  begin
    // date dialog
    cd := TCalendarDialog.Create(Self);
    try
      nn := StrToInt64Def(sValue, 0);
      if nn = 0 then
        cd.Date := Now
      else
        cd.Date := Int64ToDateTime(nn);
      if cd.Execute then
      begin
        nn := DateTimeToInt64(cd.Date);
        vleItem.Values[FieldInfo.FieldDescription] := FormatDateTime('YYYY-MM-DD', cd.Date);
        ItemProp.KeyDesc := sFieldName + ':' + IntToStr(nn);
      end;
    finally
      cd.Free();
    end;
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
  i, ii, n: Integer;
  sFieldName, sFieldDesc, sFieldValue: string;
  ItemProp: TItemProp;
  TmpField: TDbFieldInfo;
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

    TmpField := TableInfo.Fields[i];
    sFieldName := TmpField.FieldName;
    sFieldDesc := TmpField.FieldDescription;
    if sFieldDesc = '' then
      sFieldDesc := sFieldName;
    sFieldValue := DbItem.GetValueText(sFieldName);

    n := vleItem.InsertRow(sFieldDesc, sFieldValue, True);
    ItemProp := vleItem.ItemProps[n];
    ItemProp.KeyDesc := sFieldName;
    if sFieldName = 'id' then
      ItemProp.ReadOnly := True;
    if TmpField.MasterTable <> nil then
      ItemProp.EditStyle := esEllipsis
    else
    if (TmpField.FieldType = DB_FIELD_TYPE_LINK)
    or (TmpField.FieldType = DB_FIELD_TYPE_DATETIME) then
    begin
      // links and dates can be selected from dialogs
      ItemProp.EditStyle := esEllipsis;
      // name:value
      ItemProp.KeyDesc := ItemProp.KeyDesc + ':' + DbItem.GetValue(sFieldName);
    end
    else
    if (TmpField.FieldType = DB_FIELD_TYPE_INTEGER)
    and (Length(TmpField.EnumValues) > 0) then
    begin
      // enumeration, fill dropdown list
      ItemProp.EditStyle := esPickList;
      ItemProp.PickList.Clear;
      for ii := Low(TmpField.EnumValues) to High(TmpField.EnumValues) do
        ItemProp.PickList.Add(TmpField.EnumValues[ii]);
    end;

  end;
  vleItem.EndUpdate();
  btnOK.Enabled := False;
end;

procedure TFormDataItem.FormToItem;
var
  TableInfo: TDbTableInfo;
  i, ii, n: Integer;
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

    // name:value
    if Pos(':', sFieldName) > 0 then
    begin
      sNewValue := sFieldName;
      sFieldName := ExtractFirstWord(sNewValue, ':');
    end
    else
      sNewValue := vleItem.Values[sFieldDesc];
    sOldValue := DbItem.GetValueText(sFieldName);
    if sOldValue = sNewValue then
      Continue;

    if ItemProp.EditStyle = esEllipsis then
    begin
      // name and value already extracted
      DbItem.SetValue(sFieldName, sNewValue);
    end
    else
    if ItemProp.EditStyle = esPickList then
    begin
      // index of dropdown list item
      for ii := 0 to ItemProp.PickList.Count-1 do
      begin
        if ItemProp.PickList[ii] = sNewValue then
        begin
          DbItem.SetValue(sFieldName, IntToStr(ii));
          Break;
        end;
      end;
    end
    else
      DbItem.SetValue(sFieldName, sNewValue);
  end;
  btnOK.Enabled := False;
end;

end.

