unit DataListForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, EditBtn, DbUnit, RFUtils;

type

  { TFormDataList }

  TFormDataList = class(TForm)
    btnClearSearch: TBitBtn;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    btnNew: TBitBtn;
    btnEdit: TBitBtn;
    btnOK: TBitBtn;
    dedDateEnd: TDateEdit;
    dedDateStart: TDateEdit;
    edSearch: TEdit;
    lbDateFrom: TLabel;
    lbDateTo: TLabel;
    lbSearch: TLabel;
    lvList: TListView;
    panBottom: TPanel;
    panTop: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure dedDateChange(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvListData(Sender: TObject; Item: TListItem);
    procedure lvListDblClick(Sender: TObject);
    procedure lvListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FFilteredList: TList;  // filtered items
    FHideDate: Boolean;
    FSearchNames: TStringList; // field names for text search

    FItemsList: TDbItemList; // assigned
    FSortColumns: string; // column1,column2
    FIsReadOnly: Boolean;
    FHideButtons: Boolean;
    FSelectedItem: TDbItem;

    FOnItemAdd: TNotifyEvent;
    FOnItemEdit: TNotifyEvent;
    FOnItemDelete: TNotifyEvent;
    FOnItemSelected: TNotifyEvent;
    FOnItemFiltered: TDbItemFilterEvent;

    procedure SetHideButtons(AValue: Boolean);
    procedure SetHideDate(AValue: Boolean);
    procedure SetIsReadOnly(AValue: Boolean);
    procedure SetItemsList(AValue: TDbItemList);
    procedure FilterItems();
    procedure SetSelectedItem(AValue: TDbItem);
    function GetSelectedItem(): TDbItem;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure RefreshData();
    procedure AutoSizeColumns();

    property ItemsList: TDbItemList read FItemsList write SetItemsList;
    // column1,column2
    property SortColumns: string read FSortColumns write FSortColumns;
    property SelectedItem: TDbItem read FSelectedItem write SetSelectedItem;
    // Если True то кнопки Новый/Изменить/Удалить не видны
    property IsReadOnly: Boolean read FIsReadOnly write SetIsReadOnly;
    // Если True, то нижняя панель кнопок не видна, а событие OnItemSelected срабатывает
    // при смене текущего элемента
    property HideButtons: Boolean read FHideButtons write SetHideButtons;
    // Если True, то выбор интервала дат не отображается
    property HideDate: Boolean read FHideDate write SetHideDate;

    property OnItemSelected: TNotifyEvent read FOnItemSelected write FOnItemSelected;
    // срабатывает во время фильтрации элемента, IsAllowed разрешает элементу попасть в список
    property OnItemFiltered: TDbItemFilterEvent read FOnItemFiltered write FOnItemFiltered;
    // срабатывает при нажатии кнопки "новый"
    property OnItemAdd: TNotifyEvent read FOnItemAdd write FOnItemAdd;
    // срабатывает при нажатии кнопки редактирования
    property OnItemEdit: TNotifyEvent read FOnItemEdit write FOnItemEdit;
    // срабатывает при нажатии кнопки удаления
    property OnItemDelete: TNotifyEvent read FOnItemDelete write FOnItemDelete;
  end;

var
  FormDataList: TFormDataList;

implementation

{$R *.lfm}

var
  SortColumnNameArray: array of string;

{ TFormDataList }

procedure TFormDataList.lvListData(Sender: TObject; Item: TListItem);
var
  DbItem: TDbItem;
  i, n: integer;
  TableInfo: TDbTableInfo;
  fn, ft, s, sl: string;
begin
  if Assigned(Item) then
  begin
    if Item.Index < FFilteredList.Count then
    begin
      DbItem := TDbItem(FFilteredList[Item.Index]);
      TableInfo := FItemsList.DbTableInfo;
      n := 0; // номер видимой колонки
      for i := 0 to TableInfo.FieldsCount - 1 do
      begin
        if TableInfo.Fields[i].Width = 0 then
          Continue;
        fn := TableInfo.FieldNames[i];
        ft := TableInfo.Types[i];
        s := DbItem.GetValueText(fn);
        if ft = '' then
        else if ft[1] = 'L' then
        begin
          // Link
          sl := Copy(ft, 3, maxint) + '~' + s;
          {DbItem2 := DbDriver.GetDBItem(sl);
          if Assigned(DbItem2) then
            s := s + '*' + DbItem2.GetName; }
        end;

        if n = 0 then
          Item.Caption := s
        else
          Item.SubItems.Add(s);
        Inc(n);
      end;
    end;
  end;
end;

procedure TFormDataList.lvListDblClick(Sender: TObject);
begin
  if not HideButtons then
  begin
    if not Assigned(FSelectedItem) then
      FSelectedItem := GetSelectedItem();
    btnOk.Click;
  end;
end;

procedure TFormDataList.lvListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected and Assigned(Item) then
  begin
    if Item.Index < FFilteredList.Count then
    begin
      FSelectedItem := TDbItem(FFilteredList[Item.Index]);
      if HideButtons then
      begin
        if Assigned(OnItemSelected) then OnItemSelected(Self);
      end
      else
      begin
        btnOK.Enabled := True;
      end;
    end;
  end;
  if (not IsReadOnly) and (not HideButtons) then
  begin
    btnDelete.Enabled := Assigned(FSelectedItem);
    btnEdit.Enabled := Assigned(FSelectedItem);
  end;
end;

procedure TFormDataList.FormShow(Sender: TObject);
begin
  if Assigned(OnItemSelected) then
    Caption := 'Выбор из списка'
  else if IsReadOnly then
    Caption := 'Просмотр списка'
  else
    Caption := 'Редактирование списка';
  RefreshData();
end;

procedure TFormDataList.btnCancelClick(Sender: TObject);
begin
  FSelectedItem := nil;
  Close();
end;

procedure TFormDataList.btnClearSearchClick(Sender: TObject);
begin
  edSearch.Text := '';
end;

procedure TFormDataList.btnDeleteClick(Sender: TObject);
begin
  if Assigned(OnItemDelete) then OnItemDelete(Self);
  RefreshData();
end;

procedure TFormDataList.btnEditClick(Sender: TObject);
begin
  if Assigned(OnItemEdit) then OnItemEdit(Self);
  RefreshData();
end;

procedure TFormDataList.btnNewClick(Sender: TObject);
begin
  if Assigned(OnItemAdd) then OnItemAdd(Self);
  RefreshData();
end;

procedure TFormDataList.btnOKClick(Sender: TObject);
begin
  if Assigned(OnItemSelected) then OnItemSelected(Self);
  Close();
end;

procedure TFormDataList.dedDateChange(Sender: TObject);
begin
  if HideDate then Exit;
  RefreshData();
  SetSelectedItem(SelectedItem);
end;

procedure TFormDataList.edSearchChange(Sender: TObject);
begin
  RefreshData();
  SetSelectedItem(SelectedItem);
end;

procedure TFormDataList.FormActivate(Sender: TObject);
begin
  RefreshData();
end;

procedure TFormDataList.FormResize(Sender: TObject);
begin
  AutoSizeColumns();
end;

procedure TFormDataList.SetItemsList(AValue: TDbItemList);
var
  TableInfo: TDbTableInfo;
  i: Integer;
  Column: TListColumn;
begin
  if FItemsList = AValue then Exit;
  FItemsList := AValue;
  FSearchNames.Clear();
  lvList.Items.Count := 0;
  lvList.Columns.Clear();
  if not Assigned(FItemsList) then
  begin
    Exit;
  end;

  TableInfo := FItemsList.DbTableInfo;

  // Columns
  for i := 0 to TableInfo.FieldsCount - 1 do
  begin
    if TableInfo.Fields[i].Width = 0 then
      Continue;
    Column := lvList.Columns.Add();
    Column.Caption := TableInfo.Fields[i].FieldDescription;
    if Column.Caption = '' then
      Column.Caption := TableInfo.FieldNames[i];
    Column.Width := TableInfo.Fields[i].Width;

    // search names
    FSearchNames.Add(TableInfo.FieldNames[i]);
  end;

  btnOK.Enabled := False;
  RefreshData();
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  DbItem1, DbItem2: TDbItem;
  i: Integer;
begin
  DbItem1 := TDbItem(Item1);
  DbItem2 := TDbItem(Item2);
  Result := 0;
  if (not Assigned(DbItem1)) or (not Assigned(DbItem2)) then Exit;
  for i := Low(SortColumnNameArray) to High(SortColumnNameArray) do
  begin
    Result := CompareStr(DbItem1.GetValue(SortColumnNameArray[i]), DbItem2.GetValue(SortColumnNameArray[i]));
    if Result <> 0 then Exit;
  end;
end;

procedure TFormDataList.FilterItems;
var
  sSearchText, sItemText: string;
  i, ii: Integer;
  TmpItem: TDbItem;
  IsAccepted, IsCheckDate: Boolean;
  dt, dtStart, dtEnd: TDateTime;
begin
  FFilteredList.Clear();
  if not Assigned(FItemsList) then Exit;
  sSearchText := AnsiLowerCase(Trim(edSearch.Text));
  dtStart := dedDateStart.Date;
  dtEnd := dedDateEnd.Date;
  IsCheckDate := not HideDate;

  for i := 0 to FItemsList.Count-1 do
  begin
    TmpItem := FItemsList.GetItemByIndex(i);
    if IsCheckDate then
    begin
      // проверка вхождения в период
      dt := GetDateTimeFromSnowflakeID(TmpItem.GetID());
      if (dt < dtStart) or (dt > dtEnd) then
        Continue;
    end;
    IsAccepted := False;
    if sSearchText = '' then
    begin
      IsAccepted := True;
    end
    else
    begin
      // поиск в видимых колонках
      sItemText := '';
      for ii := 0 to FSearchNames.Count-1 do
      begin
        sItemText := sItemText + AnsiLowerCase(TmpItem.GetValueText(FSearchNames[ii])) + ' ';
      end;
      if Pos(sSearchText, sItemText) > 0 then
        IsAccepted := True;
    end;

    if IsAccepted and Assigned(OnItemFiltered) then
    begin
      OnItemFiltered(TmpItem, IsAccepted);
    end;

    if IsAccepted then
      FFilteredList.Add(TmpItem);
  end;

  // массив имен сортируемых колонок
  SetLength(SortColumnNameArray, 0);
  sSearchText := FSortColumns;
  while sSearchText <> '' do
  begin
    sItemText := ExtractFirstWord(sSearchText, ',');
    if sItemText <> '' then
    begin
      SetLength(SortColumnNameArray, Length(SortColumnNameArray) + 1);
      SortColumnNameArray[Length(SortColumnNameArray)-1] := sItemText;
    end;
  end;
  // сортировка по колонкам
  if Length(SortColumnNameArray) > 0 then
    FFilteredList.Sort(@CompareItems);

  // Если выбраный элемент не входит в фильтр
  if Assigned(FSelectedItem) and (FFilteredList.IndexOf(FSelectedItem) = -1) then
  begin
    FSelectedItem := nil;
    if HideButtons then
      if Assigned(OnItemSelected) then OnItemSelected(Self);
  end;
  // доступность кнопок
  if (not IsReadOnly) and (not HideButtons) then
  begin
    btnDelete.Enabled := Assigned(FSelectedItem);
    btnEdit.Enabled := Assigned(FSelectedItem);
  end;
end;

procedure TFormDataList.SetSelectedItem(AValue: TDbItem);
var
  i: Integer;
begin
  //if FSelectedItem = AValue then Exit;
  FSelectedItem := AValue;
  // фокус на выбраный элемент
  if Assigned(AValue) then
  begin
    i := FFilteredList.IndexOf(AValue);
    lvList.ItemIndex := i;
    if i = -1 then
    begin
      // не нашли в списке
      FSelectedItem := nil;
    end;
  end
  else
    lvList.ItemIndex := -1;

  if HideButtons then
    if Assigned(OnItemSelected) then OnItemSelected(Self);
end;

function TFormDataList.GetSelectedItem: TDbItem;
var
  Item: TListItem;
begin
  Result := nil;
  Item := lvList.Selected;
  if Assigned(Item) and (Item.Index < FFilteredList.Count) then
  begin
    Result := TDbItem(FFilteredList[Item.Index]);
  end;
end;

procedure TFormDataList.SetIsReadOnly(AValue: Boolean);
begin
  if FIsReadOnly = AValue then Exit;
  FIsReadOnly := AValue;

  btnNew.Visible := not FIsReadOnly;
  btnEdit.Visible := not FIsReadOnly;
  btnDelete.Visible := not FIsReadOnly;
end;

procedure TFormDataList.SetHideButtons(AValue: Boolean);
begin
  if FHideButtons = AValue then Exit;
  FHideButtons := AValue;
  panBottom.Visible := not FHideButtons;
end;

procedure TFormDataList.SetHideDate(AValue: Boolean);
begin
  if FHideDate = AValue then Exit;
  FHideDate := AValue;
  lbDateFrom.Visible   := not FHideDate;
  lbDateTo.Visible     := not FHideDate;
  dedDateStart.Visible := not FHideDate;
  dedDateEnd.Visible   := not FHideDate;
end;

procedure TFormDataList.AfterConstruction;
begin
  inherited AfterConstruction;
  FFilteredList := TList.Create();
  FSearchNames := TStringList.Create();
  HideDate := True;
  dedDateStart.Date := Now();
  dedDateEnd.Date := Now();
end;

procedure TFormDataList.BeforeDestruction;
begin
  FreeAndNil(FSearchNames);
  FreeAndNil(FFilteredList);
  inherited BeforeDestruction;
end;

procedure TFormDataList.RefreshData;
begin
  FilterItems();
  if not Assigned(FItemsList) then
  begin
    lvList.Items.Count := 0;
    Exit;
  end;

  if lvList.Items.Count <> FFilteredList.Count then
    lvList.Items.Count := FFilteredList.Count;
  lvList.Refresh();
end;

procedure TFormDataList.AutoSizeColumns;
var
  TableInfo: TDbTableInfo;
  i, n: Integer;
  Ratio: Real;
  Column: TListColumn;
begin
  if not Assigned(FItemsList) then Exit;
  TableInfo := FItemsList.DbTableInfo;

  // примитивный способ, по размеру шрифта
  Ratio := lvList.Font.Size / 10; // базовый размер 10
  // Columns
  n := 0;
  for i := 0 to TableInfo.FieldsCount - 1 do
  begin
    if TableInfo.Fields[i].Width = 0 then
      Continue;

    Column := lvList.Columns.Items[n];
    Column.Width := Round(TableInfo.Fields[i].Width * Ratio);

    Inc(n);
  end;

end;

end.

