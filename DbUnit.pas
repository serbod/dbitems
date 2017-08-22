{
Модуль подсистемы базы данных.

!! в TDbItem массив значений больше необходимого

}
unit DbUnit;

interface

uses SysUtils, Classes, Contnrs;

type
  TDbTableInfo = class;

  { TDbFieldInfo }
  TDbFieldInfo = class(TObject)
  public
    // Name
    FieldName: string;
    // Type
    FieldType: string;
    // Descriptin
    FieldDescription: string;
    // Field belongs to this table info
    TableInfo: TDbTableInfo;
    // Field contain ID for MasterTable element
    MasterTable: TDbTableInfo;
  end;


  { TDbTableInfo }
  // Информация о таблице БД
  // - сведения о колонках и типах данных в колонках
  // - название таблицы
  TDbTableInfo = class(TObject)
  private
    FFields: TObjectList;
    function GetField(Index: Integer): TDbFieldInfo;
    function GetFieldName(Index: Integer): string;
    function GetFieldType(Index: Integer): string;
    function GetFieldsCount(): Integer;
  public
    DBName: string; // Имя базы данных
    TableName: string; // Имя таблицы
    TableDescription: string; // Описание таблицы
    KeyFieldName: string; // Имя ключевого поля (ID)
    // Признак, того, что таблица соответствует своему аналогу в базе данных
    Valid: Boolean;
    constructor Create();
    destructor Destroy(); override;
    // Список полей таблицы
    property Fields[Index: Integer]: TDbFieldInfo read GetField;
    // Список имен полей таблицы
    property FieldNames[Index: Integer]: string read GetFieldName;
    // Список типов полей таблицы
    property Types[Index: Integer]: string read GetFieldType;
    // Количество полей
    property FieldsCount: Integer read GetFieldsCount;
    // Создает поле с указаным именем и типом
    function AddField(const FieldName, FieldType: string): TDbFieldInfo;
    // Изменяет поле с указаным индексом, задает новое имя и тип
    procedure ModifyField(const Index: Integer; const FieldName, FieldType: string);
    // Remove field from table
    procedure DeleteField(const FieldName: string);
    // Возвращает номер поля по его имени
    function FieldIndex(AName: string): Integer;
  end;

  { TDbTableInfoList }

  TDbTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TDbTableInfo;
  end;

  // элемент таблицы БД, один ряд таблицы
  IDbItem = interface
    function GetID(): Integer;
    procedure SetID(AValue: Integer);
    function GetName(): string;
    procedure SetName(const AValue: string);
    // Возвращает значение по имени колонки
    function GetValue(const AName: string): string;
    // Устанавливает значение по имени колонки
    procedure SetValue(const AName, AValue: string);
  end;

  { TDbItem }

  TDbItem = class(TInterfacedObject, IDbItem)
  private
    // Массив значений (полей) элемента
    FValues: array of string;
    // Инициализирует массив значений, заполняет их пустыми значениями
    procedure InitValues();
  protected
    FID: Integer;  // идентификатор элемента
    FName: string; // строковое представление значения
    procedure GetLocal();
    procedure SetLocal();
    procedure GetGlobal();
    procedure SetGlobal();
  public
    Actual: Boolean;
    // признак соответствия данных элемента и БД
    TimeStamp: TDateTime; // дата последнего изменения
    DbTableInfo: TDbTableInfo; // информация о таблице
    function GetID(): Integer;
    procedure SetID(AValue: Integer);
    function GetName(): string;
    procedure SetName(const AValue: string);
    // Возвращает значение по имени колонки
    // Может быть переопределено в потомках
    function GetValue(const AName: string): string; virtual;
    // Возвращает DBItem по имени колонки
    // Должно быть переопределено в потомках
    //function GetDBItem(AName: string): TDBItem; virtual;
    // Устанавливает значение по имени колонки
    // Может быть переопределено в потомках
    procedure SetValue(const AName, AValue: string); virtual;
    // доступ к значению поля по его имени
    property Values[const AName: string]: string read GetValue write SetValue; default;

    function GetInteger(const AName: string): Integer;
    procedure SetInteger(const AName: string; Value: Integer);
  end;

  TDbDriver = class;

  // Список однотипных элементов базы данных
  // Проще говоря - таблица
  TDbItemList = class(TObjectList)
  protected
    FLastID: Integer;
    FDbDriver: TDbDriver;
    FDbTableInfo: TDbTableInfo;
  public
    constructor Create(ADbTableInfo: TDbTableInfo; ADbDriver: TDbDriver); reintroduce;
    function AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
    function GetItemByID(ItemID: Integer): TDbItem;
    function GetItemByName(ItemName: string; Wildcard: Boolean = False): TDbItem;
    function GetItemIDByName(ItemName: string; Wildcard: Boolean = False): Integer;
    function GetItemNameByID(ItemID: Integer): string;
    function NewItem(): TDbItem; virtual;
    procedure LoadLocal();
    procedure SaveLocal();

    property DbTableInfo: TDbTableInfo read FDbTableInfo;
    property DbDriver: TDbDriver read FDbDriver;
  end;

  // Драйвер базы данных - для доступа к хранилищу данных
  // Это базовый класс, должен быть переопределено для конкретных видов БД
  TDbDriver = class(TObject)
  private
    FOnDebugSQL: TGetStrProc;
    FTablesList: TDbTableInfoList;
  public
    DbName: string;
    constructor Create();
    destructor Destroy(); override;
    // Открывает указанную базу данных
    function Open(ADbName: string): Boolean; virtual; abstract;
    // Закрывает указанную базу данных
    function Close(): Boolean; virtual; abstract;
    // Возвращает описание таблицы по ее имени
    function GetDbTableInfo(TableName: string): TDbTableInfo;
    // Заполняет указанную таблицу элементами из базы данных, по заданному фильтру
    // Фильтр в виде comma-delimited string как поле=значение
    function GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
      virtual; abstract;
    // Заполняет базу данных элементами из указанной таблицы, по заданному фильтру
    function SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; virtual; abstract;
    // Возвращает DBItem по значению вида Table_name~id
    // Должно быть переопределено в потомках
    function GetDBItem(FValue: string): TDBItem; virtual; abstract;
    function SetDBItem(FItem: TDBItem): Boolean; virtual; abstract;

    // Список описаний таблиц TDbTableInfo
    property TablesList: TDbTableInfoList read FTablesList;
    // Triggers before SQL statement executed, return SQL text
    property OnDebugSQL: TGetStrProc read FOnDebugSQL write FOnDebugSQL;
  end;

  TDbDriverCSV = class(TDbDriver)
  private
    dbPath: string;
    procedure CheckTable(TableInfo: TDbTableInfo);
  public
    function Open(ADbName: string): Boolean; override;
    function Close(): Boolean; override;
    function GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function GetDBItem(AValue: string): TDBItem; override;
    function SetDBItem(AItem: TDBItem): Boolean; override;
  end;

var
  GlobalDbDriver: TDbDriver;

implementation

{ TDbTableInfoList }

procedure TDbTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) then
  begin
    TDbTableInfo(Ptr).Free();
  end;
end;

function TDbTableInfoList.GetItem(Index: Integer): TDbTableInfo;
begin
  Result := TDbTableInfo(inherited Get(Index));
end;

// === TDbTableInfo ===
function TDbTableInfo.GetField(Index: Integer): TDbFieldInfo;
begin
  Result := nil;
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := (FFields[Index] as TDbFieldInfo);
end;

function TDbTableInfo.GetFieldName(Index: Integer): string;
begin
  Result := '';
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := (FFields[Index] as TDbFieldInfo).FieldName;
end;

function TDbTableInfo.GetFieldType(Index: Integer): string;
begin
  Result := '';
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := (FFields[Index] as TDbFieldInfo).FieldType;
end;

function TDbTableInfo.GetFieldsCount(): Integer;
begin
  Result := Self.FFields.Count;
end;

function TDbTableInfo.AddField(const FieldName, FieldType: string): TDbFieldInfo;
var
  i: Integer;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    Result := (FFields[i] as TDbFieldInfo);
    if Result.FieldName = FieldName then
      Exit;
  end;

  Result := TDbFieldInfo.Create();
  Result.FieldName := FieldName;
  Result.FieldType := FieldType;
  Result.TableInfo := Self;
  FFields.Add(Result);
end;

procedure TDbTableInfo.ModifyField(const Index: Integer;
  const FieldName, FieldType: string);
var
  s: string;
  TmpField: TDbFieldInfo;
begin
  if (Index >= FFields.Count) or (Index < 0) then
    Exit;
  TmpField := (FFields[Index] as TDbFieldInfo);
  TmpField.FieldName := FieldName;
  s := FieldType;
  if Length(s) < 1 then
    s := 'S';
  TmpField.FieldType := s;
end;

procedure TDbTableInfo.DeleteField(const FieldName: string);
var
  i: Integer;
begin
  i := FieldIndex(FieldName);
  if i <> -1 then
    Self.FFields.Delete(i);
end;

function TDbTableInfo.FieldIndex(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FFields.Count do
  begin
    if GetFieldName(i) = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

constructor TDbTableInfo.Create();
begin
  inherited Create();
  Self.FFields := TObjectList.Create(False);
  Self.Valid := False;
  AddField('id', 'I');
  AddField('name', 'S');
  AddField('timestamp', 'D');
end;

destructor TDbTableInfo.Destroy();
begin
  FreeAndNil(Self.FFields);
  inherited Destroy();
end;

// === TDbItem ===
procedure TDbItem.GetLocal();
begin
end;

procedure TDbItem.SetLocal();
begin
end;

procedure TDbItem.GetGlobal();
begin
end;

procedure TDbItem.SetGlobal();
begin
end;

function TDbItem.GetID(): Integer;
begin
  Result := FID;
end;

procedure TDbItem.SetID(AValue: Integer);
begin
  FID := AValue;
end;

function TDbItem.GetName: string;
begin
  Result := FName;
end;

procedure TDbItem.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TDbItem.InitValues();
var
  i: Integer;
begin
  SetLength(Self.FValues, Self.DbTableInfo.FieldsCount);
  for i := 0 to Self.DbTableInfo.FieldsCount - 1 do
    Self.FValues[i] := '';
end;

function TDbItem.GetValue(const AName: string): string;
var
  i: Integer;
begin
  if AName = 'id' then
    Result := IntToStr(FID)
  else if AName = 'timestamp' then
    Result := DateTimeToStr(self.Timestamp)
  else if AName = 'name' then
    Result := FName
  else
  begin
    if Length(Self.FValues) = 0 then
      InitValues();
    i := Self.DbTableInfo.FieldIndex(AName);
    if i < 0 then
      Result := ''
    else
      Result := Self.FValues[i];
  end;
end;

//function TDbItem.GetDBItem(FName: string): TDBItem;
//begin
//  result:=nil;
//  if FName='id' then result:=self;
//end;

procedure TDbItem.SetValue(const AName, AValue: string);
var
  i: Integer;
begin
  if AName = 'id' then
    FID := StrToIntDef(AValue, 0)
  else if AName = 'timestamp' then
    self.Timestamp := StrToDateTimeDef(AValue, self.Timestamp)
  else if AName = 'name' then
    FName := AValue
  else
  begin
    if Length(Self.FValues) = 0 then
      InitValues();
    i := Self.DbTableInfo.FieldIndex(AName);
    if i >= 0 then
      Self.FValues[i] := AValue;
  end;
end;

function TDbItem.GetInteger(const AName: string): Integer;
begin
  Result := StrToIntDef(self.GetValue(AName), 0);
end;

procedure TDbItem.SetInteger(const AName: string; Value: Integer);
begin
  self.SetValue(AName, IntToStr(Value));
end;

// === TDbItemList ===
constructor TDbItemList.Create(ADbTableInfo: TDbTableInfo; ADbDriver: TDbDriver);
begin
  inherited Create(True);
  FDbTableInfo := ADbTableInfo;
  FDbDriver := ADbDriver;
end;

procedure TDbItemList.LoadLocal();
begin
  if Assigned(DbDriver) then
    DbDriver.GetTable(self);
end;

procedure TDbItemList.SaveLocal();
begin
  if Assigned(DbDriver) then
    DbDriver.SetTable(self);
end;

function TDbItemList.AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
begin
  if SetNewID then
  begin
    Inc(self.FLastID);
    AItem.SetID(self.FLastID);
  end
  else
  begin
    if self.FLastID < AItem.GetID then
      self.FLastID := AItem.GetID;
  end;
  AItem.DbTableInfo := FDbTableInfo;
  Result := self.Add(AItem);
end;

function TDbItemList.GetItemByID(ItemID: Integer): TDbItem;
var
  i: Integer;
begin
  for i := 0 to self.Count - 1 do
  begin
    if (self.Items[i] as TDbItem).GetID = ItemID then
    begin
      Result := (self.Items[i] as TDbItem);
      Exit;
    end;
  end;
  Result := nil;
end;

function TDbItemList.GetItemByName(ItemName: string; Wildcard: Boolean = False): TDbItem;
var
  i: Integer;
begin
  if Wildcard then
  begin
    for i := 0 to self.Count - 1 do
    begin
      if Pos(ItemName, (self.Items[i] as TDbItem).GetName) > 0 then
      begin
        Result := (self.Items[i] as TDbItem);
        Exit;
      end;
    end;
  end

  else
  begin
    for i := 0 to self.Count - 1 do
    begin
      if (self.Items[i] as TDbItem).GetName = ItemName then
      begin
        Result := (self.Items[i] as TDbItem);
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TDbItemList.GetItemIDByName(ItemName: string;
  Wildcard: Boolean = False): Integer;
var
  Item: TDbItem;
begin
  Result := -1;
  Item := Self.GetItemByName(ItemName, Wildcard);
  if Assigned(Item) then
    Result := Item.GetID;
end;

function TDbItemList.GetItemNameByID(ItemID: Integer): string;
var
  Item: TDbItem;
begin
  Result := '';
  Item := Self.GetItemByID(ItemID);
  if Assigned(Item) then
    Result := Item.GetName;
end;

function TDbItemList.NewItem(): TDbItem;
begin
  Result := TDbItem.Create();
  self.AddItem(Result, True);
end;

// === TDbDriver ===
constructor TDbDriver.Create();
begin
  inherited;
  FTablesList := TDbTableInfoList.Create();
end;

destructor TDbDriver.Destroy();
begin
  self.Close();
  FreeAndNil(FTablesList);
  inherited;
end;

function TDbDriver.GetDbTableInfo(TableName: string): TDbTableInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTablesList.Count - 1 do
  begin
    Result := FTablesList.GetItem(i);
    if Result.TableName = TableName then
      Exit;
  end;
  Result := nil;
end;

// === TDbDriverCSV ===
function TDbDriverCSV.Open(ADbName: string): Boolean;
begin
  self.DbName := ExtractFileName(ADbName);
  self.dbPath := ExtractFileDir(ADbName);
  Result := True;
end;

function TDbDriverCSV.Close(): Boolean;
begin
  Result := True;
end;

procedure TDbDriverCSV.CheckTable(TableInfo: TDbTableInfo);
begin
  if TableInfo.Valid then
    Exit;
  if FTablesList.IndexOf(TableInfo) >= 0 then
    Exit;

  TableInfo.Valid := True;
  FTablesList.Add(TableInfo);
end;

function TDbDriverCSV.GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  sl, vl, fl: TStringList;
  i, n, m, id: Integer;
  Item: TDbItem;
  fn: string;
  FilterOk: Boolean;
begin
  Result := False;
  if not Assigned(AItemList) then
    Exit;
  CheckTable(AItemList.DbTableInfo);
  fn := '';
  if self.dbPath <> '' then
    fn := IncludeTrailingPathDelimiter(self.dbPath);
  fn := fn + AItemList.DbTableInfo.TableName + '.lst';
  if not FileExists(fn) then
    Exit;

  sl := TStringList.Create();
  vl := TStringList.Create(); // row values
  fl := TStringList.Create(); // filters
  try
    sl.LoadFromFile(fn);
    fl.CommaText := Filter;

    // первая строка - список колонок!
    for i := 1 to sl.Count - 1 do
    begin
      vl.Clear();
      vl.CommaText := StringReplace(sl[i], '~>', #13 + #10, [rfReplaceAll]);
      if vl.Count = 0 then
        Continue;
      {if vl.Count < AItemList.DbTableInfo.FieldsCount then
        Continue; //!!    }

      // check filters
      FilterOk := True;
      if fl.Count > 0 then
      begin
        for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
        begin
          fn := AItemList.DbTableInfo.GetFieldName(n);
          for m := 0 to fl.Count - 1 do
          begin
            if fl.Names[m] = fn then
            begin
              if fl.ValueFromIndex[m] <> vl[n] then
                FilterOk := False;
              Break;
            end;
          end;
        end;
      end;

      if not FilterOk then
        Continue;
      // Create new item
      id := StrToInt(vl[0]);
      Item := AItemList.GetItemByID(id);
      if not Assigned(Item) then
        Item := AItemList.NewItem();

      // fill item values
      for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
      begin
        if n >= vl.Count then
          Break;
        fn := AItemList.DbTableInfo.GetFieldName(n);
        Item.SetValue(fn, vl[n]);
      end;
    end;
    Result := True;

  finally
    fl.Free();
    vl.Free();
    sl.Free();
  end;
end;

function TDbDriverCSV.SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  sl, vl: TStringList;
  i, n: Integer;
  Item: TDbItem;
  fn: string;
begin
  Result := False;
  if not Assigned(AItemList) then
    Exit;
  CheckTable(AItemList.DbTableInfo);

  sl := TStringList.Create();
  vl := TStringList.Create();
  try

    // columns headers
    for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
    begin
      fn := AItemList.DbTableInfo.GetFieldName(n);
      vl.Add(fn);
    end;
    sl.Add(vl.CommaText);

    // rows
    for i := 0 to AItemList.Count - 1 do
    begin
      vl.Clear();
      Item := (AItemList[i] as TDbItem);
      for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
      begin
        fn := AItemList.DbTableInfo.GetFieldName(n);
        vl.Add(Item.GetValue(fn));
      end;
      sl.Add(StringReplace(vl.CommaText, #13 + #10, '~>', [rfReplaceAll]));
    end;

    sl.SaveToFile(IncludeTrailingPathDelimiter(self.dbPath) + AItemList.DbTableInfo.TableName + '.lst');
    Result := True;
  finally
    vl.Free();
    sl.Free();
  end;
end;

function TDbDriverCSV.GetDBItem(AValue: string): TDbItem;
var
  sTableName, sItemID, fn, sql: string;
  i: Integer;
  TableInfo: TDbTableInfo;
  ItemList: TDbItemList;
  Filter: string;
begin
  Result := nil;
  i := Pos('~', AValue);
  sTableName := Copy(AValue, 1, i - 1);
  sItemID := Copy(AValue, i + 1, MaxInt);
  TableInfo := Self.GetDbTableInfo(sTableName);
  if not Assigned(TableInfo) then
    Exit;

  ItemList := TDbItemList.Create(TableInfo, Self);
  Filter := 'id=' + sItemID;

  if not GetTable(ItemList, Filter) then
    Exit;
  if ItemList.Count = 0 then
    Exit;
  Result := (ItemList[0] as TDbItem);
end;

function TDbDriverCSV.SetDBItem(AItem: TDBItem): Boolean;
var
  TmpItemList: TDbItemList;
  TmpItem: TDbItem;
  i: Integer;
  fn: string;
begin
  Result := False;
  TmpItemList := TDbItemList.Create(AItem.DbTableInfo, Self);
  try
    Self.GetTable(TmpItemList);
    TmpItem := TmpItemList.GetItemByID(AItem.GetID);
    if Assigned(TmpItem) then
    begin
      for i := 0 to AItem.DbTableInfo.FieldsCount - 1 do
      begin
        fn := AItem.DbTableInfo.GetFieldName(i);
        TmpItem.Values[fn] := AItem.Values[fn];
      end;
      Self.SetTable(TmpItemList);
    end;
    Result := True;
  finally
    FreeAndNil(TmpItemList);
  end;
end;

end.
