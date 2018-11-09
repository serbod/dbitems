{
Модуль подсистемы базы данных.

!! в TDbItem массив значений больше необходимого

}
unit DbUnit;

interface

uses SysUtils, Classes;

const
  DB_FIELD_TYPE_INTEGER  = 'I';  // +-0123456789
  DB_FIELD_TYPE_NUMBER   = 'N';  // +-.E0123456789
  DB_FIELD_TYPE_STRING   = 'S';
  DB_FIELD_TYPE_DATETIME = 'D';  // YYYY-MM-DDThh:mm:ss
  DB_FIELD_TYPE_LINK     = 'L';  // table_name~id

type
  TDbTableInfo = class;

  TDbItemID = Int64;

  TDbItemClass = class of TDbItem;
  TDbItemClassID = LongWord;

  TDbItemListClass = class of TDbItemList;

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
    // Indexed field - fast search, slow write
    IsIndexed: Boolean;

    { visual field properties }
    // Width, MinWidth, MaxWidth
    // WidthUnits (pixels, points, percents)
    // Font
    // TextAlign
    // ReadOnly

    { header }
    // HeaderImage
    // HeaderText
    // HeaderHint
    // SortMode (none, ascend, descend)

    { Events }
    // OnValidate() - after edit
    // OnHint() - before show hint
    // OnText() - before show text
  end;

  { TDbFieldInfoList }

  TDbFieldInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TDbFieldInfo;
  end;

  { TDbTableInfo }
  // Database table information
  // - info about fields (columns) and info type
  // - table name
  TDbTableInfo = class(TObject)
  private
    FTopDbItemID: TDbItemID;
    FFieldInfoList: TDbFieldInfoList;
    function GetField(Index: Integer): TDbFieldInfo;
    function GetFieldName(Index: Integer): string;
    function GetFieldType(Index: Integer): string;
    function GetFieldsCount(): Integer;
  public
    // Database name
    DBName: string;
    // Table name
    TableName: string;
    // Table description
    TableDescription: string;
    // Key field name (ID)
    KeyFieldName: string;
    // Признак, того, что таблица соответствует своему аналогу в базе данных
    Valid: Boolean;

    ItemClass: TDbItemClass;
    ItemClassID: TDbItemClassID;

    ListClass: TDbItemListClass;

    constructor Create();
    destructor Destroy(); override;
    // Field info by field index
    property Fields[Index: Integer]: TDbFieldInfo read GetField;
    // Field name by field index
    property FieldNames[Index: Integer]: string read GetFieldName;
    // Field type by field index
    property Types[Index: Integer]: string read GetFieldType;
    // Fields count
    property FieldsCount: Integer read GetFieldsCount;

    // Create field with specified name and type
    function AddField(const FieldName, FieldType: string): TDbFieldInfo;
    // Chnage field name and type by field index
    procedure ModifyField(const Index: Integer; const FieldName, FieldType: string);
    // Remove field from table
    procedure DeleteField(const FieldName: string);
    // Return field index from field name
    function GetFieldIndex(const AName: string): Integer;

    // Return next unique ID for new item
    function GetNextItemID(): TDbItemID;
    // Set current top ID, if new is bigger
    procedure UpdateTopItemID(const AItemID: TDbItemID);
  end;

  { TDbTableInfoList }

  TDbTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TDbTableInfo;
    function GetItemByTableName(const AName: string): TDbTableInfo;
    function GetItemByDbItemClass(AItemClass: TDbItemClass): TDbTableInfo;
  end;

  // Database item, one row from table
  IDbItem = interface
    function GetID(): TDbItemID;
    procedure SetID(AValue: TDbItemID);
    function GetName(): string;
    procedure SetName(const AValue: string);
    // Get value by field name
    function GetValue(const AName: string): string;
    // Set value by field name
    procedure SetValue(const AName: string; AValue: string);
  end;

  { TDbItem }

  TDbItem = class(TInterfacedObject, IDbItem)
  protected
    // идентификатор элемента
    FID: TDbItemID;
    // строковое представление значения
    FName: string;
    FTimeStamp: TDateTime;
    FDbTableInfo: TDbTableInfo;
    procedure GetLocal();
    procedure SetLocal();
    procedure GetGlobal();
    procedure SetGlobal();
  public
    constructor Create(ADbTableInfo: TDbTableInfo); virtual;
    // Заполняет информацию о таблице данного элемента
    class procedure FillDbTableInfo(ADbTableInfo: TDbTableInfo); virtual;
    function GetID(): TDbItemID;
    procedure SetID(AValue: TDbItemID);
    function GetName(): string;
    procedure SetName(const AValue: string);

    // Возвращает строку TableName~ItemID
    function GetLinkID(): string;
    // Возвращает значение по имени колонки
    // Может быть переопределено в потомках
    function GetValue(const AName: string): string; virtual;

    // Устанавливает значение по имени колонки
    // Может быть переопределено в потомках
    procedure SetValue(const AName: string; AValue: string); virtual;
    // доступ к значению поля по его имени
    property Values[const AName: string]: string read GetValue write SetValue; default;

    // дата последнего изменения
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    // информация о таблице
    property DbTableInfo: TDbTableInfo read FDbTableInfo;

    function GetInteger(const AName: string): Integer;
    procedure SetInteger(const AName: string; Value: Integer);
  end;


  { TDbItemGeneric }
  { Универсальный элемент, способный хранить значения полей в текстовом виде }
  TDbItemGeneric = class(TDbItem)
  private
    // Массив значений (полей) элемента
    FValues: array of string;
    // Инициализирует массив значений, заполняет их пустыми значениями
    procedure InitValues();
  public
    function GetValue(const AName: string): string; override;
    procedure SetValue(const AName: string; AValue: string); override;
    constructor Create(ADbTableInfo: TDbTableInfo); override;
  end;


  TDbDriver = class;

  // Список однотипных элементов базы данных
  // Проще говоря - таблица

  { TDbItemList }

  TDbItemList = class(TList)
  protected
    FDbDriver: TDbDriver;
    FDbTableInfo: TDbTableInfo;
    FOwnItems: Boolean;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(ADbTableInfo: TDbTableInfo; ADbDriver: TDbDriver; AOwnItems: Boolean = False); reintroduce;
    //class function GetDbItemClass(): TDbItemClass; virtual;
    // Add item to list, set new ID if needed
    function AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
    function GetItem(AIndex: Integer): TDbItem;
    function GetItemByID(ItemID: TDbItemID): TDbItem;
    function GetItemByName(ItemName: string; Wildcard: Boolean = False): TDbItem;
    function GetItemIDByName(ItemName: string; Wildcard: Boolean = False): TDbItemID;
    function GetItemNameByID(ItemID: TDbItemID): string;
    // Create new item, add to list, set next ID
    function NewItem(): TDbItem; virtual;
    // read all items from database driver
    procedure FetchAll();
    // write all items to database driver
    procedure StoreAll();

    property DbTableInfo: TDbTableInfo read FDbTableInfo;
    property DbDriver: TDbDriver read FDbDriver;
  end;

  { TDbTableList }

  TDbTableList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TDbItemList;
    function GetItemByName(const AName: string): TDbItemList;
  end;

  { Database model, contains database description, all tables info }
  { TDbModel }

  TDbModel = class(TObject)
  private
    FTableInfoList: TDbTableInfoList;
    FTablesList: TDbTableList;
  protected
    FDbDriver: TDbDriver;
  public
    DbName: string;
    constructor Create();
    destructor Destroy(); override;
    // Возвращает описание таблицы по ее имени
    function GetDbTableInfo(const TableName: string): TDbTableInfo;
    // Добавляет описание таблицы в список описаний
    // Создает пустую локальную таблицу по ее описанию
    function AddTable(ATableInfo: TDbTableInfo): TDbItemList;
    // Добавляет поддержку элемента заданного класса
    procedure AddItemClass(AItemClass: TDbItemClass);
    // Возвращает таблицу для заданного класса элемента
    function GetTableForItemClass(AItemClass: TDbItemClass): TDbItemList;
    // Возвращает таблицу для заданного описания таблицы
    function GetTableForTableInfo(ATableInfo: TDbTableInfo): TDbItemList;

    // Создает новый элемент DBItem
    function NewDBItem(AItemClass: TDbItemClass): TDBItem; virtual;
    // Возвращает DBItem по значению вида Table_name~id
    function GetDBItem(const ALinkID: string; ACreate: Boolean = False): TDBItem; virtual;
    function SetDBItem(AItem: TDBItem): Boolean; virtual;

    // Список описаний таблиц TDbTableInfo
    property TableInfoList: TDbTableInfoList read FTableInfoList;
    // Список локальных (кеширующих) таблиц TDbItemList
    property TablesList: TDbTableList read FTablesList;

    property DbDriver: TDbDriver read FDbDriver write FDbDriver;
  end;

  // Драйвер базы данных - для доступа к хранилищу данных
  // Это базовый класс, должен быть переопределено для конкретных видов БД
  TDbDriver = class(TObject)
  private
    FOnDebugSQL: TGetStrProc;
    FTableInfoList: TDbTableInfoList;
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
    function GetDBItem(const ALinkID: string): TDBItem; virtual; abstract;
    function SetDBItem(AItem: TDBItem): Boolean; virtual; abstract;

    // Список описаний таблиц TDbTableInfo
    property TableInfoList: TDbTableInfoList read FTableInfoList;
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
    function GetDBItem(const ALinkID: string): TDBItem; override;
    function SetDBItem(AItem: TDBItem): Boolean; override;
  end;

var
  GlobalDbDriver: TDbDriver;

  function DefaultDbModel(): TDbModel;

  function ParseLinkID(const ALinkID: string; out ATableName: string; out AItemID: TDbItemID): Boolean;

  function DateTimeToDbStr(ADateTime: TDateTime): string;
  function DbStrToDateTime(const AValue: string): TDateTime;
  function DbStrToDateTimeDef(const AValue: string; ADateTime: TDateTime): TDateTime;

implementation

var
  LocalDefaultDbModel: TDbModel;

function DefaultDbModel(): TDbModel;
begin
  Result := LocalDefaultDbModel;
end;

function ParseLinkID(const ALinkID: string; out ATableName: string; out AItemID: TDbItemID): Boolean;
var
  n: Integer;
begin
  n := Pos('~', ALinkID);
  Result := (n > 0);
  if Result then
  begin
    ATableName := Copy(ALinkID, 1, n - 1);
    AItemID := StrToIntDef(Copy(ALinkID, n + 1, MaxInt), 0);
  end;
end;

function DateTimeToDbStr(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DDThh:nn:ss', ADateTime);
end;

function DbStrToDateTime(const AValue: string): TDateTime;
begin
  Result := StrToDateTime(AValue);
end;

function DbStrToDateTimeDef(const AValue: string; ADateTime: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(AValue, ADateTime);
end;

{ TDbTableList }

procedure TDbTableList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) then
    TDbItemList(Ptr).Free();
end;

function TDbTableList.GetItem(AIndex: Integer): TDbItemList;
begin
  Result := TDbItemList(Get(AIndex));
end;

function TDbTableList.GetItemByName(const AName: string): TDbItemList;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := GetItem(i);
    if Result.DbTableInfo.TableName = AName then
      Exit;
  end;
  Result := nil;
end;

{ TDbItemGeneric }

procedure TDbItemGeneric.InitValues();
var
  i: Integer;
begin
  if Assigned(DbTableInfo) then
  begin
    SetLength(FValues, DbTableInfo.FieldsCount);
    for i := 0 to DbTableInfo.FieldsCount - 1 do
      FValues[i] := '';
  end
  else
  begin
    SetLength(FValues, 0);
  end;
end;

function TDbItemGeneric.GetValue(const AName: string): string;
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
    if Length(FValues) = 0 then
      InitValues();
    if Assigned(DbTableInfo) then
      i := DbTableInfo.GetFieldIndex(AName)
    else
      i := -1;

    if i >= 0 then
      Result := FValues[i]
    else
      Result := '';
  end;
  //Result := inherited GetValue(AName);
end;

procedure TDbItemGeneric.SetValue(const AName: string; AValue: string);
var
  i: Integer;
begin
  if AName = 'id' then
    FID := StrToIntDef(AValue, 0)
  else if AName = 'timestamp' then
    FTimestamp := StrToDateTimeDef(AValue, FTimestamp)
  else if AName = 'name' then
    FName := AValue
  else
  begin
    if Length(FValues) = 0 then
      InitValues();
    if Assigned(DbTableInfo) then
      i := DbTableInfo.GetFieldIndex(AName)
    else
      i := -1;

    if i >= 0 then
      FValues[i] := AValue;
  end;
  //inherited SetValue(AName, AValue);
end;

constructor TDbItemGeneric.Create(ADbTableInfo: TDbTableInfo);
begin
  inherited Create(ADbTableInfo);
  InitValues();
end;

{ TDbModel }

constructor TDbModel.Create();
begin
  inherited;
  FTableInfoList := TDbTableInfoList.Create();
  FTablesList := TDbTableList.Create();
end;

destructor TDbModel.Destroy();
begin
  FreeAndNil(FTablesList);
  FreeAndNil(FTableInfoList);
  inherited Destroy;
end;

function TDbModel.GetDbTableInfo(const TableName: string): TDbTableInfo;
begin
  Result := TableInfoList.GetItemByTableName(TableName);
end;

function TDbModel.AddTable(ATableInfo: TDbTableInfo): TDbItemList;
var
  n: Integer;
begin
  n := TableInfoList.IndexOf(ATableInfo);
  if n < 0 then
    n := TableInfoList.Add(ATableInfo);

  Result := TablesList.GetItemByName(ATableInfo.TableName);
  if not Assigned(Result) then
  begin
    Result := ATableInfo.ListClass.Create(ATableInfo, DbDriver, True);
    TablesList.Add(Result);
  end;
end;

procedure TDbModel.AddItemClass(AItemClass: TDbItemClass);
var
  TmpTableInfo: TDbTableInfo;
begin
  TmpTableInfo := TDbTableInfo.Create();
  AItemClass.FillDbTableInfo(TmpTableInfo);

  if TableInfoList.GetItemByTableName(TmpTableInfo.TableName) <> nil then
    TmpTableInfo.Free()
  else
    AddTable(TmpTableInfo);
end;

function TDbModel.GetTableForItemClass(AItemClass: TDbItemClass): TDbItemList;
var
  TmpTableInfo: TDbTableInfo;
begin
  TmpTableInfo := TableInfoList.GetItemByDbItemClass(AItemClass);
  Result := TablesList.GetItemByName(TmpTableInfo.TableName);
end;

function TDbModel.GetTableForTableInfo(ATableInfo: TDbTableInfo): TDbItemList;
begin
  Result := TablesList.GetItemByName(ATableInfo.TableName);
end;

function TDbModel.NewDBItem(AItemClass: TDbItemClass): TDBItem;
var
  TmpTable: TDbItemList;
begin
  TmpTable := GetTableForItemClass(AItemClass);
  Result := TmpTable.NewItem();
end;

function TDbModel.GetDBItem(const ALinkID: string; ACreate: Boolean): TDBItem;
var
  sTableName: string;
  TmpItemID: TDbItemID;
  TmpTableInfo: TDbTableInfo;
  TmpTable: TDbItemList;
begin
  if Assigned(DbDriver) then
    Result := DbDriver.GetDBItem(ALinkID)
  else
    Result := nil;

  if not Assigned(Result) and ACreate then
  begin
    if ParseLinkID(ALinkID, sTableName, TmpItemID) then
    begin
      TmpTableInfo := GetDbTableInfo(sTableName);
      if Assigned(TmpTableInfo) then
      begin
        //Result := TmpTableInfo.ItemClass.Create(TmpTableInfo);
        TmpTable := TablesList.GetItemByName(TmpTableInfo.TableName);
        if Assigned(TmpTable) then
        begin
          Result := TmpTable.NewItem();
          Result.SetID(TmpItemID);
        end;
      end;
    end;
  end;
end;

function TDbModel.SetDBItem(AItem: TDBItem): Boolean;
begin
  if Assigned(DbDriver) then
    Result := DbDriver.SetDBItem(AItem)
  else
    Result := False;
end;

{ TDbFieldInfoList }

procedure TDbFieldInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) then
  begin
    TDbFieldInfo(Ptr).Free();
  end;
end;

function TDbFieldInfoList.GetItem(Index: Integer): TDbFieldInfo;
begin
  Result := TDbFieldInfo(inherited Get(Index));
end;

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

function TDbTableInfoList.GetItemByTableName(const AName: string): TDbTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := GetItem(i);
    if Result.TableName = AName then
      Exit;
  end;
  Result := nil;
end;

function TDbTableInfoList.GetItemByDbItemClass(AItemClass: TDbItemClass): TDbTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := GetItem(i);
    if Result.ItemClass = AItemClass then
      Exit;
  end;
  Result := nil;
end;

// === TDbTableInfo ===
function TDbTableInfo.GetField(Index: Integer): TDbFieldInfo;
begin
  Result := nil;
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := FFieldInfoList.GetItem(Index);
end;

function TDbTableInfo.GetFieldName(Index: Integer): string;
begin
  Result := '';
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := FFieldInfoList.GetItem(Index).FieldName;
end;

function TDbTableInfo.GetFieldsCount: Integer;
begin
  Result := FFieldInfoList.Count;
end;

function TDbTableInfo.GetFieldType(Index: Integer): string;
begin
  Result := '';
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := FFieldInfoList.GetItem(Index).FieldType;
end;

function TDbTableInfo.AddField(const FieldName, FieldType: string): TDbFieldInfo;
var
  i: Integer;
begin
  for i := 0 to FFieldInfoList.Count - 1 do
  begin
    Result := FFieldInfoList.GetItem(i);
    if Result.FieldName = FieldName then
      Exit;
  end;

  Result := TDbFieldInfo.Create();
  Result.FieldName := FieldName;
  Result.FieldType := FieldType;
  Result.TableInfo := Self;
  FFieldInfoList.Add(Result);
end;

procedure TDbTableInfo.ModifyField(const Index: Integer;
  const FieldName, FieldType: string);
var
  s: string;
  TmpField: TDbFieldInfo;
begin
  if (Index >= FFieldInfoList.Count) or (Index < 0) then
    Exit;
  TmpField := FFieldInfoList.GetItem(Index);
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
  i := GetFieldIndex(FieldName);
  if i <> -1 then
    FFieldInfoList.Delete(i);
end;

function TDbTableInfo.GetFieldIndex(const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FFieldInfoList.Count do
  begin
    if FFieldInfoList.GetItem(i).FieldName = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDbTableInfo.GetNextItemID(): TDbItemID;
begin
  Inc(FTopDbItemID);
  Result := FTopDbItemID;
end;

procedure TDbTableInfo.UpdateTopItemID(const AItemID: TDbItemID);
begin
  if FTopDbItemID < AItemID then
    FTopDbItemID := AItemID;
end;

constructor TDbTableInfo.Create();
begin
  inherited Create();
  FFieldInfoList := TDbFieldInfoList.Create();
  Valid := False;
  ItemClass := TDbItem;
  ListClass := TDbItemList;
  {AddField('id', 'I');
  AddField('name', 'S');
  AddField('timestamp', 'D');
  KeyFieldName := 'id';
  }
end;

destructor TDbTableInfo.Destroy();
begin
  FreeAndNil(FFieldInfoList);
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

constructor TDbItem.Create(ADbTableInfo: TDbTableInfo);
begin
  inherited Create();
  FDbTableInfo := ADbTableInfo;
end;

class procedure TDbItem.FillDbTableInfo(ADbTableInfo: TDbTableInfo);
begin
  ADbTableInfo.AddField('id', DB_FIELD_TYPE_INTEGER);
  ADbTableInfo.AddField('name', DB_FIELD_TYPE_STRING);
  ADbTableInfo.AddField('timestamp', DB_FIELD_TYPE_DATETIME);
  ADbTableInfo.KeyFieldName := 'id';
end;

function TDbItem.GetID(): TDbItemID;
begin
  Result := FID;
end;

procedure TDbItem.SetID(AValue: TDbItemID);
begin
  FID := AValue;
end;

function TDbItem.GetName(): string;
begin
  Result := FName;
end;

procedure TDbItem.SetName(const AValue: string);
begin
  FName := AValue;
end;

function TDbItem.GetLinkID(): string;
begin
  if Assigned(DbTableInfo) then
  begin
    Result := DbTableInfo.TableName + '~' + IntToStr(FID);
  end
  else
    Result := '~' + IntToStr(FID);
end;

function TDbItem.GetValue(const AName: string): string;
begin
  if AName = 'id' then
    Result := IntToStr(FID)
  else if AName = 'timestamp' then
    Result := DateTimeToStr(self.Timestamp)
  else if AName = 'name' then
    Result := FName
  else
    Result := '';
end;

//function TDbItem.GetDBItem(FName: string): TDBItem;
//begin
//  result:=nil;
//  if FName='id' then result:=self;
//end;

procedure TDbItem.SetValue(const AName: string; AValue: string);
begin
  if AName = 'id' then
    FID := StrToIntDef(AValue, 0)
  else if AName = 'timestamp' then
    self.Timestamp := StrToDateTimeDef(AValue, self.Timestamp)
  else if AName = 'name' then
    FName := AValue;
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
constructor TDbItemList.Create(ADbTableInfo: TDbTableInfo; ADbDriver: TDbDriver; AOwnItems: Boolean);
begin
  inherited Create();
  FDbTableInfo := ADbTableInfo;
  FDbDriver := ADbDriver;
  FOwnItems := AOwnItems;
end;

procedure TDbItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) and FOwnItems then
    TDbItem(Ptr).Free();
end;

{function TDbItemList.GetDbItemClass(): TDbItemClass;
begin
  Result := DbTableInfo.ItemClass;
end;  }

procedure TDbItemList.FetchAll();
begin
  if Assigned(DbDriver) then
    DbDriver.GetTable(self);
end;

procedure TDbItemList.StoreAll();
begin
  if Assigned(DbDriver) then
    DbDriver.SetTable(self);
end;

function TDbItemList.AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
begin
  if SetNewID then
  begin
    AItem.SetID(FDbTableInfo.GetNextItemID());
  end
  else
  begin
    FDbTableInfo.UpdateTopItemID(AItem.GetID());
  end;
  Result := Add(AItem);
end;

function TDbItemList.GetItem(AIndex: Integer): TDbItem;
begin
  Result := TDbItem(Get(AIndex));
end;

function TDbItemList.GetItemByID(ItemID: TDbItemID): TDbItem;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TDbItem(Get(i));
    if Result.GetID() = ItemID then
      Exit;
  end;
  Result := nil;
end;

function TDbItemList.GetItemByName(ItemName: string; Wildcard: Boolean = False): TDbItem;
var
  i: Integer;
begin
  if Wildcard then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := TDbItem(Get(i));
      if Pos(ItemName, Result.GetName()) > 0 then
        Exit;
    end;
  end

  else
  begin
    for i := 0 to Count - 1 do
    begin
      Result := TDbItem(Get(i));
      if Result.GetName() = ItemName then
        Exit;
    end;
  end;
  Result := nil;
end;

function TDbItemList.GetItemIDByName(ItemName: string;
  Wildcard: Boolean = False): TDbItemID;
var
  Item: TDbItem;
begin
  Result := -1;
  Item := Self.GetItemByName(ItemName, Wildcard);
  if Assigned(Item) then
    Result := Item.GetID;
end;

function TDbItemList.GetItemNameByID(ItemID: TDbItemID): string;
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
  Result := DbTableInfo.ItemClass.Create(DbTableInfo);
  AddItem(Result, True);
end;

// === TDbDriver ===
constructor TDbDriver.Create();
begin
  inherited;
  FTableInfoList := TDbTableInfoList.Create();
end;

destructor TDbDriver.Destroy();
begin
  self.Close();
  FreeAndNil(FTableInfoList);
  inherited;
end;

function TDbDriver.GetDbTableInfo(TableName: string): TDbTableInfo;
begin
  Result := TableInfoList.GetItemByTableName(TableName);
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
  if TableInfoList.IndexOf(TableInfo) >= 0 then
    Exit;

  TableInfo.Valid := True;
  TableInfoList.Add(TableInfo);
end;

function TDbDriverCSV.GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  sl, vl, fl, cl: TStringList;
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
  cl := TStringList.Create(); // column names
  vl := TStringList.Create(); // row values
  fl := TStringList.Create(); // filters
  try
    sl.LoadFromFile(fn);
    fl.CommaText := Filter;

    // первая строка - список колонок!
    for i := 0 to sl.Count - 1 do
    begin
      if i = 0 then
      begin
        cl.CommaText := sl[i];
        Continue;
      end;

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
        for n := 0 to cl.Count - 1 do
        begin
          fn := cl[n];
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
      for n := 0 to cl.Count - 1 do
      begin
        fn := cl[n];
        Item.SetValue(fn, vl[n]);
      end;
    end;
    Result := True;

  finally
    fl.Free();
    vl.Free();
    cl.Free();
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
      Item := AItemList.GetItem(i);
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

function TDbDriverCSV.GetDBItem(const ALinkID: string): TDbItem;
var
  sTableName, sItemID: string;
  i: Integer;
  TableInfo: TDbTableInfo;
  ItemList: TDbItemList;
  Filter: string;
begin
  Result := nil;
  i := Pos('~', ALinkID);
  sTableName := Copy(ALinkID, 1, i - 1);
  sItemID := Copy(ALinkID, i + 1, MaxInt);
  TableInfo := Self.GetDbTableInfo(sTableName);
  if not Assigned(TableInfo) then
    Exit;

  ItemList := TDbItemList.Create(TableInfo, Self);
  Filter := 'id=' + sItemID;

  if not GetTable(ItemList, Filter) then
    Exit;
  if ItemList.Count = 0 then
    Exit;
  Result := ItemList.GetItem(0);
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

initialization

LocalDefaultDbModel := TDbModel.Create();

finalization;

FreeAndNil(LocalDefaultDbModel);

end.
