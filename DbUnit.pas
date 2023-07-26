{
Модуль подсистемы базы данных.

!! в TDbItem массив значений больше необходимого

TDbDriverCSV - драйвер БД, хранящей данные в файлах формата CSV
Переносы строк заменяются на "~>"

GlobalDbDriver - установите после создания драйвера БД
GlobalUseSnowflakeID - установитье в True для использования Snowflake ID
}
unit DbUnit;

interface

uses SysUtils, Classes, Contnrs;

const
  DB_FIELD_TYPE_INTEGER  = 'I';  // +-0123456789
  DB_FIELD_TYPE_NUMBER   = 'N';  // +-.E0123456789
  DB_FIELD_TYPE_STRING   = 'S';
  DB_FIELD_TYPE_DATETIME = 'D';  //
  DB_FIELD_TYPE_LINK     = 'L';  // table_name~id

type
  TDbTableInfo = class;
  TDbItemList = class;
  TDbItemClass = class of TDbItem;

  TDbItemID = Int64;
  TDbOperType = (otNone, otAdd, otUpdate, otDelete);

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
    //IsVisible: Boolean;
    Width: Integer;
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
    FFieldInfoList: TDbFieldInfoList;
    FItemsCache: TDbItemList;
    FNameFieldIndex: Integer;
    FDbItemClass: TDbItemClass;
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
    constructor Create(AItemClass: TDbItemClass);
    destructor Destroy(); override;
    function GetDbItemClass(): TDbItemClass; virtual;
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
    // Return 'name' field info
    function GetNameField(): TDbFieldInfo;
    // All created/readed items of same type
    property ItemsCache: TDbItemList read FItemsCache write FItemsCache;
  end;

  { TDbTableInfoList }

  TDbTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TDbTableInfo;
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
  private
    // Массив значений (полей) элемента
    FValues: array of string;
    // Инициализирует массив значений, заполняет их пустыми значениями
    procedure InitValues();
  protected
    // идентификатор элемента
    FID: TDbItemID;
    // строковое представление значения
    FName: string;
    procedure GetLocal();
    procedure SetLocal();
    procedure GetGlobal();
    procedure SetGlobal();
  public
    // дата последнего изменения
    //TimeStamp: TDateTime;
    // информация о таблице
    DbTableInfo: TDbTableInfo;
    class procedure FillDbTableInfo(ADbTableInfo: TDbTableInfo); virtual;
    function GetID(): TDbItemID;
    procedure SetID(AValue: TDbItemID);
    function GetName(): string;
    procedure SetName(const AValue: string);
    // Возвращает значение по имени колонки
    // Может быть переопределено в потомках
    function GetValue(const AName: string): string; virtual;

    // Возвращает текстовое представление (для пользователя) значения по имени колонки
    function GetValueText(const AName: string): string; virtual;

    // Устанавливает значение по имени колонки
    // Может быть переопределено в потомках
    procedure SetValue(const AName: string; AValue: string); virtual;
    // доступ к значению поля по его имени
    property Values[const AName: string]: string read GetValue write SetValue; default;

    function GetInteger(const AName: string): Integer;
    procedure SetInteger(const AName: string; Value: Integer);
  end;

  TDbManager = class;
  TDbDriver = class;

  // Список однотипных элементов базы данных
  // Проще говоря - таблица

  { TDbItemList }

  TDbItemList = class(TObjectList)
  protected
    FLastID: TDbItemID;
    FDbManager: TDbManager;
    FDbTableInfo: TDbTableInfo;
  public
    constructor Create(ADbTableInfo: TDbTableInfo; AManager: TDbManager); reintroduce;
    function AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
    function GetItemByIndex(AIndex: Integer): TDbItem;
    function GetItemByID(ItemID: TDbItemID): TDbItem;
    function GetItemByName(ItemName: string; Wildcard: Boolean = False): TDbItem;
    function GetItemIDByName(ItemName: string; Wildcard: Boolean = False): TDbItemID;
    function GetItemNameByID(ItemID: TDbItemID): string;
    function NewItem(): TDbItem; virtual;
    // read all items from database driver
    procedure FetchAll();
    // write all items to database driver
    procedure StoreAll();

    property DbTableInfo: TDbTableInfo read FDbTableInfo;
    property DbManager: TDbManager read FDbManager;
  end;

  { Database model, contains database description, all tables info }
  { TDbManager }

  TDbManager = class(TObject)
  private
    FTableInfoList: TDbTableInfoList;
  protected
    FDbDriver: TDbDriver;
  public
    DbName: string;
    constructor Create();
    destructor Destroy(); override;

    // Возвращает описание таблицы по классу элемента, создает новое при необходимости
    function GetDbTableInfo(AItemClass: TDbItemClass): TDbTableInfo; overload;
    // Возвращает описание таблицы по ее имени
    function GetDbTableInfo(const TableName: string): TDbTableInfo; overload;
    // Возвращает DBItem по значению вида Table_name~id
    function GetDBItem(const AValue: string): TDBItem; virtual;
    function SetDBItem(AItem: TDBItem): Boolean; virtual;

    // Список описаний таблиц TDbTableInfo
    property TableInfoList: TDbTableInfoList read FTableInfoList;

    property DbDriver: TDbDriver read FDbDriver write FDbDriver;
  end;

  // Драйвер базы данных - для доступа к хранилищу данных
  // Это базовый класс, должен быть переопределено для конкретных видов БД

  { TDbDriver }

  TDbDriver = class(TObject)
  private
    FOnDebugSQL: TGetStrProc;
    FDeletedItems: TList;
    FDbManager: TDbManager;
    function GetTablesList: TDbTableInfoList;
  public
    DbName: string;

    constructor Create(AManager: TDbManager);
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
    function GetDBItem(const AValue: string): TDBItem; virtual; abstract;
    function SetDBItem(AItem: TDBItem): Boolean; virtual; abstract;
    // удаляет элемент из базы данных и добавляет в список удаленных
    function DeleteDBItem(AItem: TDBItem): Boolean; virtual;

    property DbManager: TDbManager read FDbManager;
    // Список описаний таблиц TDbTableInfo
    property TablesList: TDbTableInfoList read GetTablesList;
    // Triggers before SQL statement executed, return SQL text
    property OnDebugSQL: TGetStrProc read FOnDebugSQL write FOnDebugSQL;
  end;

  { TDbDriverCSV }

  TDbDriverCSV = class(TDbDriver)
  private
    dbPath: string;
    FEventSourcingMode: Boolean; // режим регистрации событий (event sourcing)
    procedure CheckTable(TableInfo: TDbTableInfo);
    function GetTableFileName(TableInfo: TDbTableInfo): string;
    function ItemToStrCSV(AItem: TDBItem; ASL: TStringList = nil): string;
  public
    LogFileName: string; // if defined, write tables change log

    function Open(ADbName: string): Boolean; override;
    function Close(): Boolean; override;
    function GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function GetDBItem(const AValue: string): TDBItem; override;
    function SetDBItem(AItem: TDBItem): Boolean; override;
    function DeleteDBItem(AItem: TDBItem): Boolean; override;

    property EventSourcingMode: Boolean read FEventSourcingMode;
  end;

var
  GlobalDbManager: TDbManager;
  GlobalUseSnowflakeID: Boolean; // признак использования Snowflake ID вместо порядкового номера

function DefaultDbModel(): TDbManager;

// Возвращает новый GUID формата Snowflake ID
function GetSnowflakeID(): Int64;
// Установить machine ID (0..1023)
procedure SetMachineID(AValue: Integer);
// Получить дату/время из Snowflake ID
function GetDateTimeFromSnowflakeID(AValue: Int64): TDateTime;

// 2023-07-24 10:26:34 -> 20230724102634
function DateTimeToInt64(AValue: TDateTime): Int64;
// 20230724102634 -> 2023-07-24 10:26:34
function Int64ToDateTime(AValue: Int64): TDateTime;

implementation

var
  LocalDefaultDbModel: TDbManager;
  SnowflakeIDPrevTime: Int64;
  SnowflakeIDPrevSeq: Integer;
  SnowflakeIDMachineID: Integer;

function DefaultDbModel(): TDbManager;
begin
  Result := LocalDefaultDbModel;
end;

function GetSnowflakeID(): Int64;
var
  UTime: Int64;
  //dt: TDateTime;
  ts: TTimeStamp;
begin
  { The first 41 bits are a timestamp, representing milliseconds since the chosen epoch.
    The next 10 bits represent a machine ID, preventing clashes.
    Twelve more bits represent a per-machine sequence number,
    to allow creation of multiple snowflakes in the same millisecond. }
  //dt := Now() + (GetLocalTimeOffset / MinsPerDay);
  ts := DateTimeToTimeStamp(Now());
  ts.Date := ts.Date - DateDelta - UnixDateDelta; // 0001 -> 1899 -> 1970
  UTime := (ts.Date * MSecsPerDay) + ts.Time;

  if UTime <> SnowflakeIDPrevTime then
  begin
    SnowflakeIDPrevTime := UTime;
    SnowflakeIDPrevSeq := 0;
  end
  else
    Inc(SnowflakeIDPrevSeq);
  Result := SnowflakeIDPrevTime;
  Result := Result shl 10;
  Result := Result or SnowflakeIDMachineID;
  Result := Result shl 12;
  Result := Result or (SnowflakeIDPrevSeq and $FFF); // 12 bit
end;

procedure SetMachineID(AValue: Integer);
begin
  SnowflakeIDMachineID := (AValue and $3FF); // 10 bit
end;

function GetDateTimeFromSnowflakeID(AValue: Int64): TDateTime;
var
  UTime: Int64;
  ts: TTimeStamp;
begin
  UTime := (AValue shr 22); // msecs from 1970
  ts.Date := UTime div MSecsPerDay; // days from 1970
  ts.Time := UTime mod MSecsPerDay;
  ts.Date := ts.Date + UnixDateDelta + DateDelta; // 1970 -> 1899 -> 0001
  Result := TimeStampToDateTime(ts);
end;

// 2023-07-24 10:26:34 -> 20230724102634
function DateTimeToInt64(AValue: TDateTime): Int64;
var
  ny, nm, nd: Word;
  nh, nn, ns, nms: Word;
begin
  DecodeDate(AValue, ny, nm, nd);
  DecodeTime(AValue, nh, nn, ns, nms);
  Result := ns + (nn * 100) + (nh * 10000)
    + (nd * 1000000) + (nm * 100000000) + (ny * 10000000000);
end;

// 20230724102634 -> 2023-07-24 10:26:34
function Int64ToDateTime(AValue: Int64): TDateTime;
var
  ny, nm, nd: Word;
  nh, nn, ns, nms: Word;
begin
  ny := AValue div 10000000000;
  Dec(AValue, ny);
  nm := AValue div 100000000;
  Dec(AValue, nm);
  nd := AValue div 1000000;
  Dec(AValue, nd);
  nh := AValue div 10000;
  Dec(AValue, nh);
  nn := AValue div 100;
  Dec(AValue, nn);
  ns := AValue;
  nms := 0;
  Result := EncodeDate(ny, nm, nd) + EncodeTime(nh, nn, ns, nms);
end;

function AppendStrToFile(AFileName, AStr: string): Boolean;
var
  fs: TFileStream;
begin
  if AStr = '' then Exit;
  if FileExists(AFileName) then
    fs := TFileStream.Create(AFileName, fmOpenWrite + fmShareDenyNone)
  else
    fs := TFileStream.Create(AFileName, fmCreate + fmShareDenyNone);
  try
    fs.Seek(0, soFromEnd);
    fs.Write(AStr[1], Length(AStr) * SizeOf(Char));
  finally
    fs.Free();
  end;
end;

{ TDbManager }

constructor TDbManager.Create;
begin
  inherited;
  FTableInfoList := TDbTableInfoList.Create();
end;

destructor TDbManager.Destroy;
begin
  FreeAndNil(FTableInfoList);
  inherited Destroy;
end;

function TDbManager.GetDbTableInfo(AItemClass: TDbItemClass): TDbTableInfo;
var
  i: Integer;
begin
  for i := 0 to FTableInfoList.Count - 1 do
  begin
    Result := FTableInfoList.GetItem(i);
    if Result.GetDbItemClass() = AItemClass then
      Exit;
  end;
  // не нашли, создаем новую и добавляем в список
  Result := TDbTableInfo.Create(AItemClass);
  Result.ItemsCache := TDbItemList.Create(Result, Self);
  AItemClass.FillDbTableInfo(Result);
  FTableInfoList.Add(Result);
end;

function TDbManager.GetDbTableInfo(const TableName: string): TDbTableInfo;
var
  i: Integer;
begin
  for i := 0 to FTableInfoList.Count - 1 do
  begin
    Result := FTableInfoList.GetItem(i);
    if Result.TableName = TableName then
      Exit;
  end;
  Result := nil;
end;

function TDbManager.GetDBItem(const AValue: string): TDBItem;
begin
  if Assigned(DbDriver) then
    Result := DbDriver.GetDBItem(AValue)
  else
    Result := nil;
end;

function TDbManager.SetDBItem(AItem: TDBItem): Boolean;
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

// === TDbTableInfo ===
constructor TDbTableInfo.Create(AItemClass: TDbItemClass);
begin
  inherited Create();
  FFieldInfoList := TDbFieldInfoList.Create();
  FDbItemClass := AItemClass;
  Self.Valid := False;
  AddField('id', 'I');
  AddField('name', 'S');
  //AddField('timestamp', 'D');
end;

destructor TDbTableInfo.Destroy();
begin
  FreeAndNil(FFieldInfoList);
  inherited Destroy();
end;

function TDbTableInfo.GetDbItemClass: TDbItemClass;
begin
  Result := FDbItemClass;
end;

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

function TDbTableInfo.GetFieldType(Index: Integer): string;
begin
  Result := '';
  if (Index >= FieldsCount) or (Index < 0) then
    Exit;
  Result := FFieldInfoList.GetItem(Index).FieldType;
end;

function TDbTableInfo.GetFieldsCount(): Integer;
begin
  Result := FFieldInfoList.Count;
end;

function TDbTableInfo.AddField(const FieldName, FieldType: string): TDbFieldInfo;
var
  i, n: Integer;
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
  n := FFieldInfoList.Add(Result);
  if FieldName = 'name' then
    FNameFieldIndex := n;
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
  for i := 0 to FFieldInfoList.Count-1 do
  begin
    if FFieldInfoList.GetItem(i).FieldName = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDbTableInfo.GetNameField: TDbFieldInfo;
begin
  Result := Fields[FNameFieldIndex];
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

class procedure TDbItem.FillDbTableInfo(ADbTableInfo: TDbTableInfo);
begin
end;

function TDbItem.GetID(): TDbItemID;
begin
  Result := FID;
end;

procedure TDbItem.SetID(AValue: TDbItemID);
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
  //else if AName = 'timestamp' then
  //  Result := DateTimeToStr(self.Timestamp)
  else if AName = 'name' then
    Result := FName
  else
  begin
    if Length(Self.FValues) = 0 then
      InitValues();
    i := Self.DbTableInfo.GetFieldIndex(AName);
    if i < 0 then
      Result := ''
    else
      Result := Self.FValues[i];
  end;
end;

function TDbItem.GetValueText(const AName: string): string;
begin
  Result := GetValue(AName);
end;

procedure TDbItem.SetValue(const AName: string; AValue: string);
var
  i: Integer;
begin
  if AName = 'id' then
    FID := StrToInt64Def(AValue, 0)
  //else if AName = 'timestamp' then
  //  self.Timestamp := StrToDateTimeDef(AValue, self.Timestamp)
  else if AName = 'name' then
    FName := AValue
  else
  begin
    if Length(Self.FValues) = 0 then
      InitValues();
    i := Self.DbTableInfo.GetFieldIndex(AName);
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
constructor TDbItemList.Create(ADbTableInfo: TDbTableInfo; AManager: TDbManager);
begin
  inherited Create(True);
  FDbTableInfo := ADbTableInfo;
  FDbManager := AManager;
end;

procedure TDbItemList.FetchAll();
begin
  if Assigned(DbManager.DbDriver) then
    DbManager.DbDriver.GetTable(self);
end;

procedure TDbItemList.StoreAll();
begin
  if Assigned(DbManager.DbDriver) then
    DbManager.DbDriver.SetTable(self);
end;

function TDbItemList.AddItem(AItem: TDbItem; SetNewID: Boolean = False): Integer;
begin
  if SetNewID then
  begin
    if GlobalUseSnowflakeID then
      AItem.SetID(GetSnowflakeID())
    else
    begin
      Inc(self.FLastID);
      AItem.SetID(self.FLastID);
    end;
  end
  else
  begin
    if self.FLastID < AItem.GetID then
      self.FLastID := AItem.GetID;
  end;
  AItem.DbTableInfo := FDbTableInfo;
  Result := self.Add(AItem);
end;

function TDbItemList.GetItemByIndex(AIndex: Integer): TDbItem;
begin
  Result := (self.Items[AIndex] as TDbItem);
end;

function TDbItemList.GetItemByID(ItemID: TDbItemID): TDbItem;
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
  Result := DbTableInfo.GetDbItemClass().Create();
  AddItem(Result, True);
end;

function TDbDriver.GetTablesList: TDbTableInfoList;
begin
  Result := DbManager.TableInfoList;
end;

// === TDbDriver ===
constructor TDbDriver.Create(AManager: TDbManager);
begin
  inherited Create;
  FDbManager := AManager;
  FDeletedItems := TList.Create();
end;

destructor TDbDriver.Destroy();
begin
  self.Close();
  FreeAndNil(FDeletedItems);
  inherited;
end;

function TDbDriver.GetDbTableInfo(TableName: string): TDbTableInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to DbManager.TableInfoList.Count - 1 do
  begin
    Result := DbManager.TableInfoList.GetItem(i);
    if Result.TableName = TableName then
      Exit;
  end;
  Result := nil;
end;

function TDbDriver.DeleteDBItem(AItem: TDBItem): Boolean;
begin
  FDeletedItems.Add(AItem);
  Result := False;
end;

// === TDbDriverCSV ===
function TDbDriverCSV.Open(ADbName: string): Boolean;
begin
  self.DbName := ExtractFileName(ADbName);
  self.dbPath := ExtractFileDir(ADbName);
  if self.dbPath <> '' then
    ForceDirectories(self.dbPath);
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
  if TablesList.IndexOf(TableInfo) >= 0 then
    Exit;

  TableInfo.Valid := True;
  TablesList.Add(TableInfo);
end;

function TDbDriverCSV.GetTableFileName(TableInfo: TDbTableInfo): string;
begin
  Result := '';
  if self.dbPath <> '' then
    Result := IncludeTrailingPathDelimiter(self.dbPath);
  Result := Result + TableInfo.TableName + '.lst';
end;

function TDbDriverCSV.ItemToStrCSV(AItem: TDBItem; ASL: TStringList): string;
var
  sl: TStringList;
  i: Integer;
  s: string;
begin
  if Assigned(ASL) then
    sl := ASL
  else
    sl := TStringList.Create();
  try
    for i := 0 to AItem.DbTableInfo.FieldsCount - 1 do
    begin
      s := AItem.DbTableInfo.GetFieldName(i);
      sl.Add(AItem.GetValue(s));
    end;
    Result := StringReplace(sl.CommaText, #13 + #10, '~>', [rfReplaceAll]);
  finally
    if not Assigned(ASL) then
      sl.Free();
  end;
end;

function TDbDriverCSV.GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  sl, vl, fl, cl: TStringList;
  i, n, m: Integer;
  id: Int64;
  Item: TDbItem;
  ot: TDbOperType;
  fn: string;
  FilterOk: Boolean;
begin
  Result := False;
  if not Assigned(AItemList) then
    Exit;
  CheckTable(AItemList.DbTableInfo);
  fn := GetTableFileName(AItemList.DbTableInfo);
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
      ot := otUpdate;
      if FEventSourcingMode then
      begin
        // обработка первого символа первой колонки
        if Copy(vl[0], 1, 1) = '-' then
          ot := otDelete;
        vl[0] := Copy(vl[0], 2, MaxInt);
      end;

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
      id := StrToInt64(vl[0]);
      Item := AItemList.GetItemByID(id);
      if (not Assigned(Item)) and (ot <> otDelete) then
        Item := AItemList.NewItem()
      else if Assigned(Item) and (ot = otDelete) then
      begin
        // delete item from list
        AItemList.Extract(Item);
        FDeletedItems.Add(Item);
        Continue;
      end
      else
        Continue;

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
      Item := (AItemList[i] as TDbItem);
      if FEventSourcingMode then
        fn := ' ' + ItemToStrCSV(Item, vl)
      else
        fn := ItemToStrCSV(Item, vl);
      sl.Add(fn);
    end;

    sl.SaveToFile(GetTableFileName(AItemList.DbTableInfo));
    Result := True;
  finally
    vl.Free();
    sl.Free();
  end;
end;

function TDbDriverCSV.GetDBItem(const AValue: string): TDBItem;
var
  sTableName, sItemID: string;
  i: Integer;
  TableInfo: TDbTableInfo;
  Filter: string;
begin
  Result := nil;
  i := Pos('~', AValue);
  sTableName := Copy(AValue, 1, i - 1);
  sItemID := Copy(AValue, i + 1, MaxInt);
  TableInfo := Self.GetDbTableInfo(sTableName);
  if not Assigned(TableInfo) then
    Exit;

  Filter := 'id=' + sItemID;
  if GetTable(TableInfo.ItemsCache, Filter) then
  begin
    Result := TableInfo.ItemsCache.GetItemByID(StrToInt64(sItemID));
  end;
end;

function TDbDriverCSV.SetDBItem(AItem: TDBItem): Boolean;
var
  TmpItemList: TDbItemList;
  TmpItem: TDbItem;
  i: Integer;
  fn, s: string;
begin
  Result := False;
  if LogFileName <> '' then
  begin
    s := ' ' + IntToStr(AItem.GetID()) + ',' + IntToStr(DateTimeToInt64(Now()));
    AppendStrToFile(LogFileName, s + ',' + AItem.DbTableInfo.TableName + sLineBreak);
  end;

  if FEventSourcingMode then
  begin
    // добавление изменения
    s := ' ' + ItemToStrCSV(AItem) + sLineBreak;
    fn := GetTableFileName(AItem.DbTableInfo);
    AppendStrToFile(fn, s);
    Result := True;
    Exit;
  end;

  TmpItemList := TDbItemList.Create(AItem.DbTableInfo, DbManager);
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

function TDbDriverCSV.DeleteDBItem(AItem: TDBItem): Boolean;
var
  s, sFileName: string;
begin
  Result := inherited DeleteDBItem(AItem);
  sFileName := GetTableFileName(AItem.DbTableInfo);
  s := '-' + IntToStr(AItem.GetID()) + ',' + IntToStr(DateTimeToInt64(Now()));
  if FEventSourcingMode then
  begin
    // добавление в файл таблицы
    AppendStrToFile(sFileName, s + sLineBreak);
  end;

  if LogFileName <> '' then
  begin
    AppendStrToFile(LogFileName, s + ',' + AItem.DbTableInfo.TableName + sLineBreak);
  end;
end;

initialization

LocalDefaultDbModel := TDbManager.Create();

GlobalUseSnowflakeID := False;
SnowflakeIDPrevTime := 0;
SnowflakeIDPrevSeq := 0;
SnowflakeIDMachineID := 0;

finalization;

FreeAndNil(LocalDefaultDbModel);

end.
