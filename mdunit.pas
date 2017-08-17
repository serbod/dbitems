unit MdUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataStorage, DbUnit;

type

  { TMdStorage }

  TMdStorage = class(TInterfacedObject)
  public
    DbTableInfoList: TStringList;
    Filename: string;
    procedure SaveToFile();
    function LoadFromFile(): boolean;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    function DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): TDataStorage;
    function DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo;
      AStorage: IDataStorage): boolean;
  end;

implementation

{ TMdStorage }

procedure TMdStorage.SaveToFile();
var
  Storage, SubStorage, ItemStorage: IDataStorage;
  i: integer;
  Serializer: TDataSerializer;
begin
  if Filename = '' then
    Exit;

  SubStorage := TDataStorage.Create(stList);

  for i := 0 to DbTableInfoList.Count - 1 do
  begin
    ItemStorage := DbTableInfoToStorage((DbTableInfoList.Objects[i] as TDbTableInfo));
    SubStorage.SetValue(ItemStorage);
  end;

  Storage := TDataStorage.Create(stDictionary);
  Storage.SetValue('DbTableInfoList', 'DataType');
  Storage.SetValue(SubStorage, 'Items');

  Serializer := TDataSerializerBencode.Create();
  Serializer.StorageToFile(Storage, Filename);
  FreeAndNil(Serializer);

end;

function TMdStorage.LoadFromFile(): boolean;
var
  Storage: IDataStorage;
  SubStorage, ItemStorage: IDataStorage;
  i: integer;
  Serializer: TDataSerializer;
  DbTableInfo: TDbTableInfo;
begin
  Result := False;
  if Filename = '' then
    Exit;
  Storage := TDataStorage.Create(stDictionary);

  Serializer := TDataSerializerBencode.Create();
  Result := Serializer.StorageFromFile(Storage, Filename);
  FreeAndNil(Serializer);
  if not Result then
    Exit;
  Result := False;

  if Storage.GetString('DataType') <> 'DbTableInfoList' then
    Exit;

  SubStorage := Storage.GetObject('Items');
  if not Assigned(SubStorage) then
    Exit;

  for i := 0 to SubStorage.GetCount - 1 do
  begin
    ItemStorage := SubStorage.GetObject(i);
    DbTableInfo := TDbTableInfo.Create();
    if DbTableInfoFromStorage(DbTableInfo, ItemStorage) then
    begin
      DbTableInfoList.AddObject(DbTableInfo.TableName, DbTableInfo);
    end;
  end;
  Result := True;
end;

procedure TMdStorage.AfterConstruction();
begin
  inherited AfterConstruction;
  DbTableInfoList := TStringList.Create();
end;

procedure TMdStorage.BeforeDestruction();
begin
  FreeAndNil(DbTableInfoList);
  inherited BeforeDestruction;
end;

function TMdStorage.DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): TDataStorage;
var
  Storage, SubStorage: TDataStorage;
  i: integer;
  TmpField: TDbFieldInfo;
begin
  Result := TDataStorage.Create(stDictionary);
  if not Assigned(ADbTableInfo) then
    Exit;

  SubStorage := TDataStorage.Create(stList);

  for i := 0 to ADbTableInfo.FieldsCount - 1 do
  begin
    TmpField := ADbTableInfo.Fields[i];
    Storage := TDataStorage.Create(stDictionary);
    Storage.SetValue(TmpField.FieldName, 'Name');
    Storage.SetValue(TmpField.FieldType, 'Type');
    Storage.SetValue(TmpField.FieldDescription, 'Desc');
    SubStorage.SetValue(Storage);
  end;

  Result.SetValue('DbTableInfo', 'DataType');
  Result.SetValue(ADbTableInfo.DBName, 'DBName');
  Result.SetValue(ADbTableInfo.TableName, 'TableName');
  Result.SetValue(ADbTableInfo.TableDescription, 'TableDesc');
  Result.SetValue(ADbTableInfo.KeyFieldName, 'KeyFieldName');
  Result.SetValue(SubStorage, 'Fields');

end;

function TMdStorage.DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo;
  AStorage: IDataStorage): boolean;
var
  SubStorage: IDataStorage;
  SubStorageItem: IDataStorage;
  DbField: TDbFieldInfo;
  i: integer;
begin
  Result := False;
  if not Assigned(AStorage) then
    Exit;
  if AStorage.GetStorageType <> stDictionary then
    Exit;
  if AStorage.GetString('DataType') <> 'DbTableInfo' then
    Exit;
  ADbTableInfo.DBName := AStorage.GetString('DBName');
  ADbTableInfo.TableName := AStorage.GetString('TableName');
  ADbTableInfo.TableDescription := AStorage.GetString('TableDesc');
  ADbTableInfo.KeyFieldName := AStorage.GetString('KeyFieldName');
  SubStorage := AStorage.GetObject('Fields');
  if Assigned(SubStorage) then
  begin
    for i := 0 to SubStorage.GetCount - 1 do
    begin
      SubStorageItem := SubStorage.GetObject(i);
      DbField := ADbTableInfo.AddField(SubStorageItem.GetString('Name'),
        SubStorageItem.GetString('Type'));
      DbField.FieldDescription := SubStorageItem.GetString('Desc');
    end;
  end;
  Result := True;
end;

end.
