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
    function LoadFromFile(): Boolean;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    function DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): IDataStorage;
    function DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo;
      AStorage: IDataStorage): Boolean;
  end;

implementation

{ TMdStorage }

procedure TMdStorage.SaveToFile();
var
  Storage, SubStorage, ItemStorage: IDataStorage;
  i: Integer;
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
  try
    Serializer.StorageToFile(Storage, Filename);
  finally
    FreeAndNil(Serializer);
  end;

end;

function TMdStorage.LoadFromFile(): Boolean;
var
  Storage: IDataStorage;
  SubStorage, ItemStorage: IDataStorage;
  i: Integer;
  Serializer: TDataSerializer;
  DbTableInfo: TDbTableInfo;
begin
  Result := False;
  if Filename = '' then
    Exit;
  Storage := nil;

  Serializer := TDataSerializerBencode.Create();
  try
    Storage := Serializer.StorageFromFile(Filename);
  finally
    FreeAndNil(Serializer);
  end;

  if (not Assigned(Storage)) or (Storage.GetString('DataType') <> 'DbTableInfoList') then
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

function TMdStorage.DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): IDataStorage;
var
  Storage, SubStorage: IDataStorage;
  i: Integer;
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
    if Assigned(TmpField.MasterTable) then
      Storage.SetValue(TmpField.MasterTable.TableName, 'MasterTable');
    if TmpField.IsIndexed then
      Storage.SetValue(TmpField.IsIndexed, 'Indexed');
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
  AStorage: IDataStorage): Boolean;
var
  SubStorage: IDataStorage;
  SubStorageItem: IDataStorage;
  DbField: TDbFieldInfo;
  i: Integer;
begin
  Result := False;
  if (not Assigned(AStorage))
  or (AStorage.GetStorageType <> stDictionary)
  or (AStorage.GetString('DataType') <> 'DbTableInfo') then
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
      DbField.IsIndexed := SubStorageItem.GetBool('Indexed');
    end;
  end;
  Result := True;
end;

end.
