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
    function DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): TDataStorage;
    function DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo; AStorage: TDataStorage): Boolean;
  end;

implementation

{ TMdStorage }

procedure TMdStorage.SaveToFile();
var
  Storage, SubStorage, ItemStorage: TDataStorage;
  i: integer;
  Serializer: TDataSerializer;
begin
  if Filename='' then Exit;

  SubStorage:=TDataStorage.Create(stList);

  for i:=0 to DbTableInfoList.Count-1 do
  begin
    ItemStorage:=DbTableInfoToStorage((DbTableInfoList.Objects[i] as TDbTableInfo));
    SubStorage.Add('', ItemStorage);
  end;

  Storage:=TDataStorage.Create(stDictionary);
  Storage.Add('DataType', 'DbTableInfoList');
  Storage.Add('Items', SubStorage);

  Serializer:=TDataSerializerBencode.Create();
  Serializer.StorageToFile(Storage, Filename);
  FreeAndNil(Serializer);

end;

function TMdStorage.LoadFromFile(): Boolean;
var
  Storage, SubStorage, ItemStorage: TDataStorage;
  i: integer;
  Serializer: TDataSerializer;
  DbTableInfo: TDbTableInfo;
begin
  Result:=False;
  if Filename='' then Exit;
  Storage:=TDataStorage.Create(stDictionary);

  Serializer:=TDataSerializerBencode.Create();
  Result:=Serializer.StorageFromFile(Storage, Filename);
  FreeAndNil(Serializer);
  if not Result then Exit;
  Result:=False;

  if Storage.GetString('DataType') <> 'DbTableInfoList' then Exit;

  SubStorage:=Storage.GetObject('Items');
  if not Assigned(SubStorage) then Exit;

  for i:=0 to SubStorage.Count-1 do
  begin
    ItemStorage:=SubStorage.GetObject(i);
    DbTableInfo:=TDbTableInfo.Create();
    if DbTableInfoFromStorage(DbTableInfo, ItemStorage) then
    begin
      DbTableInfoList.AddObject(DbTableInfo.TableName, DbTableInfo);
    end;
  end;
  Result:=True;
end;

procedure TMdStorage.AfterConstruction();
begin
  inherited AfterConstruction;
  DbTableInfoList:=TStringList.Create();
end;

procedure TMdStorage.BeforeDestruction();
begin
  FreeAndNil(DbTableInfoList);
  inherited BeforeDestruction;
end;

function TMdStorage.DbTableInfoToStorage(ADbTableInfo: TDbTableInfo
  ): TDataStorage;
var
  Storage, SubStorage: TDataStorage;
  i: integer;
  TmpField: TDbFieldInfo;
begin
  Result:=TDataStorage.Create(stDictionary);
  if not Assigned(ADbTableInfo) then Exit;

  SubStorage:=TDataStorage.Create(stList);

  for i:=0 to ADbTableInfo.FieldsCount-1 do
  begin
    TmpField:=ADbTableInfo.Fields[i];
    Storage:=TDataStorage.Create(stDictionary);
    Storage.Add('Name', TmpField.FieldName);
    Storage.Add('Type', TmpField.FieldType);
    Storage.Add('Desc', TmpField.FieldDescription);
    SubStorage.Add('', Storage);
  end;

  Result.Add('DataType', 'DbTableInfo');
  Result.Add('DBName', ADbTableInfo.DBName);
  Result.Add('TableName', ADbTableInfo.TableName);
  Result.Add('TableDesc', ADbTableInfo.TableDescription);
  Result.Add('KeyFieldName', ADbTableInfo.KeyFieldName);
  Result.Add('Fields', SubStorage);

end;

function TMdStorage.DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo;
  AStorage: TDataStorage): Boolean;
var
  SubStorage: TDataStorage;
  SubStorageItem: TDataStorage;
  DbField: TDbFieldInfo;
  i: Integer;
begin
  Result:=False;
  if not Assigned(AStorage) then Exit;
  if AStorage.StorageType <> stDictionary then Exit;
  if AStorage.GetString('DataType') <> 'DbTableInfo' then Exit;
  ADbTableInfo.DBName:=AStorage.GetString('DBName');
  ADbTableInfo.TableName:=AStorage.GetString('TableName');
  ADbTableInfo.TableDescription:=AStorage.GetString('TableDesc');
  ADbTableInfo.KeyFieldName:=AStorage.GetString('KeyFieldName');
  SubStorage:=AStorage.GetObject('Fields');
  if Assigned(SubStorage) then
  begin
    for i:=0 to SubStorage.Count-1 do
    begin
      SubStorageItem:=SubStorage.GetObject(i);
      DbField:=ADbTableInfo.AddField(SubStorageItem.GetString('Name'), SubStorageItem.GetString('Type'));
      DbField.FieldDescription:=SubStorageItem.GetString('Desc');
    end;
  end;
  Result:=True;
end;

end.

