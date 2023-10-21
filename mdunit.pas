unit MdUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbUnit, Variants, VarDicts, VarLists, BencodeUnit;

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
    function DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): Variant;
    function DbTableInfoFromStorage(ADbTableInfo: TDbTableInfo;
      AStorage: Variant): Boolean;
  end;

implementation

{ TMdStorage }

procedure TMdStorage.SaveToFile();
var
  Storage, SubStorage, ItemStorage: Variant;
  i: Integer;
begin
  if Filename = '' then
    Exit;

  SubStorage := VarListCreate;

  for i := 0 to DbTableInfoList.Count - 1 do
  begin
    ItemStorage := DbTableInfoToStorage((DbTableInfoList.Objects[i] as TDbTableInfo));
    SubStorage.SetValue(ItemStorage);
  end;

  Storage := VarDictCreate;
  Storage.SetValue('DbTableInfoList', 'DataType');
  Storage.SetValue(SubStorage, 'Items');

  VariantToFileBencode(Storage, Filename);
end;

function TMdStorage.LoadFromFile(): Boolean;
var
  Storage: Variant;
  SubStorage, ItemStorage: Variant;
  s: string;
  i: Integer;
  DbTableInfo: TDbTableInfo;
begin
  Result := False;
  if Filename = '' then
    Exit;

  Storage := VariantFromFileBencode(Filename);
  s := VarTypeAsText(VarType(Storage));
  if (not VarIsDict(Storage)) then
    Exit;
  s := Storage;
  if (not VarIsDict(Storage)) or (Storage.DataType <> 'DbTableInfoList') then
    Exit;

  SubStorage := Storage.Items;
  if not VarIsList(SubStorage) then
    Exit;

  for i := 0 to SubStorage.GetCount - 1 do
  begin
    ItemStorage := SubStorage.GetValue(i);
    DbTableInfo := TDbTableInfo.Create(nil);
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

function TMdStorage.DbTableInfoToStorage(ADbTableInfo: TDbTableInfo): Variant;
var
  Storage, SubStorage: Variant;
  i: Integer;
  TmpField: TDbFieldInfo;
begin
  Result := VarDictCreate;
  if not Assigned(ADbTableInfo) then
    Exit;

  SubStorage := VarListCreate;

  for i := 0 to ADbTableInfo.FieldsCount - 1 do
  begin
    TmpField := ADbTableInfo.Fields[i];
    Storage := VarDictCreate;
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
  AStorage: Variant): Boolean;
var
  vFieldList, vField: Variant;
  DbField: TDbFieldInfo;
  i: Integer;
begin
  Result := False;
  NullStrictConvert := False;
  if (not VarIsDict(AStorage))
  or (AStorage.DataType <> 'DbTableInfo') then
    Exit;
  ADbTableInfo.DBName := AStorage.DBName;
  ADbTableInfo.TableName := AStorage.TableName;
  ADbTableInfo.TableDescription := AStorage.TableDesc;
  ADbTableInfo.KeyFieldName := AStorage.KeyFieldName;
  vFieldList := AStorage.Fields;
  if VarIsList(vFieldList) then
  begin
    for i := 0 to vFieldList.GetCount - 1 do
    begin
      vField := vFieldList.GetValue(i);
      DbField := ADbTableInfo.AddField(vField.Name, vField.GetValue('Type'));
      DbField.FieldDescription := vField.Desc;
      DbField.IsIndexed := vField.GetValue('Indexed');
    end;
  end;
  Result := True;
end;

end.
