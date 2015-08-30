unit DataStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDataStorageType = (stUnknown, stString, stInteger, stNumber, stList, stDictionary);

  { TDataStorage }
  TDataStorage = class(TInterfacedObject)
  private
    { [name:object] items storage }
    FItems: TStringList;
    function FGetCount(): integer;
  public
    { stUnknown, stString, stInteger, stNumber, stList, stDictionary }
    StorageType: TDataStorageType;
    { Value for (String, Integer, Number) types }
    Value: AnsiString;
    constructor Create(AStorageType: TDataStorageType);
    destructor Destroy; override;
    { Items count for (List, Dictionary) types }
    property Count: Integer read FGetCount;
    procedure Add(AName: string; AValue: TDataStorage);
    procedure Add(AName, AValue: string); overload;
    procedure Add(AName: string; AValue: Integer); overload;
    procedure Add(AName: string; AValue: Real); overload;
    procedure Add(AName: string; AValue: Boolean); overload;
    { Get storage item by name }
    function GetObject(AName: string): TDataStorage;
    { Get storage item by index }
    function GetObject(Index: integer): TDataStorage; overload;
    { Get name by index }
    function GetObjectName(Index: integer): string;
    { Get string by name (from dictionary). If name empty, get value }
    function GetString(AName: string = ''): string;
    function GetInteger(AName: string = ''): Integer;
    function GetCardinal(AName: string = ''): Cardinal;
    function GetReal(AName: string = ''): Real;
    function GetBool(AName: string = ''): Boolean;
    function HaveName(AName: string): Boolean;
  end;


  { TDataSerializer }

  TDataSerializer = class(TInterfacedObject)
  public
    // Serializer format/protocol name
    function GetName(): string; virtual;
    // Serialize storage to string
    function StorageToString(AStorage: TDataStorage): AnsiString; virtual;
    // De-serialize storage from string
    function StorageFromString(AStorage: TDataStorage; AString: AnsiString): Boolean; virtual;
    // Save storage to file. Filename must be without extension
    function StorageToFile(AStorage: TDataStorage; AFileName: string): Boolean; virtual;
    // Load storage from file. Filename must be without extension
    function StorageFromFile(AStorage: TDataStorage; AFileName: string): Boolean; virtual;
  end;

  { TDataSerializerBencode }
  {
  Bencode serializer
  integers: i<value>e
    i0e  i42e  i-42e
  strings: <lalie_len>:<value>
    3:ben  4:code
  lists: l<items>e (without any spaces)
    l i42e 3:ben 4:code e
  dictionaries: d<items>e  where items is <string_name><value>
    d 4:name 3:ben  4:code i42e e
  }
  TDataSerializerBencode = class(TDataSerializer)
  private
    function StorageToBencode(AStorage: TDataStorage): AnsiString;
    function ReadBencodeValue(AStorage: TDataStorage; AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): Boolean;
    function ReadBencodeIntegerStr(var AString: AnsiString; var APos: Cardinal;
      ALen: Cardinal): AnsiString;
    function ReadBencodeString(var AString: AnsiString; var APos: Cardinal;
      ALen: Cardinal): AnsiString;
    function ReadBencodeList(AStorage: TDataStorage; var AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): Boolean;
    function ReadBencodeDictionary(AStorage: TDataStorage; var AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): Boolean;
  public
    function GetName(): string; override;
    function StorageToString(AStorage: TDataStorage): AnsiString; override;
    function StorageFromString(AStorage: TDataStorage; AString: AnsiString
      ): Boolean; override;
    function StorageToFile(AStorage: TDataStorage; AFileName: string): Boolean;
      override;
    function StorageFromFile(AStorage: TDataStorage; AFileName: string
      ): Boolean; override;
  end;

implementation

function StreamToStr(AStream: TStream): AnsiString;
var
  ss: TStringStream;
begin
  Result:='';
  ss:=TStringStream.Create('');
  try
    AStream.Seek(0, soFromBeginning);
    ss.CopyFrom(AStream, AStream.Size);
    Result:=ss.DataString;
  finally
    ss.Free();
  end;
end;

function StrToStream(s: AnsiString; AStream: TStream): boolean;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(s);
  try
    ss.Seek(0, soFromBeginning);
    AStream.Seek(0, soFromBeginning);
    AStream.CopyFrom(ss, ss.Size);
    Result:=True;
  finally
    ss.Free();
  end;
end;

function StrToFile(FileName, Str: AnsiString): Boolean;
var
  fs: TFileStream;
begin
  Result:=False;
  try
    fs:=TFileStream.Create(FileName, fmCreate);
  except
    fs:=nil;
  end;

  if not Assigned(fs) then Exit;
  try
    StrToStream(Str, fs);
    Result:=True;
  finally
    FreeAndNil(fs);
  end;
end;

function FileToStr(FileName: string): AnsiString;
var
  fs: TFileStream;
begin
  Result:='';
  if not FileExists(FileName) then Exit;
  try
    fs:=TFileStream.Create(FileName, fmOpenRead);
  except
    fs:=nil;
  end;

  if not Assigned(fs) then Exit;
  try
    Result:=StreamToStr(fs);
  finally
    fs.Free();
  end;
end;

{ TDataStorage }

constructor TDataStorage.Create(AStorageType: TDataStorageType);
begin
  inherited Create();
  StorageType:=AStorageType;
  FItems:=TStringList.Create();
  FItems.OwnsObjects:=True;
end;

destructor TDataStorage.Destroy();
begin
  FItems.Free();
  inherited Destroy();
end;

function TDataStorage.FGetCount(): integer;
begin
  Result:=FItems.Count;
end;

procedure TDataStorage.Add(AName: string; AValue: TDataStorage);
begin
  if (StorageType=stDictionary) or (StorageType=stList) then
  begin
    FItems.AddObject(AName, AValue);
  end
  else
  begin
    // not valid for current storage type
  end;
end;

procedure TDataStorage.Add(AName, AValue: string);
var
  TmpItem: TDataStorage;
begin
  if (StorageType=stDictionary) or (StorageType=stList) then
  begin
    TmpItem:=TDataStorage.Create(stString);
    TmpItem.Value:=AValue;
    FItems.AddObject(AName, TmpItem)
  end
  else Self.Value:=AValue;
end;

procedure TDataStorage.Add(AName: string; AValue: Integer);
var
  TmpItem: TDataStorage;
begin
  if (StorageType=stDictionary) or (StorageType=stList) then
  begin
    TmpItem:=TDataStorage.Create(stInteger);
    TmpItem.Value:=IntToStr(AValue);
    FItems.AddObject(AName, TmpItem)
  end
  else Self.Value:=IntToStr(AValue);
end;

procedure TDataStorage.Add(AName: string; AValue: Real);
var
  TmpItem: TDataStorage;
begin
  if (StorageType=stDictionary) or (StorageType=stList) then
  begin
    TmpItem:=TDataStorage.Create(stNumber);
    TmpItem.Value:=FloatToStr(AValue);
    FItems.AddObject(AName, TmpItem);
  end
  else Self.Value:=FloatToStr(AValue);
end;

procedure TDataStorage.Add(AName: string; AValue: Boolean);
begin
  Self.Add(AName, BoolToStr(AValue, '1', '0'));
end;

function TDataStorage.GetObject(AName: string): TDataStorage;
var
  n: integer;
  TmpItem: TDataStorage;
begin
  Result:=nil;
  n:=FItems.IndexOf(AName);
  if n>=0 then
  begin
    TmpItem:=(FItems.Objects[n] as TDataStorage);
    if (TmpItem.StorageType in [stList, stDictionary]) then Result:=TmpItem;
  end;
end;

function TDataStorage.GetObject(Index: integer): TDataStorage;
begin
  Result:=nil;
  if (Index>=0) and (Index<Count) then
  begin
    Result:=(FItems.Objects[Index] as TDataStorage);
  end;
end;

function TDataStorage.GetObjectName(Index: integer): string;
begin
  Result:='';
  if (Index>=0) and (Index<FItems.Count) then Result:=FItems[Index];
end;

function TDataStorage.GetString(AName: string): string;
var
  n: integer;
  TmpItem: TDataStorage;
begin
  Result:='';
  if AName='' then Result:=Self.Value
  else
  begin
    n:=FItems.IndexOf(AName);
    if n<>-1 then
    begin
      TmpItem:=(FItems.Objects[n] as TDataStorage);
      Result:=TmpItem.Value;
      //if TmpItem.StorageType=stString then Result:=TmpItem.Value;
    end;
  end;
end;

function TDataStorage.GetInteger(AName: string): Integer;
begin
  Result:=StrToIntDef(GetString(AName), 0);
end;

function TDataStorage.GetCardinal(AName: string): Cardinal;
begin
  Result:=StrToQWordDef(GetString(AName), 0);
end;

function TDataStorage.GetReal(AName: string): Real;
begin
  Result:=StrToFloatDef(GetString(AName), 0);
end;

function TDataStorage.GetBool(AName: string): Boolean;
begin
  Result:=(GetString(AName)='1');
end;

function TDataStorage.HaveName(AName: string): Boolean;
begin
  Result:=(FItems.IndexOf(AName)<>-1);
end;

{ TDataSerializer }

function TDataSerializer.GetName: string;
begin
  Result:='NONE';
end;

function TDataSerializer.StorageToString(AStorage: TDataStorage): AnsiString;
begin
  Result:='';
end;

function TDataSerializer.StorageFromString(AStorage: TDataStorage;
  AString: AnsiString): Boolean;
begin
  Result:=False;
end;

function TDataSerializer.StorageToFile(AStorage: TDataStorage; AFileName: string
  ): Boolean;
begin
  Result:=False;
end;

function TDataSerializer.StorageFromFile(AStorage: TDataStorage;
  AFileName: string): Boolean;
begin
  Result:=False;
end;

{ TDataSerializerBencode }

function TDataSerializerBencode.GetName: string;
begin
  Result:='BENCODE';
end;

function TDataSerializerBencode.StorageToBencode(AStorage: TDataStorage
  ): AnsiString;
var
  sName: AnsiString;
  SubItem: TDataStorage;
  i: integer;
begin
  Result:='';
  if AStorage.StorageType=stString then
  begin
    Result:=Result+IntToStr(Length(AStorage.Value))+':'+AStorage.Value;
  end;

  if AStorage.StorageType=stNumber then
  begin
    Result:=Result+IntToStr(Length(AStorage.Value))+':'+AStorage.Value;
  end;

  if AStorage.StorageType=stInteger then
  begin
    Result:=Result+'i'+AStorage.Value+'e';
  end;

  if AStorage.StorageType=stDictionary then
  begin
    Result:=Result+'d';
    for i:=0 to AStorage.Count-1 do
    begin
      sName:=AStorage.GetObjectName(i);
      SubItem:=(AStorage.GetObject(i) as TDataStorage);
      // name
      Result:=Result+IntToStr(Length(sName))+':'+sName;
      // value
      Result:=Result+StorageToBencode(SubItem);
    end;
    Result:=Result+'e';
  end;

  if AStorage.StorageType=stList then
  begin
    Result:=Result+'l';
    for i:=0 to AStorage.Count-1 do
    begin
      SubItem:=(AStorage.GetObject(i) as TDataStorage);
      // value
      Result:=Result+StorageToBencode(SubItem);
    end;
    Result:=Result+'e';
  end;
end;

function TDataSerializerBencode.ReadBencodeIntegerStr(var AString: AnsiString;
  var APos: Cardinal; ALen: Cardinal): AnsiString;
begin
  Result:='';
  if AString[APos]='i' then Inc(APos) else Exit;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Break
    end;
    Result:=Result+AString[APos];
    Inc(APos);
  end;
end;

function TDataSerializerBencode.ReadBencodeString(var AString: AnsiString;
  var APos: Cardinal; ALen: Cardinal): AnsiString;
var
  sValue: AnsiString;
  ValueLen: Cardinal;
begin
  Result:='';
  sValue:='';
  while APos<=ALen do
  begin
    if AString[APos]=':' then
    begin
      ValueLen:=StrToIntDef(sValue, 0);
      Result:=Copy(AString, APos+1, ValueLen);
      APos:=APos+ValueLen+1;
      Exit;
    end;
    sValue:=sValue+AString[APos];
    Inc(APos);
  end;
end;

function TDataSerializerBencode.ReadBencodeDictionary(AStorage: TDataStorage;
  var AString: AnsiString; var APos: Cardinal; ALen: Cardinal): Boolean;
var
  sName: AnsiString;
  SubStorage: TDataStorage;
begin
  Result:=False;
  if AString[APos]='d' then Inc(APos) else Exit;
  AStorage.StorageType:=stDictionary;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Result:=True;
      Exit;
    end;
    sName:=ReadBencodeString(AString, APos, ALen);
    SubStorage:=TDataStorage.Create(stUnknown);
    if ReadBencodeValue(SubStorage, AString, APos, ALen) then AStorage.Add(sName, SubStorage);
  end;
end;

function TDataSerializerBencode.ReadBencodeList(AStorage: TDataStorage;
  var AString: AnsiString; var APos: Cardinal; ALen: Cardinal): Boolean;
var
  SubStorage: TDataStorage;
begin
  Result:=False;
  if AString[APos]='l' then Inc(APos) else Exit;
  AStorage.StorageType:=stList;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Result:=True;
      Exit;
    end;
    SubStorage:=TDataStorage.Create(stUnknown);
    if ReadBencodeValue(SubStorage, AString, APos, ALen) then AStorage.Add('', SubStorage);
  end;
end;

function TDataSerializerBencode.ReadBencodeValue(AStorage: TDataStorage;
  AString: AnsiString; var APos: Cardinal; ALen: Cardinal): Boolean;
begin
  Result:=False;
  if not Assigned(AStorage) then Exit;
  if APos<=ALen then
  begin
    if AString[APos]='i' then
    begin
      // read integer value
      AStorage.StorageType:=stInteger;
      AStorage.Value:=ReadBencodeIntegerStr(AString, APos, ALen);
      Result:=True;
    end

    else if Pos(AString[APos], '0123456789')>0 then
    begin
      // read string value
      AStorage.StorageType:=stString;
      AStorage.Value:=ReadBencodeString(AString, APos, ALen);
      Result:=True;
    end

    else if AString[APos]='d' then
    begin
      // read dictionary value
      ReadBencodeDictionary(AStorage, AString, APos, ALen);
      Result:=True;
    end

    else if AString[APos]='l' then
    begin
      // read list value
      ReadBencodeList(AStorage, AString, APos, ALen);
      Result:=True;
    end

    else
    begin
      // error
      Exit;
    end;
  end;
end;

function TDataSerializerBencode.StorageToString(AStorage: TDataStorage
  ): AnsiString;
begin
  Result:=StorageToBencode(AStorage);
end;

function TDataSerializerBencode.StorageFromString(AStorage: TDataStorage;
  AString: AnsiString): Boolean;
var
  n: Cardinal;
begin
  n:=1;
  Result:=ReadBencodeValue(AStorage, AString, n, Length(AString));
end;

function TDataSerializerBencode.StorageToFile(AStorage: TDataStorage;
  AFileName: string): Boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  Result:=StrToFile(AFileName+'.be', Self.StorageToString(AStorage));
end;

function TDataSerializerBencode.StorageFromFile(AStorage: TDataStorage;
  AFileName: string): Boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  Result:=Self.StorageFromString(AStorage, FileToStr(AFileName+'.be'));
end;


end.

