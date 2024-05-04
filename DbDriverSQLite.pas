unit DbDriverSQLite;
(*
SQLite driver for DbUnit

Usage example:
  GlobalDbManager.DbDriver := TDbDriverSQLite.Create(GlobalDbManager);
  (GlobalDbManager.DbDriver as TDbDriverSQLite).OnDebugSQL := @OnDebugSQLHandler;
  GlobalDbManager.DbDriver.Open(DB_PATH);
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbUnit, ZConnection, ZDataset, Variants;

type

  { TDbDriverSQLite }

  TDbDriverSQLite = class(TDbDriver)
  private
    db: TZConnection;
    Active: Boolean;
    procedure CheckTable(ATableInfo: TDbTableInfo);
  protected
    procedure DebugSQL(AMsg: string); virtual;
    procedure CreateTable(ATableInfo: TDbTableInfo);
    procedure RebuildTable(ATableInfo: TDbTableInfo);
  public
    constructor Create(AManager: TDbManager);
    //destructor Destroy(); override;
    function Open(ADbName: string): Boolean; override;
    function Close(): Boolean; override;
    function GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function GetDBItem(const AValue: string): TDBItem; override;
    function SetDBItem(AItem: TDBItem): Boolean; override;
    function DeleteDBItem(AItem: TDBItem): Boolean; override;
  end;


implementation

function DisarmStrToHex(const AStr: string): string;
var
  i: Integer;
begin
  Result := 'X''';
  for i := 1 to Length(AStr) do
  begin
    Result := Result + IntToHex(Ord(AStr[i]), 2);
  end;
  Result := Result + '''';
end;

function DisarmStr(const AStr: string): string;
begin
  if (Pos('\r', AStr) > 0) or (Pos('\n', AStr) > 0) then
     Result := DisarmStrToHex(AStr)
  else
  begin
    Result := '''' + StringReplace(AStr, '''', '''''', [rfReplaceAll]) + '''';
    if Pos(#13, Result) > 0 then
    begin
      Result := 'replace(' + StringReplace(Result, #13, '\r', [rfReplaceAll]) + ',''\r'',char(13))';
    end;
    if Pos(#10, Result) > 0 then
    begin
      Result := 'replace(' + StringReplace(Result, #10, '\n', [rfReplaceAll]) + ',''\n'',char(10))';
    end;
  end;
end;

function StreamToDbStr(AStream: TStream): string;
var
  b: Byte;
  n, nLen, nPos: Integer;
  //s: string;
begin
  nLen := 3 + (AStream.Size * 2);
  SetLength(Result, nLen);
  Result[1] := 'X';
  Result[2] := '''';
  AStream.Position := 0;
  // read to second half of result
  nPos := AStream.Size + 3;
  AStream.Read(Result[nPos], AStream.Size);
  n := 3;
  while n < nLen do
  begin
    //AStream.Read(b, SizeOf(b));
    //s := IntToHex(b, 2);
    b := Ord(Result[nPos]);
    if (b and $0F) < $A then
      Result[n+1] := Chr((b and $0F) + $30)  // Ord('0')
    else
      Result[n+1] := Chr((b and $0F) + $37); // Ord('A') - $A

    b := b shr 4;
    if (b and $0F) < $A then
      Result[n] := Chr((b and $0F) + $30)
    else
      Result[n] := Chr((b and $0F) + $37);

    Inc(n, 2);
    Inc(nPos);
  end;
  Result[n] := '''';
end;

function DateStr(const ADate: TDateTime): string;
begin
  Result := '''' + FormatDateTime('YYYY-MM-DD HH:NN:SS', ADate) + '''';
end;

// === TDbDriverSQLite ===
constructor TDbDriverSQLite.Create(AManager: TDbManager);
begin
  inherited Create(AManager);
  self.Active := False;
end;

procedure TDbDriverSQLite.CheckTable(ATableInfo: TDbTableInfo);
var
  sl: TStringList;
  s, sn, st, sql: string;
  i: Integer;
begin
  if not Assigned(db) then
    Exit;
  if ATableInfo.IsValid then
    Exit;
  //if Self.TablesList.IndexOf(TableInfo) >= 0 then
  //  Exit;

  // get table info
  //sql:='SELECT * FROM sqlite_master WHERE type=''table'' and name='''+TableInfo.TableName+'''';
  //DebugSQL(sql);
  //rs:=db.SchemaTableInfo(ATableInfo.TableName);

  sl := TStringList.Create();
  try
    DB.GetColumnNames('%'+ATableInfo.TableName+'%', '', sl);

    if sl.Count <= 0 then
    begin
      CreateTable(ATableInfo);
    end
    else
    begin
      {for i := 0 to sl.Count-1 do
      begin
        if ATableInfo.GetFieldIndex(sl[i]) < 0 then
        begin
          RebuildTable(ATableInfo);
          Break;
        end;
      end;}
      // new fields
      for i := 0 to ATableInfo.FieldsCount-1 do
      begin
        if sl.IndexOf(ATableInfo.FieldNames[i]) < 0 then
        begin
          RebuildTable(ATableInfo);
          Break;
        end;
      end;
    end;

  finally
    sl.Free;
  end;

  ATableInfo.IsValid := True;
  if Self.TablesList.IndexOf(ATableInfo) < 0 then
    Self.TablesList.Add(ATableInfo);
end;

procedure TDbDriverSQLite.DebugSQL(AMsg: string);
begin
  if Assigned(OnDebugSQL) then
    OnDebugSQL(AMsg);
end;

procedure TDbDriverSQLite.CreateTable(ATableInfo: TDbTableInfo);
var
  s, sn, st, sql: string;
  i: Integer;
begin
  s := '';
  for i := 0 to ATableInfo.FieldsCount - 1 do
  begin
    sn := ATableInfo.FieldNames[i];
    st := ATableInfo.Types[i];
    if Length(s) > 0 then
      s := s + ',';
    s := s + QuotedStr(sn);
    if sn = ATableInfo.KeyFieldName then
    begin
      s := s + ' INTEGER PRIMARY KEY NOT NULL';
      //ATableInfo.KeyFieldName := sn;
    end
    else if st = 'I' then
      s := s + ' INTEGER NOT NULL DEFAULT 0'
    else if st = 'N' then
      s := s + ' REAL NOT NULL DEFAULT 0'
    else if st = 'S' then
      s := s + ' TEXT NOT NULL DEFAULT '''''
    else if st = 'B' then
      s := s + ' INTEGER NOT NULL DEFAULT 0'
    else if st = 'D' then
      s := s + ' TEXT NOT NULL DEFAULT '''''
    //else if st = '' then // nothing;
    else if st[1] = 'L' then
      s := s + ' INTEGER';
  end;
  sql := 'CREATE TABLE ' + QuotedStr(ATableInfo.TableName) + ' (' + s + ')';
  DebugSQL(sql);
  try
    DB.ExecuteDirect(sql);
  finally
  end;
end;

procedure TDbDriverSQLite.RebuildTable(ATableInfo: TDbTableInfo);
var
  i: integer;
  sTmpTableName, sOrigTableName: string;
  Query: TZReadOnlyQuery;
  sn, sql: string;
begin
  if not Assigned(db) then Exit;
  // old table info
  sn := '';
  //rs:=db.SchemaTableInfo(sTableName);
  sql := 'SELECT * FROM ' + ATableInfo.TableName + ' LIMIT 1;';
  DebugSQL(sql);
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := db;
    Query.SQL.Text := sql;
    Query.Open();

    for i := 0 to Query.FieldCount-1 do
    begin
      if ATableInfo.GetFieldIndex(Query.Fields[i].FieldName) < 0 then Continue;
      if Length(sn) > 0 then
        sn := sn + ',';
      sn := sn + Query.Fields[i].FieldName;
    end;
  finally
    Query.Free();
  end;

  sOrigTableName := ATableInfo.TableName;
  sTmpTableName := ATableInfo.TableName + '_tmp';

  try
    // drop temp table (if exists)
    sql := 'DROP TABLE IF EXISTS ' + sTmpTableName + ';';
    DebugSQL(sql);
    db.ExecuteDirect(sql);

    // create new table
    ATableInfo.TableName := sTmpTableName;
    CreateTable(ATableInfo);

    // copy to new table
    sql := 'INSERT INTO ' + sTmpTableName + ' (' + sn + ') '
      + 'SELECT ' + sn + ' FROM ' + sOrigTableName + ';';
    DebugSQL(sql);
    db.ExecuteDirect(sql);

    // drop orig table
    sql := 'DROP TABLE ' + sOrigTableName + ';';
    DebugSQL(sql);
    db.ExecuteDirect(sql);

    // rename temp table to orig
    sql := 'ALTER TABLE ' + sTmpTableName + ' RENAME TO ' + sOrigTableName + ';';
    DebugSQL(sql);
    db.ExecuteDirect(sql);
  finally
    ATableInfo.TableName := sOrigTableName;
  end;
end;

function TDbDriverSQLite.Open(ADbName: string): Boolean;
begin
  DbName := ADbName;
  Result := True;
  if Assigned(db) then
    FreeAndNil(db);
  if ExtractFileExt(ADbName) = '' then
    ADbName := ADbName + '.db3';

  db := TZConnection.Create(nil);
  try
    db.Protocol := 'sqlite-3';
    {$ifndef CPU32}
    db.LibraryLocation := 'SQLite3_64.dll';
    {$endif}
    db.Database := ADbName;
    db.Connect;
  except
    FreeAndNil(db);
    Result := False;
  end;
  Active := Result;
end;

function TDbDriverSQLite.Close(): Boolean;
begin
  Result := True;
  TablesList.Clear();
  if not Active then
    Exit;
  if not Assigned(db) then
    Exit;
  DB.Disconnect;
  FreeAndNil(db);
end;

function TDbDriverSQLite.GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  //rs: IMkSqlStmt;
  Query: TZReadOnlyQuery;
  n, m: Integer;
  TmpID: TDbItemID;
  Item: TDbItem;
  fn, sql: string;
  fl: TStringList;
begin
  Result := False;
  if not Active then
    Exit;
  if not Assigned(AItemList) then
    Exit;
  if not Assigned(db) then
    Exit;

  CheckTable(AItemList.DbTableInfo);

  fl := TStringList.Create(); // filters
  try
    fl.CommaText := Filter;

    sql := 'SELECT * FROM ' + AnsiQuotedStr(AItemList.DbTableInfo.TableName, '"');
    // filters
    if fl.Count > 0 then
      sql := sql + ' WHERE ';
    for m := 0 to fl.Count - 1 do
    begin
      if m > 0 then
        sql := sql + ' AND ';
      sql := sql + AnsiQuotedStr(fl.Names[m], '"') + '=' + QuotedStr(fl.ValueFromIndex[m]);
    end;
    sql := sql + ' ORDER BY ' + AItemList.DbTableInfo.KeyFieldName;
    AItemList.SetFlag(LIST_FLAG_SORTED, True);
  finally
    FreeAndNil(fl);
  end;

  DebugSQL(sql);

  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := db;
    //rs:=db.Exec(sql);
    Query.SQL.Text := sql;
    Query.Open();
    while not Query.EOF do
    begin
      TmpID := Query.FieldValues[AItemList.DbTableInfo.KeyFieldName];

      Item := AItemList.GetItemByID(TmpID);
      if not Assigned(Item) and Assigned(AItemList.ParentList) then
      begin
        Item := AItemList.ParentList.GetItemByID(TmpID);
        if Assigned(Item) then
          AItemList.Add(Item);
      end;
      if not Assigned(Item) then
        Item := AItemList.NewItem();
      for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
      begin
        fn := AItemList.DbTableInfo.FieldNames[n]; // field name
        Item.SetValue(fn, VarToStrDef(Query.FieldValues[fn], ''));
      end;
      Query.Next();
    end;
    Result := True;

  finally
    FreeAndNil(Query);
  end;
end;

function TDbDriverSQLite.SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  i, n: Integer;
  Item: TDbItem;
  fn, iv, vl, sql: string;
begin
  Result := False;
  if (not Active) or (not Assigned(AItemList)) or (not Assigned(db)) then
    Exit;
  CheckTable(AItemList.DbTableInfo);

  for i := 0 to AItemList.Count - 1 do
  begin
    vl := '';
    Item := (AItemList[i] as TDbItem);
    for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
    begin
      fn := AItemList.DbTableInfo.FieldNames[n]; // field name
      iv := Item.GetValue(fn);                 // field value
      iv := QuotedStr(iv);
      if n > 0 then
        vl := vl + ',';
      vl := vl + iv;
      //vl:=vl+fn+'='''+iv+'''';
    end;
    sql := 'INSERT OR REPLACE INTO ' + AnsiQuotedStr(AItemList.DbTableInfo.TableName, '"') + ' VALUES (' + vl + ')';
    DebugSQL(sql);
    //sql:='UPDATE '+AItemList.DbTableInfo.TableName+' SET '+vl+' WHERE ROWID='+IntToStr(Item.ID);
    try
      DB.ExecuteDirect(sql);
      Result := True;
    finally
    end;
  end;
end;

function TDbDriverSQLite.GetDBItem(const AValue: string): TDBItem;
var
  sTableName, sItemID, fn, sql: string;
  i: Integer;
  TableInfo: TDbTableInfo;
  Query: TZReadOnlyQuery;
begin
  Result := nil;
  if not Assigned(db) then
    Exit;
  i := Pos('~', AValue);
  sTableName := Copy(AValue, 1, i - 1);
  sItemID := Copy(AValue, i + 1, MaxInt);
  TableInfo := Self.GetDbTableInfo(sTableName);
  if not Assigned(TableInfo) then
    Exit;

  sql := Format('SELECT * FROM %s WHERE %s = %s', [AnsiQuotedStr(TableInfo.TableName, '"'), TableInfo.KeyFieldName, QuotedStr(sItemID)]);
  DebugSQL(sql);

  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := db;
    Query.SQL.Text := sql;
    Query.Open();
    while not Query.EOF do
    begin
      Result := TDbItem.Create();
      for i := 0 to TableInfo.FieldsCount - 1 do
      begin
        fn := TableInfo.FieldNames[i];  // field name
        Result.SetValue(fn, Query.FieldValues[fn]);
      end;
      Query.Next();
    end;

  finally
    FreeAndNil(Query);
  end;
end;

function TDbDriverSQLite.SetDBItem(AItem: TDBItem): Boolean;
var
  n: Integer;
  TableInfo: TDbTableInfo;
  fn, iv, vl, sql: string;
begin
  Result := False;
  if (not Active) or (not Assigned(AItem)) or (not Assigned(db)) then
    Exit;

  TableInfo := AItem.DbTableInfo;
  if not Assigned(TableInfo) then
    Exit;
  CheckTable(TableInfo);

  vl := '';
  if AItem.IsNew then
  begin
    for n := 0 to TableInfo.FieldsCount - 1 do
    begin
      fn := TableInfo.FieldNames[n]; // field name
      iv := AItem.GetValue(fn);
      if vl <> '' then
        vl := vl + ',';
      vl := vl + QuotedStr(iv);
      //vl:=vl+fn+'='''+iv+'''';
    end;
    sql := 'INSERT OR REPLACE INTO ' + AnsiQuotedStr(TableInfo.TableName, '"') + ' VALUES (' + vl + ')';
  end
  else
  begin
    for n := 0 to TableInfo.FieldsCount - 1 do
    begin
      fn := TableInfo.FieldNames[n]; // field name
      if fn = TableInfo.KeyFieldName then
        Continue;
      iv := AItem.GetValue(fn);
      if vl <> '' then
        vl := vl + ', ';
      vl := vl + fn + '=' + QuotedStr(iv);
    end;
    sql := 'UPDATE ' + AnsiQuotedStr(TableInfo.TableName, '"') + ' SET ' + vl
         + ' WHERE ' + TableInfo.KeyFieldName + '=' + IntToStr(AItem.GetID);
  end;
  DebugSQL(sql);
  try
    DB.ExecuteDirect(sql);
    Result := True;

  finally
  end;
  AItem.SetFlag(ITEM_FLAG_NEW, False);
end;

function TDbDriverSQLite.DeleteDBItem(AItem: TDBItem): Boolean;
var
  TableInfo: TDbTableInfo;
  sql: string;
begin
  Result := False;
  if not Assigned(AItem) then Exit;
  TableInfo := AItem.DbTableInfo;
  sql := Format('DELETE FROM %s WHERE %s = %s', [AnsiQuotedStr(TableInfo.TableName, '"'), TableInfo.KeyFieldName, IntToStr(AItem.GetID())]);
  DebugSQL(sql);
  DB.ExecuteDirect(sql);
  Result := inherited DeleteDBItem(AItem);
end;


end.
