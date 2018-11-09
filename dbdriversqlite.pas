unit DbDriverSqlite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbUnit, sqldb, sqlite3conn;

type

  { TDbDriverSQLite }

  TDbDriverSQLite = class(TDbDriver)
  private
    db: TSQLite3Connection;
    Active: Boolean;
    procedure CheckTable(TableInfo: TDbTableInfo);
  protected
    procedure DebugSQL(AMsg: string); virtual;
  public
    constructor Create();
    //destructor Destroy(); override;
    function Open(ADbName: string): Boolean; override;
    function Close(): Boolean; override;
    function GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function SetTable(AItemList: TDbItemList; Filter: string = ''): Boolean; override;
    function GetDBItem(const AValue: string): TDBItem; override;
    function SetDBItem(AItem: TDBItem): Boolean; override;
  end;


implementation

// === TDbDriverSQLite ===
constructor TDbDriverSQLite.Create();
begin
  inherited Create();
  self.Active := False;
end;

procedure TDbDriverSQLite.CheckTable(TableInfo: TDbTableInfo);
var
  sl: TStringList;
  s, sn, st, sql: string;
  i: Integer;
begin
  if not Assigned(db) then
    Exit;
  if TableInfo.Valid then
    Exit;
  if Self.TableInfoList.IndexOf(TableInfo) >= 0 then
    Exit;

  // get table info
  //sql:='SELECT * FROM sqlite_master WHERE type=''table'' and name='''+TableInfo.TableName+'''';
  //DebugSQL(sql);
  //rs:=db.SchemaTableInfo(TableInfo.TableName);

  sl := TStringList.Create();
  try
    DB.GetFieldNames(TableInfo.TableName, sl);

    if sl.Count <= 0 then
    begin
      s := '';
      for i := 0 to TableInfo.FieldsCount - 1 do
      begin
        sn := TableInfo.FieldNames[i];
        st := TableInfo.Types[i];
        if Length(s) > 0 then
          s := s + ',';
        s := s + '''' + sn + '''';
        if sn = 'id' then
        begin
          s := s + ' INTEGER PRIMARY KEY NOT NULL';
          TableInfo.KeyFieldName := sn;
        end
        else if st = 'I' then
          s := s + ' INTEGER NOT NULL'
        else if st = 'S' then
          s := s + ' TEXT NOT NULL'
        else if st = 'B' then
          s := s + ' INTEGER NOT NULL'
        else if st = 'D' then
          s := s + ' TEXT NOT NULL'
        //else if st = '' then // nothing;
        else if st[1] = 'L' then
          s := s + ' INTEGER NOT NULL';
      end;
      sql := 'CREATE TABLE ''' + TableInfo.TableName + ''' (' + s + ')';
      DebugSQL(sql);
      try
        DB.ExecuteDirect(sql);
      finally
      end;
    end;

  finally
    sl.Free;
  end;

  TableInfo.Valid := True;
  Self.TableInfoList.Add(TableInfo);
end;

procedure TDbDriverSQLite.DebugSQL(AMsg: string);
begin
  if Assigned(OnDebugSQL) then
    OnDebugSQL(AMsg);
end;

function TDbDriverSQLite.Open(ADbName: string): Boolean;
begin
  DbName := ADbName;
  Result := True;
  if Assigned(db) then
    FreeAndNil(db);
  db := TSQLite3Connection.Create(nil);
  try
    DB.DatabaseName := ADbName + '.sqlite';
    DB.Open();
  except
    FreeAndNil(db);
    Result := False;
  end;
  Active := Result;
end;

function TDbDriverSQLite.Close(): Boolean;
begin
  Result := True;
  TableInfoList.Clear();
  if not Active then
    Exit;
  if not Assigned(db) then
    Exit;
  DB.Close();
  FreeAndNil(db);
end;

function TDbDriverSQLite.GetTable(AItemList: TDbItemList; Filter: string = ''): Boolean;
var
  //rs: IMkSqlStmt;
  Query: TSQLQuery;
  i, n, m: Integer;
  Item: TDbItem;
  fn, sql: string;
  fl: TStringList;
  FilterOk: Boolean;
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

    sql := 'SELECT * FROM "' + AItemList.DbTableInfo.TableName + '"';
    // filters
    if fl.Count > 0 then
      sql := sql + ' WHERE ';
    for m := 0 to fl.Count - 1 do
    begin
      if m > 0 then
        sql := sql + ' AND ';
      sql := sql + '"' + fl.Names[m] + '"="' + fl.ValueFromIndex[m] + '"';
    end;
  finally
    FreeAndNil(fl);
  end;

  DebugSQL(sql);

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := db;
    //rs:=db.Exec(sql);
    Query.SQL.Text := sql;
    Query.Open();
    while not Query.EOF do
    begin
      i := Query.FieldValues['id'];

      Item := AItemList.GetItemByID(i);
      if not Assigned(Item) then
        Item := AItemList.NewItem();
      for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
      begin
        fn := AItemList.DbTableInfo.FieldNames[n]; // field name
        Item.SetValue(fn, Query.FieldValues[fn]);
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
    Item := AItemList.GetItem(i);
    for n := 0 to AItemList.DbTableInfo.FieldsCount - 1 do
    begin
      fn := AItemList.DbTableInfo.FieldNames[n]; // field name
      iv := Item.GetValue(fn);                 // field value
      if n > 0 then
        vl := vl + ',';
      vl := vl + '"' + iv + '"';
      //vl:=vl+fn+'='''+iv+'''';
    end;
    sql := 'INSERT OR REPLACE INTO "' + AItemList.DbTableInfo.TableName + '" VALUES (' + vl + ')';
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
  Query: TSQLQuery;
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

  sql := 'SELECT * FROM ' + TableInfo.TableName + ' WHERE id="' + sItemID + '"';
  DebugSQL(sql);

  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := db;
    Query.SQL.Text := sql;
    Query.Open();
    while not Query.EOF do
    begin
      Result := TDbItem.Create(TableInfo);
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
  Item: TDbItem;
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
  for n := 0 to TableInfo.FieldsCount - 1 do
  begin
    fn := TableInfo.FieldNames[n]; // field name
    iv := AItem.GetValue(fn);
    if n > 0 then
      vl := vl + ',';
    vl := vl + '"' + iv + '"';
    //vl:=vl+fn+'='''+iv+'''';
  end;
  sql := 'INSERT OR REPLACE INTO "' + TableInfo.TableName + '" VALUES (' + vl + ')';
  DebugSQL(sql);
  //sql:='UPDATE '+AItemList.DbTableInfo.TableName+' SET '+vl+' WHERE ROWID='+IntToStr(Item.ID);
  try
    DB.ExecuteDirect(sql);
    Result := True;

  finally
  end;
end;


end.
