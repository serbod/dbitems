unit DbBrowserFrame;

interface

uses
  SysUtils, Controls, Forms, ComCtrls, Classes, ExtCtrls, Menus,
  DbUnit;

type

  { TFrameDbBrowser }

  TFrameDbBrowser = class(TFrame)
    miEditTable: TMenuItem;
    miRefresh: TMenuItem;
    pmItems: TPopupMenu;
    panCenter: TPanel;
    tvDbTree: TTreeView;
    splH1: TSplitter;
    lvDbTable: TListView;
    procedure lvDbTableData(Sender: TObject; Item: TListItem);
    procedure ToolButtonClick(Sender: TObject);
    procedure tvDbTreeChange(Sender: TObject; Node: TTreeNode);
    procedure tvDbTreeDblClick(Sender: TObject);
  private
    { Private declarations }
    FDbItemList: TDbItemList;
    FOnEditTable: TNotifyEvent;

    function GetTableName: string;
  public
    { Public declarations }
    DbManager: TDbManager;
    procedure Refresh();

    property DbItemList: TDbItemList read FDbItemList;
    property TableName: string read GetTableName;

    property OnEditTable: TNotifyEvent read FOnEditTable write FOnEditTable;
  end;

implementation

{$R *.lfm}

procedure TFrameDbBrowser.Refresh();
var
  i: integer;
  TableInfo: TDbTableInfo;
  TreeNode: TTreeNode;
  s: string;
begin
  tvDbTree.Items.Clear();
  for i := 0 to DbManager.TableInfoList.Count - 1 do
  begin
    TableInfo := DbManager.TableInfoList.GetItem(i);

    s := TableInfo.TableName;
    if TableInfo.TableDescription <> '' then
      s := s + ' (' + TableInfo.TableDescription + ')';
    TreeNode := tvDbTree.Items.Add(nil, s);
    TreeNode.Data := TableInfo;
  end;
end;

procedure TFrameDbBrowser.ToolButtonClick(Sender: TObject);
begin
  if Sender = miRefresh then
  begin
    Refresh();
  end
  else if Sender = miEditTable then
  begin
    if Assigned(OnEditTable) then OnEditTable(Self);
  end;
end;

procedure TFrameDbBrowser.lvDbTableData(Sender: TObject; Item: TListItem);
var
  n: integer;
  TableInfo: TDbTableInfo;
  DbItem, DbItem2: TDbItem;
  fn, ft, s, sl: string;
begin
  if Assigned(Item) and Assigned(FDbItemList) and (Item.Index < FDbItemList.Count) then
  begin
    DbItem := (DbItemList.Items[Item.Index] as TDbItem);
    TableInfo := DbItem.DbTableInfo;
    for n := 0 to TableInfo.FieldsCount - 1 do
    begin
      fn := TableInfo.FieldNames[n];
      ft := TableInfo.Types[n];
      s := DbItem.GetValue(fn);
      if ft = '' then
      else if ft[1] = 'L' then
      begin
        // Link
        sl := Copy(ft, 3, maxint) + '~' + s;
        DbItem2 := DbManager.GetDBItem(sl);
        if Assigned(DbItem2) then
          s := s + '*' + DbItem2.GetName;
      end;

      if n = 0 then
        Item.Caption := s
      else
        Item.SubItems.Add(s);
    end;
  end;
end;

procedure TFrameDbBrowser.tvDbTreeChange(Sender: TObject; Node: TTreeNode);
var
  i: integer;
  TableInfo: TDbTableInfo;
  Column: TListColumn;
begin
  if not Assigned(Node) then
    Exit;
  if not Assigned(Node.Data) then
    Exit;
  TableInfo := TDbTableInfo(Node.Data);

  lvDbTable.Visible := False;
  lvDbTable.Clear();
  lvDbTable.Columns.Clear();

  // Columns
  for i := 0 to TableInfo.FieldsCount - 1 do
  begin
    Column := lvDbTable.Columns.Add();
    Column.Caption := TableInfo.FieldNames[i];
    Column.Width := 100;
  end;

  // Rows
  FDbItemList := TableInfo.ItemsCache;
  //if not DbDriver.GetTable(FDbItemList) then
  //  Exit;
  if lvDbTable.Items.Count <> DbItemList.Count then
    lvDbTable.Items.Count := DbItemList.Count;

  lvDbTable.Visible := True;
end;

procedure TFrameDbBrowser.tvDbTreeDblClick(Sender: TObject);
begin
  if Assigned(OnEditTable) then OnEditTable(Self);
end;

function TFrameDbBrowser.GetTableName: string;
begin
  if Assigned(DbItemList) then
    Result := DbItemList.DbTableInfo.TableName
  else
    Result := '';
end;

end.
