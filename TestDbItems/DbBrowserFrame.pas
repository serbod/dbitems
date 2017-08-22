unit DbBrowserFrame;

interface

uses
  SysUtils, Controls, Forms, ComCtrls, ToolWin, Classes, ExtCtrls, DbUnit;

type
  TFrameDbBrowser = class(TFrame)
    toolbarDbBrowser: TToolBar;
    btnRefresh: TToolButton;
    btnEditTable: TToolButton;
    panCenter: TPanel;
    tvDbTree: TTreeView;
    splH1: TSplitter;
    lvDbTable: TListView;
    procedure ToolButtonClick(Sender: TObject);
    procedure tvDbTreeChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    FDbItemList: TDbItemList;
    procedure Refresh();
  public
    { Public declarations }
    DbDriver: TDbDriver;
    property DbItemList: TDbItemList read FDbItemList;
  end;

implementation

//uses Main, MainFunc;

{$R *.lfm}

procedure TFrameDbBrowser.Refresh();
var
  i: integer;
  TableInfo: TDbTableInfo;
  TreeNode: TTreeNode;
begin
  tvDbTree.Items.Clear();
  for i := 0 to DbDriver.TablesList.Count - 1 do
  begin
    TableInfo := DbDriver.TablesList.GetItem(i);

    TreeNode := tvDbTree.Items.Add(nil, TableInfo.TableName);
    TreeNode.Data := TableInfo;
  end;
end;

procedure TFrameDbBrowser.ToolButtonClick(Sender: TObject);
begin
  if Sender = btnRefresh then
  begin
    Refresh();
  end
  else if Sender = btnEditTable then
  begin
    //MainFunc.OpenTableEdit(DbItemList);
  end
  else if Sender = btnEditTable then
  begin
  end
  else if Sender = btnEditTable then
  begin
  end;
end;

procedure TFrameDbBrowser.tvDbTreeChange(Sender: TObject; Node: TTreeNode);
var
  i, n: integer;
  TableInfo: TDbTableInfo;
  Column: TListColumn;
  DbItem, DbItem2: TDbItem;
  ListItem: TListItem;
  fn, ft, s, sl: string;
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
  if Assigned(FDbItemList) then
    FreeAndNil(FDbItemList);
  FDbItemList := TDbItemList.Create(TableInfo, DbDriver);
  //FDbItemList.DbTableInfo:=TableInfo;
  if not DbDriver.GetTable(FDbItemList) then
    Exit;

  for i := 0 to DbItemList.Count - 1 do
  begin
    DbItem := (DbItemList.Items[i] as TDbItem);
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
        DbItem2 := DbDriver.GetDBItem(sl);
        if Assigned(DbItem2) then
          s := s + '*' + DbItem2.GetName;
      end;

      if n = 0 then
      begin
        ListItem := lvDbTable.Items.Add();
        ListItem.Caption := s;
        Continue;
      end;
      ListItem.SubItems.Add(s);
    end;
  end;
  lvDbTable.Visible := True;
end;

end.
