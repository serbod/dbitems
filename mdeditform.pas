unit MdEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ExtCtrls, DbUnit, MdUnit;

type

  { TFormMdEditor }

  TFormMdEditor = class(TForm)
    actTableAdd: TAction;
    actTableDel: TAction;
    actTableFieldAdd: TAction;
    actTableFieldDel: TAction;
    actTableFieldEdit: TAction;
    actLoadMdFromFile: TAction;
    actSaveMdToFile: TAction;
    alMdTree: TActionList;
    gbMdTree: TGroupBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PanSelectedItem: TPanel;
    pmMdTree: TPopupMenu;
    tvMdTree: TTreeView;
    procedure actLoadMdFromFileExecute(Sender: TObject);
    procedure actTableAddExecute(Sender: TObject);
    procedure actTableFieldAddExecute(Sender: TObject);
    procedure actSaveMdToFileExecute(Sender: TObject);
    procedure actTableFieldDelExecute(Sender: TObject);
    procedure actTableFieldEditExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvMdTreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    DbTablesNode: TTreeNode;
    procedure ItemRenameHandler(Sender: TObject);
    function FieldTypeToIconIndex(AFieldType: string): Integer;
  public
    { public declarations }
    MdStorage: TMdStorage;
    SelectedDbTableInfo: TDbTableInfo;
    SelectedDbFieldInfo: TDbFieldInfo;
    procedure Reset();
    function AddItemToMdTree(AParentNode: TTreeNode; AName: string; AImageIndex: integer; AObject: TObject): TTreeNode;
    procedure UpdateDbTableTree(AItem: TDbTableInfo);
  end;

var
  FormMdEditor: TFormMdEditor;

const
  ciIconItems = -1;
  ciIconDbTable = -1;
  ciIconItemInteger = -1;
  ciIconItemNumber = -1;
  ciIconItemString = -1;
  ciIconItemDateTime = -1;
  ciIconItemLink = -1;
  ciIconItemBinaryData = -1;

implementation

uses EditTableFieldFrame, EditTableFrame;

{$R *.lfm}

{ TFormMdEditor }

procedure TFormMdEditor.actTableAddExecute(Sender: TObject);
var
  DbTableInfo: TDbTableInfo;
begin
  // add new table
  DbTableInfo:=TDbTableInfo.Create();
  DbTableInfo.TableName:='new_table';
  MdStorage.DbTableInfoList.AddObject(DbTableInfo.TableName, DbTableInfo);

  // update metadata tree
  UpdateDbTableTree(DbTableInfo);
end;

procedure TFormMdEditor.actLoadMdFromFileExecute(Sender: TObject);
begin
  Reset();
end;

procedure TFormMdEditor.actTableFieldAddExecute(Sender: TObject);
begin
  // add field to selected table
  if not Assigned(SelectedDbTableInfo) then Exit;
  SelectedDbTableInfo.AddField('new_field', 'S');

  // update metadata tree
  UpdateDbTableTree(SelectedDbTableInfo);
end;

procedure TFormMdEditor.actTableFieldEditExecute(Sender: TObject);
begin
  if not Assigned(SelectedDbTableInfo) then Exit;

end;

procedure TFormMdEditor.actSaveMdToFileExecute(Sender: TObject);
begin
  MdStorage.SaveToFile();
end;

procedure TFormMdEditor.actTableFieldDelExecute(Sender: TObject);
begin
  if not Assigned(SelectedDbFieldInfo) then Exit;
  if not Assigned(SelectedDbTableInfo) then Exit;
  SelectedDbTableInfo.DeleteField(SelectedDbFieldInfo.FieldName);
  UpdateDbTableTree(SelectedDbTableInfo);
end;

procedure TFormMdEditor.FormCreate(Sender: TObject);
begin
  Reset();
end;

procedure TFormMdEditor.tvMdTreeSelectionChanged(Sender: TObject);
var
  Obj: TObject;
  i: Integer;
  TmpFrame: TFrame;
begin
  if not Assigned(tvMdTree.Selected) then Exit;
  if not Assigned(tvMdTree.Selected.Data) then Exit;
  Obj:=TObject(tvMdTree.Selected.Data);

  // Clear item controls
  SelectedDbFieldInfo:=nil;
  SelectedDbTableInfo:=nil;
  for i:=0 to PanSelectedItem.ControlCount-1 do
  begin
    PanSelectedItem.Controls[i].Free();
  end;

  // Table selected
  if (Obj is TDbTableInfo) then
  begin
    SelectedDbTableInfo:=(Obj as TDbTableInfo);
    TmpFrame:=TFrameEditTable.Create(PanSelectedItem);
    TmpFrame.Parent:=PanSelectedItem;
    TmpFrame.Align:=alClient;
    (TmpFrame as TFrameEditTable).MdStorage:=MdStorage;
    (TmpFrame as TFrameEditTable).OnItemRename:=@ItemRenameHandler;
    (TmpFrame as TFrameEditTable).DbTableInfo:=SelectedDbTableInfo;
  end;

  // Field selected
  if (Obj is TDbFieldInfo) then
  begin
    SelectedDbFieldInfo:=(Obj as TDbFieldInfo);
    SelectedDbTableInfo:=SelectedDbFieldInfo.TableInfo;
    // show frame
    TmpFrame:=TFrameEditTableField.Create(PanSelectedItem);
    TmpFrame.Parent:=PanSelectedItem;
    TmpFrame.Align:=alClient;
    (TmpFrame as TFrameEditTableField).MdStorage:=MdStorage;
    (TmpFrame as TFrameEditTableField).OnItemRename:=@ItemRenameHandler;
    (TmpFrame as TFrameEditTableField).DbFieldInfo:=SelectedDbFieldInfo;
  end;
end;

procedure TFormMdEditor.ItemRenameHandler(Sender: TObject);
var
  Obj: TObject;
begin
  if not Assigned(tvMdTree.Selected) then Exit;
  if not Assigned(tvMdTree.Selected.Data) then Exit;
  Obj:=TObject(tvMdTree.Selected.Data);

  // Field selected
  if (Obj is TDbFieldInfo) then
  begin
    tvMdTree.Selected.Text:=(Obj as TDbFieldInfo).FieldName;
    tvMdTree.Selected.ImageIndex:=FieldTypeToIconIndex( (Obj as TDbFieldInfo).FieldType );
  end;

  // Table selected
  if (Obj is TDbTableInfo) then
  begin
    tvMdTree.Selected.Text:=(Obj as TDbTableInfo).TableName;
    //tvMdTree.Selected.ImageIndex:=FieldTypeToIconIndex( (Obj as TDbFieldInfo).FieldType );
  end;
end;

function TFormMdEditor.FieldTypeToIconIndex(AFieldType: string): Integer;
begin
  Result:=ciIconItemString;
  if AFieldType='S' then Result:=ciIconItemString
  else if AFieldType='I' then Result:=ciIconItemInteger
  else if AFieldType='T' then Result:=ciIconItemDateTime
  else if AFieldType='N' then Result:=ciIconItemNumber
  else if AFieldType='D' then Result:=ciIconItemBinaryData
  else if Copy(AFieldType, 1, 1)='L' then Result:=ciIconItemLink;
end;

procedure TFormMdEditor.Reset();
var
  i: Integer;
begin
  MdStorage:=TMdStorage.Create();
  MdStorage.Filename:='metadata';
  MdStorage.LoadFromFile();

  tvMdTree.Items.BeginUpdate();

  // clear tree
  tvMdTree.Items.Clear();

  // Add root folders to tree
  DbTablesNode:=AddItemToMdTree(nil, 'DB Tables', ciIconItems, nil);

  // fill db tables list
  for i:=0 to MdStorage.DbTableInfoList.Count-1 do
  begin
    UpdateDbTableTree( (MdStorage.DbTableInfoList.Objects[i] as TDbTableInfo) );
  end;
  tvMdTree.Items.EndUpdate();
end;

function TFormMdEditor.AddItemToMdTree(AParentNode: TTreeNode; AName: string;
  AImageIndex: integer; AObject: TObject): TTreeNode;
begin
  Result:=tvMdTree.Items.AddChild(AParentNode, Name);
  Result.Data:=AObject;
  Result.Text:=AName;
  Result.ImageIndex:=AImageIndex;
  Result.StateIndex:=AImageIndex;
end;

procedure TFormMdEditor.UpdateDbTableTree(AItem: TDbTableInfo);
var
  i, n: Integer;
  DbTableNode, FieldNode: TTreeNode;
  FieldType: string;
begin
  if not Assigned(AItem) then Exit;
  tvMdTree.Items.BeginUpdate();

  // find table node
  DbTableNode:=tvMdTree.Items.FindNodeWithData(AItem);
  if not Assigned(DbTableNode) then
  begin
    // not exists, create db table node
    DbTableNode:=AddItemToMdTree(DbTablesNode, AItem.TableName, ciIconDbTable, AItem);
  end;

  // re-create table fields nodes
  DbTableNode.DeleteChildren();

  for i:=0 to AItem.FieldsCount-1 do
  begin
    n:=FieldTypeToIconIndex(AItem.Types[i]);
    FieldNode:=AddItemToMdTree(DbTableNode, AItem.Names[i], n, AItem.Fields[i]);
  end;
  tvMdTree.Items.EndUpdate();
end;

end.

