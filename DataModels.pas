unit DataModels;

{ Author: Sergey Bodrov (serbod@gmail.com) 2010-2017 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls;

type

  TListItemRec = class
  public
    ListObj: TObject;
    Index: Integer;
  end;

  { TListItemRecList }

  TListItemRecList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TListItemRec;
    function FindListItem(AListObj: TObject; AIndex: Integer): Integer;
    function AddListItem(AListObj: TObject; AIndex: Integer): TListItemRec;
  end;

  { TDataModel }

  TDataModel = class(TComponent)
  private
    FListItemRecList: TListItemRecList;
    procedure OnChangeHandler(Sender: TObject);
    procedure ListViewOnDeletionHandler(Sender: TObject; Item: TListItem);
  protected
    FControls: TList;
    FValue: string;
    FAllowChange: Boolean;
    procedure SetValue(const AValue: string);
    procedure UpdateControl(AItem: TObject);
    procedure UpdateListItem(AItem: TObject; AIndex: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    { Add control as model observer }
    procedure AddControl(AControl: TControl);
    { Add list item as model observer
      for TListBox set AIndex to line index
      for TListItem set AIndex to column index }
    procedure AddListItem(AItem: TObject; AIndex: Integer);
    { Delete control from model observers }
    procedure DelControl(AControl: TControl);
    { if AIndex = -1 then delete all indexes from AItem }
    procedure DelListItem(AItem: TObject; AIndex: Integer = -1);
    { Data model value, reflected on all observers }
    property Value: string read FValue write SetValue;
    { Allow Value change from observer controls,
      auto-disabled when controls changed by model }
    property AllowChange: Boolean read FAllowChange;
  end;

implementation

{ TListItemRecList }

procedure TListItemRecList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) then
    TListItemRec(Ptr).Free();
end;

function TListItemRecList.GetItem(Index: Integer): TListItemRec;
begin
  Result := TListItemRec(inherited Get(Index));
end;

function TListItemRecList.FindListItem(AListObj: TObject; AIndex: Integer
  ): Integer;
var
  TmpItem: TListItemRec;
begin
  Result := 0;
  while Result < Count do
  begin
    TmpItem := GetItem(Result);
    if (TmpItem.ListObj = AListObj)
    and (TmpItem.Index = AIndex) then
      Exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TListItemRecList.AddListItem(AListObj: TObject; AIndex: Integer
  ): TListItemRec;
begin
  Result := TListItemRec.Create();
  Result.ListObj := AListObj;
  Result.Index := AIndex;
  Add(Result);
end;

{ TDataModel }

procedure TDataModel.OnChangeHandler(Sender: TObject);
begin
  if AllowChange then
  begin
    if (Sender is TCustomEdit) then
      Value := (Sender as TCustomEdit).Text;
  end;
end;

procedure TDataModel.ListViewOnDeletionHandler(Sender: TObject; Item: TListItem
  );
begin
  DelListItem(Item);
end;

procedure TDataModel.SetValue(const AValue: string);
var
  i, Index: Integer;
  Item: TObject;
  ListItemRec: TListItemRec;
begin
  if FValue <> AValue then
  begin
    FAllowChange := False;
    FValue := AValue;
    try
      // update controls
      for i := 0 to FControls.Count-1 do
      begin
        Item := TObject(FControls[i]);
        UpdateControl(Item);
      end;

      // update list items
      for i := 0 to FListItemRecList.Count-1 do
      begin
        ListItemRec := FListItemRecList.GetItem(i);
        Item := ListItemRec.ListObj;
        Index := ListItemRec.Index;
        UpdateListItem(Item, Index);
      end;
    finally
      FAllowChange := True;
    end;
  end;
end;

procedure TDataModel.UpdateControl(AItem: TObject);
begin
  if (AItem is TCustomEdit) then
    (AItem as TCustomEdit).Text := FValue
  else if (AItem is TLabel) then
    (AItem as TLabel).Caption := FValue
  else if (AItem is TStaticText) then
    (AItem as TStaticText).Caption := FValue;
end;

procedure TDataModel.UpdateListItem(AItem: TObject; AIndex: Integer);
begin
  if (AItem is TCustomListBox) then
  begin
    (AItem as TCustomListBox).Items[AIndex] := FValue;
  end
  else if (AItem is TListItem) then
  begin
    if AIndex = 0 then
      (AItem as TListItem).Caption := FValue
    else
      (AItem as TListItem).SubItems[AIndex-1] := FValue;
  end;
end;

// remote component destroyed
procedure TDataModel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation <> opRemove then
    Exit;

  if FControls.IndexOf(AComponent) <> -1 then
  begin
    if (AComponent is TControl) then
      DelControl(AComponent as TControl);
  end
  else
  begin
    DelListItem(AComponent);
  end;
end;

procedure TDataModel.AfterConstruction;
begin
  inherited AfterConstruction;
  FControls := TList.Create();
  FListItemRecList := TListItemRecList.Create();
  FAllowChange := True;
  FValue := '';
end;

procedure TDataModel.BeforeDestruction;
var
  i: Integer;
  ListItemRec: TListItemRec;
begin

  for i := FControls.Count-1 downto 0 do
    DelControl(TControl(FControls[i]));

  for i := FListItemRecList.Count-1 downto 0 do
  begin
    ListItemRec := FListItemRecList.GetItem(i);
    DelListItem(ListItemRec.ListObj, ListItemRec.Index);
  end;

  FreeAndNil(FListItemRecList);
  FreeAndNil(FControls);
  inherited BeforeDestruction;
end;

procedure TDataModel.AddControl(AControl: TControl);
begin
  if FControls.IndexOf(AControl) = -1 then
  begin
    FControls.Add(AControl);
    // Add FreeNotification to control, he call Notify() if destroyed
    AControl.FreeNotification(Self);
    UpdateControl(AControl);
    if (AControl is TCustomEdit) then
    begin
      (AControl as TCustomEdit).OnChange := @OnChangeHandler;
    end
  end;
end;

procedure TDataModel.AddListItem(AItem: TObject; AIndex: Integer);
var
  ListItem: TListItem;
begin
  if FListItemRecList.FindListItem(AItem, AIndex) <> -1 then
    Exit;

  FListItemRecList.AddListItem(AItem, AIndex);

  if (AItem is TComponent) then
  begin
    // Add FreeNotification to control, he call Notify() if destroyed
    (AItem as TComponent).FreeNotification(Self)
  end
  else if (AItem is TListItem) then
  begin
    // add OnDeletion handler to ListView
    ListItem := (AItem as TListItem);
    if Assigned(ListItem.Owner) and (Assigned(ListItem.Owner.Owner)) then
      if (ListItem.Owner.Owner is TListView) then
        (ListItem.Owner.Owner as TListView).OnDeletion := @ListViewOnDeletionHandler;
  end;

  // set value
  UpdateListItem(AItem, AIndex);
end;

procedure TDataModel.DelControl(AControl: TControl);
var
  n: Integer;
begin
  n := FControls.IndexOf(AControl);
  if n <> -1 then
  begin
    if (AControl is TCustomEdit) then
    begin
      (AControl as TCustomEdit).OnChange := nil;
    end;
    AControl.RemoveFreeNotification(Self);
    FControls.Delete(n);
  end;
end;

procedure TDataModel.DelListItem(AItem: TObject; AIndex: Integer);
var
  i, ii, n: Integer;
  ListItemRec: TListItemRec;
  ListItem: TListItem;
  lv: TListView;
begin
  n := FListItemRecList.FindListItem(AItem, AIndex);
  if n <> -1 then
    FListItemRecList.Delete(n);

  // remove destroying notification
  if (AItem is TComponent) then
  begin
    // Remove FreeNotification
    (AItem as TComponent).RemoveFreeNotification(Self)
  end
  else if (AItem is TListItem) then
  begin
    // remove OnDeletion handler from ListView
    ListItem := (AItem as TListItem);
    if Assigned(ListItem.Owner) and (Assigned(ListItem.Owner.Owner)) then
    begin
      if (ListItem.Owner.Owner is TListView) then
      begin
        lv := (ListItem.Owner.Owner as TListView);
        // find same ListView for other items
        ii := 0;
        for i := 0 to FListItemRecList.Count-1 do
        begin
          ListItemRec := FListItemRecList.GetItem(i);
          if (ListItemRec.ListObj is TListItem) then
          begin
            ListItem := (ListItemRec.ListObj as TListItem);
            if Assigned(ListItem.Owner) and (Assigned(ListItem.Owner.Owner)) then
            begin
              if (ListItem.Owner.Owner = lv) then
              begin
                ii := 1;
                Break;
              end;
            end;
          end;
        end;
        if ii = 0 then
          lv.OnDeletion := nil;
      end;
    end;
  end;
end;

end.

