unit DataModels;

{ Author: Sergey Bodrov (serbod@gmail.com) 2010-2017 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, DataStorage;

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
  protected
    FControls: TList;
    FListItemRecList: TListItemRecList;
    FDataStorage: IDataStorage;
    FAllowChange: Boolean;
    procedure SetDataStorage(AValue: IDataStorage); virtual;
    procedure UpdateControl(AItem: TObject); virtual;
    procedure UpdateListItem(AItem: TObject; AIndex: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure ListViewOnDeletionHandler(Sender: TObject; Item: TListItem);
    procedure OnChangeHandler(Sender: TObject); virtual;
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
    { Set all assigned controls to value from DataStorage }
    procedure UpdateControls(); virtual;
    { DataStorage for value.
      Changes in DataStorage.Value is not automatically reflected to controls!
      You need to call UpdateControls() if DataStorage.Value changed }
    property DataStorage: IDataStorage read FDataStorage write SetDataStorage;
    { Allow Value change from observer controls,
      auto-disabled when controls changed by model }
    property AllowChange: Boolean read FAllowChange;
  end;

  { TDataModelText }

  TDataModelText = class(TDataModel)
  private
    function GetValue: string;
  protected
    FTextValue: string;
    procedure SetValue(const AValue: string);
    procedure UpdateControl(AItem: TObject); override;
    procedure UpdateListItem(AItem: TObject; AIndex: Integer); override;
    procedure OnChangeHandler(Sender: TObject); override;
  published
    { Data model value, reflected on all observers }
    property TextValue: string read GetValue write SetValue;
  end;

implementation

{ TDataModelText }

function TDataModelText.GetValue: string;
begin
  if Assigned(FDataStorage) then
    Result := FDataStorage.GetValue()
  else
    Result := FTextValue;
end;

procedure TDataModelText.SetValue(const AValue: string);
begin
  if Assigned(FDataStorage) then
    FDataStorage.SetValue(AValue);

  if FTextValue <> AValue then
  begin
    FTextValue := AValue;
    UpdateControls();
  end;
end;

procedure TDataModelText.UpdateControl(AItem: TObject);
begin
  inherited UpdateControl(AItem);
  if (AItem is TCustomEdit) then
    (AItem as TCustomEdit).Text := FTextValue
  else if (AItem is TLabel) then
    (AItem as TLabel).Caption := FTextValue
  else if (AItem is TStaticText) then
    (AItem as TStaticText).Caption := FTextValue;
end;

procedure TDataModelText.UpdateListItem(AItem: TObject; AIndex: Integer);
begin
  inherited UpdateListItem(AItem, AIndex);
  if (AItem is TCustomListBox) then
  begin
    (AItem as TCustomListBox).Items[AIndex] := FTextValue;
  end
  else if (AItem is TListItem) then
  begin
    if AIndex = 0 then
      (AItem as TListItem).Caption := FTextValue
    else
      (AItem as TListItem).SubItems[AIndex-1] := FTextValue;
  end;
end;

procedure TDataModelText.OnChangeHandler(Sender: TObject);
begin
  inherited OnChangeHandler(Sender);
  if AllowChange and (Sender is TCustomEdit) then
  begin
    FTextValue := (Sender as TCustomEdit).Text;
    if Assigned(FDataStorage) then
      FDataStorage.SetValue(FTextValue);
    UpdateControls();
  end;
end;

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
end;

procedure TDataModel.UpdateControls();
var
  i, Index: Integer;
  Item: TObject;
  ListItemRec: TListItemRec;
begin
  FAllowChange := False;
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

procedure TDataModel.SetDataStorage(AValue: IDataStorage);
begin
  if FDataStorage = AValue then Exit;
  FDataStorage := AValue;
end;

procedure TDataModel.UpdateControl(AItem: TObject);
begin
end;

procedure TDataModel.UpdateListItem(AItem: TObject; AIndex: Integer);
begin
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

procedure TDataModel.ListViewOnDeletionHandler(Sender: TObject; Item: TListItem);
begin
  DelListItem(Item);
end;

procedure TDataModel.AfterConstruction;
begin
  inherited AfterConstruction;
  FControls := TList.Create();
  FListItemRecList := TListItemRecList.Create();
  FAllowChange := True;
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

