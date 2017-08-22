unit MsgBoardUnit;

interface

uses Classes, Contnrs, SysUtils, DbUnit;

type
  TMsgBoardItem = class(TDbItem)
  public
    Desc: string;
    Text: string;
    Priority: integer;
    Author: string;
    BeginDate: TDateTime;
    EndDate: TDateTime;
    function GetValue(const AName: string): string; override;
    procedure SetValue(const AName, AValue: string); override;
  end;

  TMsgBoardList = class(TDbItemList)
  private
    //LastID: integer;
    //function CompareFunc(Item1, Item2: Pointer): Integer;
  public
    BeginDate: TDateTime;
    EndDate: TDateTime;
    FileName: string;
    constructor Create(ADbDriver: TDbDriver); reintroduce;
    procedure LoadList();
    procedure SaveList();
    procedure Sort();
    //function AddItem(AItem: TMsgBoardItem): integer;
    function NewItem(): TDbItem; override;
  end;


implementation

//uses MainFunc;

// === TMsgBoardItem ===
function TMsgBoardItem.GetValue(const AName: string): string;
begin
  if AName = 'id' then
    Result := IntToStr(self.FID)
  else if AName = 'desc' then
    Result := self.Desc
  else if AName = 'text' then
    Result := self.Text
  else if AName = 'priority' then
    Result := IntToStr(self.Priority)
  else if AName = 'author' then
    Result := self.Author
  else if AName = 'begin_date' then
    Result := DateTimeToStr(self.BeginDate)
  else if AName = 'end_date' then
    Result := DateTimeToStr(self.EndDate)
  else
    Result := '';
end;

procedure TMsgBoardItem.SetValue(const AName, AValue: string);
begin
  if AName = 'id' then
    self.FID := StrToIntDef(AValue, 0)
  else if AName = 'desc' then
    self.Desc := AValue
  else if AName = 'text' then
    self.Text := AValue
  else if AName = 'priority' then
    self.Priority := StrToIntDef(AValue, 0)
  else if AName = 'author' then
    self.Author := AValue
  else if AName = 'begin_date' then
    self.BeginDate := StrToDateTime(AValue)
  else if AName = 'end_date' then
    self.EndDate := StrToDateTime(AValue);
end;

// === TMsgBoardList ===
constructor TMsgBoardList.Create(ADbDriver: TDbDriver);
var
  ti: TDbTableInfo;
begin
  if not Assigned(ADbDriver) then
    ADbDriver := GlobalDbDriver;
  ti := ADbDriver.GetDbTableInfo('msg_board');
  if not Assigned(ti) then
  begin
    ti := TDbTableInfo.Create();
    with ti do
    begin
      TableName := 'msg_board';
      AddField('id', 'I');
      AddField('desc', 'S');
      AddField('text', 'S');
      AddField('priority', 'I');
      AddField('author', 'S');
      AddField('begin_date', 'D');
      AddField('end_date', 'D');
    end;
    ADbDriver.TablesList.Add(ti);
  end;
  inherited Create(ti, ADbDriver);
end;

procedure TMsgBoardList.LoadList();
begin
  DbDriver.GetTable(self);
end;

procedure TMsgBoardList.SaveList();
begin
  DbDriver.SetTable(self);
end;

{function TMsgBoardList.AddItem(AItem: TMsgBoardItem): Integer;
begin
  AItem.ID:=LastID;
  Inc(LastID);
  Add(AItem);
end;}

function TMsgBoardList.NewItem(): TDbItem;
begin
  Result := TMsgBoardItem.Create();
  self.AddItem(Result, True);
end;

function CompareFunc(Item1, Item2: Pointer): integer;
begin
  Result := TMsgBoardItem(Item2).Priority - TMsgBoardItem(Item1).Priority;
end;

procedure TMsgBoardList.Sort();
begin
  inherited Sort(@CompareFunc);
end;


end.
