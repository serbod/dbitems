unit MsgBoardUnit;

interface

uses Classes, SysUtils, DbUnit;

type

  { TMsgBoardItem }

  TMsgBoardItem = class(TDbItem)
  public
    Desc: string;
    Text: string;
    Priority: integer;
    Author: string;
    BeginDate: TDateTime;
    EndDate: TDateTime;
    class procedure FillDbTableInfo(ADbTableInfo: TDbTableInfo); override;
    function GetValue(const AName: string): string; override;
    procedure SetValue(const AName: string; AValue: string); override;
  end;

  { TMsgBoardList }

  TMsgBoardList = class(TDbItemList)
  private
    //LastID: integer;
    //function CompareFunc(Item1, Item2: Pointer): Integer;
  public
    BeginDate: TDateTime;
    EndDate: TDateTime;
    FileName: string;
    constructor Create(ADbDriver: TDbDriver); reintroduce;
    class function GetDbItemClass(): TDbItemClass; override;
    procedure Sort();
  end;


implementation

//uses MainFunc;

class procedure TMsgBoardItem.FillDbTableInfo(ADbTableInfo: TDbTableInfo);
begin
  inherited FillDbTableInfo(ADbTableInfo);
  with ADbTableInfo do
  begin
    TableName := 'msg_board';
    AddField('desc', 'S');
    AddField('text', 'S');
    AddField('priority', 'I');
    AddField('author', 'S');
    AddField('begin_date', 'D');
    AddField('end_date', 'D');
  end;
end;

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
    Result := inherited GetValue(AName);
end;

procedure TMsgBoardItem.SetValue(const AName: string; AValue: string);
begin
  if AName = 'id' then
    self.FID := StrToInt64Def(AValue, 0)
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
    self.EndDate := StrToDateTime(AValue)
  else
    inherited SetValue(AName, AValue);
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

class function TMsgBoardList.GetDbItemClass(): TDbItemClass;
begin
  Result := TMsgBoardItem;
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
