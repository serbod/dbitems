unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, RTTIGrids, Forms, Controls, Graphics,
  Dialogs, StdCtrls, MaskEdit, ExtCtrls, ComCtrls, DataModels;

type

  { TFormDataModelsTest }

  TFormDataModelsTest = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    gbStandard: TGroupBox;
    gbRTTI: TGroupBox;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    ListBox1: TListBox;
    ListView1: TListView;
    MaskEdit1: TMaskEdit;
    Memo1: TMemo;
    StaticText1: TStaticText;
    TIEdit1: TTIEdit;
    TIGrid1: TTIGrid;
    TILabel1: TTILabel;
    TIListBox1: TTIListBox;
    TIMaskEdit1: TTIMaskEdit;
    TIMemo1: TTIMemo;
    TISpinEdit1: TTISpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    DataModel1: TDataModelText;
  end;

var
  FormDataModelsTest: TFormDataModelsTest;

implementation

{$R *.lfm}

{ TFormDataModelsTest }

procedure TFormDataModelsTest.FormCreate(Sender: TObject);
begin
  //Self.Controls[];
  DataModel1 := TDataModelText.Create(Self);
  DataModel1.AddControl(Edit1);
  DataModel1.AddControl(MaskEdit1);
  DataModel1.AddControl(LabeledEdit1);
  DataModel1.AddControl(StaticText1);
  DataModel1.AddControl(Label1);
  DataModel1.AddControl(Memo1);
  DataModel1.AddListItem(ListBox1, 1);
  DataModel1.AddListItem(ListView1.Items[0], 1);

  // Link property
  TIEdit1.Link.TIObject := DataModel1;
  TIEdit1.Link.TIPropertyName := 'TextValue';

  TIMaskEdit1.Link.TIObject := DataModel1;
  TIMaskEdit1.Link.TIPropertyName := 'TextValue';

  //TISpinEdit1.Link.TIObject := DataModel1;
  //TISpinEdit1.Link.TIPropertyName := 'TextValue';

  TIMemo1.Link.TIObject := DataModel1;
  TIMemo1.Link.TIPropertyName := 'TextValue';

  TILabel1.Link.TIObject := DataModel1;
  TILabel1.Link.TIPropertyName := 'TextValue';

  TIListBox1.Link.TIObject := ListBox1;
  TIListBox1.Link.TIPropertyName := 'Items';

end;

procedure TFormDataModelsTest.Button1Click(Sender: TObject);
begin
  //ListView1.Free();
  //ListBox1.Free();
  //Label1.Free();
  //Memo1.Free();
  //StaticText1.Free();
  //LabeledEdit1.Free();
  //MaskEdit1.Free();
  //Button1.Free();
end;

end.

