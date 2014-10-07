unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Grids, chartframe;

type

  { THeikinAshiTrader }

  THeikinAshiTrader = class(TForm)
    PageControl1: TPageControl;
    sgSymbols: TStringGrid;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure SelectChart(AChart: string);
    procedure AddPage(AFile: string);
    procedure AddSymbol(AFile: string);
  public
    { public declarations }
  end;

var
  HeikinAshiTrader: THeikinAshiTrader;

implementation

{$R *.lfm}

{ THeikinAshiTrader }

procedure THeikinAshiTrader.FormCreate(Sender: TObject);
var
  lFiles: TStringList;
  I: Integer;
begin
  sgSymbols.RowCount:= 1;
  lFiles := TStringList.Create;
  try
    lFiles.LoadFromFile('files.list');
    for I := 0 to lFiles.Count - 1 do
      AddSymbol(lFiles[I]);

    sgSymbols.Row:= 0;
    AddPage(lFiles[0]);
  finally
    lFiles.Free;
  end;
end;

procedure THeikinAshiTrader.sgSymbolsDblClick(Sender: TObject);
begin
  if sgSymbols.Row > 0 then
    AddPage(sgSymbols.Cells[0, sgSymbols.Row]);
end;

procedure THeikinAshiTrader.Timer1Timer(Sender: TObject);
begin
  (PageControl1.ActivePage.Controls[0] as TChartFrame).actRefresh.Execute;
end;

procedure THeikinAshiTrader.SelectChart(AChart: string);
var
  I: Integer;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    if UpperCase(PageControl1.Pages[I].Caption) = UpperCase(AChart) then
    begin
      PageControl1.ActivePage := PageControl1.Pages[I];
      Break;
    end;
  end;

end;

procedure THeikinAshiTrader.AddPage(AFile: string);
var
  lTab: TTabSheet;
begin
  lTab := PageControl1.AddTabSheet;
  with lTab do
  begin
    Caption:= AFile;
    with TChartFrame.Create(lTab) do
    begin
      Parent := lTab;
      Align := alClient;
      Display(AFile);
    end;
  end;
  PageControl1.ActivePage := lTab;
end;

procedure THeikinAshiTrader.AddSymbol(AFile: string);
begin
  sgSymbols.RowCount := sgSymbols.RowCount + 1;
  sgSymbols.Cells[0, sgSymbols.RowCount - 1] := Afile;
end;

end.

