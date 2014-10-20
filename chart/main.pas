unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, chartframe,
  symbols;

type

  { THeikinAshiTrader }

  THeikinAshiTrader = class(TForm)
    PageControl1: TPageControl;
    sgSymbols: TStringGrid;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSymbols: TSymbols;
    procedure SelectChart(AChart: string);
    procedure AddPage(AFile: string);
    procedure AddSymbol(AFile: string);
    procedure FeedBack(AString: string);
    procedure GetFeedBack(AFeedBack: string);
    function FindPage(AFile: string): boolean;
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
  lSymbol: TSymbol;
  I: Integer;
begin
  FSymbols := TSymbols.Create;
  FSymbols.LoadData;
  sgSymbols.RowCount:= 1;

  for lSymbol in FSymbols do
    AddSymbol(lSymbol.FilePath);

    sgSymbols.Row:= 0;
end;

procedure THeikinAshiTrader.FormDestroy(Sender: TObject);
begin
  FSymbols.Free;
end;

procedure THeikinAshiTrader.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  timer1.Enabled:= False;
end;

procedure THeikinAshiTrader.FormShow(Sender: TObject);
var
  lFile: string;
begin
  lFile := sgSymbols.Cells[0, 0];
  if lFile <> '' then
    AddPage(lFile);
end;

procedure THeikinAshiTrader.sgSymbolsDblClick(Sender: TObject);
begin
  if sgSymbols.Row > 0 then
    if not FindPage(sgSymbols.Cells[0, sgSymbols.Row]) then
      AddPage(sgSymbols.Cells[0, sgSymbols.Row]);
end;

procedure THeikinAshiTrader.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  (PageControl1.ActivePage.Controls[0] as TChartFrame).actRefresh.Execute;
  Timer1.Enabled := True;
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
    with TChartFrame.Create(Afile, lTab) do
    begin
      OnFeedBack := @GetFeedBack;
      Parent := lTab;
      Align := alClient;
      GetFile;
    end;
  end;
  PageControl1.ActivePage := lTab;
end;

procedure THeikinAshiTrader.AddSymbol(AFile: string);
begin
  sgSymbols.RowCount := sgSymbols.RowCount + 1;
  sgSymbols.Cells[0, sgSymbols.RowCount - 1] := Afile;
end;

procedure THeikinAshiTrader.FeedBack(AString: string);
begin
  StatusBar1.SimpleText:= AString;
end;

procedure THeikinAshiTrader.GetFeedBack(AFeedBack: string);
begin
  FeedBack(AFeedBack);
  Application.ProcessMessages;
end;

function THeikinAshiTrader.FindPage(AFile: string): boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to PageControl1.PageCount - 1 do
    if PageControl1.Pages[I].Caption = AFile then
    begin
      Result := True;
      PageControl1.ActivePage := PageControl1.Pages[I];
      Break;
    end;
end;

end.

