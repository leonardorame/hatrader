unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, chartframe,
  symbols, loader;

type

  { THeikinAshiTrader }

  THeikinAshiTrader = class(TForm)
    PageControl1: TPageControl;
    sgSymbols: TStringGrid;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure ChartNeedData(Sender: TObject);
  private
    FSymbols: TSymbols;
    procedure GetFile(ASymbol: TSymbol);
    procedure SelectChart(AChart: string);
    procedure AddPage(ASymbol: TSymbol);
    procedure AddSymbol(ASymbol: TSymbol);
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
  gThreadList := TThreadList.Create;
  FSymbols := TSymbols.Create;
  FSymbols.LoadData;
  sgSymbols.RowCount:= 1;

  for lSymbol in FSymbols do
    AddSymbol(lSymbol);

  sgSymbols.Row:= 0;
end;

procedure THeikinAshiTrader.FormDestroy(Sender: TObject);
var
  lList: TList;
  lThread: TGetDataThread;
  I: Integer;
  lSymbol: TSymbol;
begin
  lList := gThreadList.LockList;
  for I := lList.Count - 1 downto 0 do
  begin
    lThread := TGetDataThread(lList[I]);
    lThread.Terminate;
    Sleep(100);
  end;
  gThreadList.UnlockList;
  gThreadList.Free;
  FSymbols.Free;
end;

procedure THeikinAshiTrader.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure THeikinAshiTrader.FormShow(Sender: TObject);
var
  lSymbol: TSymbol;
begin
  lSymbol := FSymbols[0];
  if lSymbol <> nil then
    AddPage(lSymbol);
end;

procedure THeikinAshiTrader.sgSymbolsDblClick(Sender: TObject);
var
  lSymbol: TSymbol;
begin
  if sgSymbols.Row > 0 then
  begin
    lSymbol := sgSymbols.Objects[0, sgSymbols.Row] as TSymbol;
    if not FindPage(lSymbol.Name) then
      AddPage(lSymbol);
  end;
end;

procedure THeikinAshiTrader.ChartNeedData(Sender: TObject);
var
  lChartFrame: TChartFrame;
begin
  if (Sender is TChartFrame) then
  begin
    lChartFrame := (Sender as TChartFrame);
    GetFile(lChartFrame.Symbol);
  end;
end;

procedure THeikinAshiTrader.GetFile(ASymbol: TSymbol);
var
  lThread: TGetDataThread;
begin
  lThread := TGetDataThread.Create;
  lThread.Symbol := ASymbol;
  lThread.OnFeedBack := @GetFeedBack;
  lThread.start;
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

procedure THeikinAshiTrader.AddPage(ASymbol: TSymbol);
var
  lTab: TTabSheet;
  lChartFrame: TChartFrame;
begin
  lTab := PageControl1.AddTabSheet;
  with lTab do
  begin
    Caption:= ASymbol.Name;
    lChartFrame := TChartFrame.Create(ASymbol, lTab);
    with lChartFrame do
    begin
      Parent := lTab;
      Align := alClient;
      GetFile(ASymbol);
      lChartFrame.OnNeedData:= @ChartNeedData;
    end;
  end;
  PageControl1.ActivePage := lTab;
end;

procedure THeikinAshiTrader.AddSymbol(ASymbol: TSymbol);
begin
  sgSymbols.RowCount := sgSymbols.RowCount + 1;
  sgSymbols.Objects[0, sgSymbols.RowCount - 1] := ASymbol;
  sgSymbols.Cells[0, sgSymbols.RowCount - 1] := ASymbol.Name;
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

