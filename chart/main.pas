unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, symbolgrid, cclgrid, ActnList, chartframe,
  symbols, loader, newwindow, grids,
  LCLProc;

type

  { THeikinAshiTrader }

  THeikinAshiTrader = class(TForm)
    actNewWindow: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    tsSymbols: TTabSheet;
    tsCCL: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure actNewWindowExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure ChartNeedData(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSymbols: TSymbols;
    FCCL: TCCLSymbols;
    sgSymbols: TSymbolGrid;
    sgCCL: TCCLGrid;
    procedure CreateCalculatedSyms;
    procedure UpdateCalculatedSyms;
    procedure GetAllData(AData: string);
    procedure AbrirVentana(ASymbol: TSymbol);
    procedure GetFile(ASymbol: TSymbol);
    procedure AddSymbol(ASymbol: TSymbol; AGrid: TStringGrid);
    procedure FeedBack(AString: string);
    procedure GetFeedBack(AFeedBack: string);
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
  I: Integer;
begin
  gThreadList := TThreadList.Create;
  FSymbols := TSymbols.Create;
  FCCL := TCCLSymbols.Create;
  sgSymbols := TSymbolGrid.Create(FSymbols, tsSymbols);
  sgSymbols.Parent := tsSymbols;
  sgSymbols.Align:= alClient;
  sgSymbols.FocusRectVisible:= False;
  sgSymbols.RowCount:= 1;
  sgSymbols.OnDblClick := @sgSymbolsDblClick;

  sgCCL := TCCLGrid.Create(FCCL, tsCCL);
  sgCCL.Parent := tsCCL;
  sgCCL.Align:= alClient;
  sgCCL.FocusRectVisible:= False;
  sgCCL.RowCount:= 1;
end;

procedure THeikinAshiTrader.FormDestroy(Sender: TObject);
var
  lList: TList;
  lThread: TGetDataThread;
  I: Integer;
  lSymbol: TSymbol;
begin
  Timer1.Enabled:= False;
  lList := gThreadList.LockList;
  for I := lList.Count - 1 downto 0 do
  begin
    lThread := TGetDataThread(lList[I]);
    lThread.Terminate;
    Sleep(100);
  end;
  gThreadList.UnlockList;
  gThreadList.Free;
  sgSymbols.Free;
  sgCCL.Free;
  FCCL.Free;
  FSymbols.Free;
end;

procedure THeikinAshiTrader.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure THeikinAshiTrader.actNewWindowExecute(Sender: TObject);
begin
  if sgSymbols.Row > 0 then
    AbrirVentana(TSymbol(sgSymbols.Objects[0, sgSymbols.Row]));
end;

procedure THeikinAshiTrader.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  actNewWindow.Enabled:= (PageControl1.ActivePage = tsSymbols);
end;

procedure THeikinAshiTrader.sgSymbolsDblClick(Sender: TObject);
begin
  actNewWindow.Execute;
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

procedure THeikinAshiTrader.Timer1Timer(Sender: TObject);
var
  lSymbol: TSymbol;
  lThread: TGetAllDataThread;
begin
  Timer1.Enabled:= False;
  if FSymbols.Count = 0 then
  begin
    try
      // el intervalo inicial es de 500, para que se ejecute el método LoadInitialData
      // lo más rápido posible, luego lo pasamos a 5000.
      Timer1.Interval:= 5000;
      FeedBack('Cargando datos iniciales...');
      FSymbols.LoadInitialData;
      for lSymbol in FSymbols do
        AddSymbol(lSymbol, sgSymbols);
      sgSymbols.Row:= 0;

      // Una ves creados los símbolos iniciales
      // se generan los calculados
      CreateCalculatedSyms;
      for lSymbol in FCCL do
        AddSymbol(lSymbol, sgCCL);
      sgCCL.Row:= 0;

    except
      on E: Exception do
      begin
        FeedBack('Falló la conexión al servidor.');
      end;
    end;
  end
  else
  begin
    lThread := TGetAllDataThread.Create('http://www.ceciliastrada.com.ar/cgi-bin/intraday.bf/all');
    lThread.OnFeedBack := @GetFeedBack;
    lThread.OnGetData:= @GetAllData;
    lThread.start;
  end;
  Timer1.Enabled:= True;
end;

procedure THeikinAshiTrader.CreateCalculatedSyms;
var
  lSym: TCCLSymbol;
  lLocal: TSymbol;
  lUsa: TSymbol;

begin
  // APBR.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'APBR.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 2;
  lLocal := FSymbols.Find('APBR');
  lUsa := FSymbols.Find('PBR.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);

  // BMA.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'BMA.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 10;
  lLocal := FSymbols.Find('BMA');
  lUsa := FSymbols.Find('BMA.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);

  // END.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'EDN.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 20;
  lLocal := FSymbols.Find('EDN');
  lUsa := FSymbols.Find('EDN.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // FRAN.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'FRAN.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 3;
  lLocal := FSymbols.Find('FRAN');
  lUsa := FSymbols.Find('BFR.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // GGAL.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'GGAL.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 10;
  lLocal := FSymbols.Find('GGAL');
  lUsa := FSymbols.Find('GGAL.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // TECO2.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'TECO2.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 5;
  lLocal := FSymbols.Find('TECO2');
  lUsa := FSymbols.Find('TEO.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // TS.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'TS.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 2;
  lLocal := FSymbols.Find('TS');
  lUsa := FSymbols.Find('TS.ADR');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // AA17.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'AA17.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 1;
  lLocal := FSymbols.Find('AA17');
  lUsa := FSymbols.Find('AA17D');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  // AY24.CCL
  lSym := TCCLSymbol.Create;
  lSym.Name:= 'AY24.CCL';
  lSym.Symbol:= lSym.Name;
  lSym.Factor:= 1;
  lLocal := FSymbols.Find('AY24');
  lUsa := FSymbols.Find('AY24D');
  lSym.Local := lLocal;
  lSym.Usa := lUsa;
  FCCL.Add(lSym);
  UpdateCalculatedSyms;
end;

procedure THeikinAshiTrader.UpdateCalculatedSyms;
var
  lSym: TCCLSymbol;

begin
  for lSym in FCCL do
  begin
    if (lSym.Local <> nil) and (lSym.Usa <> nil) then
    begin
      lSym.Last.Open:= lSym.Local.Last.Open / lSym.Usa.Last.Open * lSym.Factor;
      lSym.Last.High:= lSym.Local.Last.High / lSym.Usa.Last.High * lSym.Factor;
      lSym.Last.Low:= lSym.Local.Last.Low / lSym.Usa.Last.Low * lSym.Factor;
      lSym.Last.Close:= lSym.Local.Last.Close / lSym.Usa.Last.Close * lSym.Factor;
    end;
  end;
end;

procedure THeikinAshiTrader.GetAllData(AData: string);
begin
  FSymbols.UpdateSymbolData(AData);
  sgSymbols.Invalidate;
  UpdateCalculatedSyms;
  sgCCL.Invalidate;
end;

procedure THeikinAshiTrader.AbrirVentana(ASymbol: TSymbol);
var
  lWin: TNewWindow;
  lChart: TChartFrame;
  lData: string;
  lSymbol: TSymbol;

begin
  lSymbol := TSymbol.Create;
  lSymbol.Symbol:= ASymbol.Symbol;
  lSymbol.Name:= ASymbol.Name;
  lSymbol.FilePath:= ASymbol.FilePath;
  lSymbol.SymbolType:= stDaily;

  lWin := TNewWindow.Create(nil);
  lWin.Caption:= lSymbol.Name;
  lChart := TChartFrame.Create(lSymbol, nil);
  lChart.ToolBar1.Visible:= True;
  lChart.OnFeedBack:= @FeedBack;
  lChart.OnNeedData:= @ChartNeedData;
  lChart.Parent := lWin;
  lChart.Align:= alClient;
  lSymbol.OnDataChanged:= @lChart.DataChanged;
  lWin.OnDestroy:= @lChart.DestroyNewWindow;
  lWin.OnClose := @lChart.CloseNewWindow;
  lWin.Show;
  GetFile(lSymbol);
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

procedure THeikinAshiTrader.AddSymbol(ASymbol: TSymbol; AGrid: TStringGrid);
begin
  AGrid.RowCount := AGrid.RowCount + 1;
  AGrid.Objects[0, AGrid.RowCount - 1] := ASymbol;
end;

procedure THeikinAshiTrader.FeedBack(AString: string);
begin
  StatusBar1.SimpleText:= AString;
  StatusBar1.Invalidate;
end;

procedure THeikinAshiTrader.GetFeedBack(AFeedBack: string);
begin
  FeedBack(AFeedBack);
  Application.ProcessMessages;
end;

end.

