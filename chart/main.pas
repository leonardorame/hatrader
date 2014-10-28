unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, symbolgrid, ActnList, chartframe,
  symbols, loader, newwindow;

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
    procedure actNewWindowExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure ChartNeedData(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSymbols: TSymbols;
    sgSymbols: TSymbolGrid;
    procedure GetAllData(AData: string);
    procedure AbrirVentana(ASymbol: TSymbol);
    procedure GetFile(ASymbol: TSymbol);
    procedure AddSymbol(ASymbol: TSymbol);
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
  sgSymbols := TSymbolGrid.Create(FSymbols, tsSymbols);
  sgSymbols.Parent := tsSymbols;
  sgSymbols.Align:= alClient;
  sgSymbols.FocusRectVisible:= False;
  sgSymbols.RowCount:= 1;
  sgSymbols.OnDblClick := @sgSymbolsDblClick;
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
  if FSymbols.Count = 0 then
  begin
    try
      // el intervalo inicial es de 500, para que
      // se ejecute el método LoadInitialData
      // lo más rápido posible,
      // luego lo pasamos a 5000.
      Timer1.Interval:= 5000;
      FeedBack('Cargando datos iniciales...');
      FSymbols.LoadInitialData;
      for lSymbol in FSymbols do
        AddSymbol(lSymbol);

      sgSymbols.Row:= 0;
    except
      on E: Exception do
      begin
        FeedBack('Falló la conexión al servidor.');
      end;
    end;
  end
  else
  begin
    lThread := TGetAllDataThread.Create;
    lThread.OnFeedBack := @GetFeedBack;
    lThread.OnGetData:= @GetAllData;
    lThread.start;
  end;
end;

procedure THeikinAshiTrader.GetAllData(AData: string);
begin
  FSymbols.UpdateSymbolData(AData);
  sgSymbols.Invalidate;
end;

procedure THeikinAshiTrader.AbrirVentana(ASymbol: TSymbol);
var
  lWin: TNewWindow;
  lChart: TChartFrame;
  lData: string;
  lThread: TGetDataThread;
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

procedure THeikinAshiTrader.AddSymbol(ASymbol: TSymbol);
begin
  sgSymbols.RowCount := sgSymbols.RowCount + 1;
  sgSymbols.Objects[0, sgSymbols.RowCount - 1] := ASymbol;
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

