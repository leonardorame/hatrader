unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, ActnList, chartframe,
  symbols, loader, newwindow;

type

  { THeikinAshiTrader }

  THeikinAshiTrader = class(TForm)
    actNewWindow: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    sgSymbols: TStringGrid;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure actNewWindowExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgSymbolsDblClick(Sender: TObject);
    procedure ChartNeedData(Sender: TObject);
    procedure sgSymbolsDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure Timer1Timer(Sender: TObject);
  private
    FSymbols: TSymbols;
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
  sgSymbols.FocusRectVisible:= False;
  sgSymbols.RowCount:= 1;

  FSymbols := TSymbols.Create;
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

procedure THeikinAshiTrader.sgSymbolsDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  lSymbol: TSymbol;
  lX: Integer;
  lY: Integer;
  lCellHeigh: Integer;
  lCanvas: TCanvas;
  lText: string;
  lOpen: double;
  lHigh: double;
  lLow: double;
  lClose: double;
  lPrev: double;
begin
  if gdSelected in aState then
  begin
    sgSymbols.Canvas.Brush.Style:= bsSolid;
    sgSymbols.Canvas.Brush.Color:= $442200;
    sgSymbols.Canvas.FillRect(aRect);
    lCanvas.Font.Color := $111111;
  end
  else
    lCanvas.Font.Color := $1F1F1F;
  if ARow > 0 then
  begin
    lCanvas := sgSymbols.Canvas;
    lCellHeigh := aRect.Bottom - aRect.Top;
    lSymbol := TSymbol(sgSymbols.Objects[0, ARow]);
    lOpen := lSymbol.Last.Open;
    lHigh := lSymbol.Last.High;
    lLow := lSymbol.Last.Low;
    lClose := lSymbol.Last.Close;
    lPrev := lSymbol.Last.Prev;
    case ACol of
      0: begin
         lText := lSymbol.Name;
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lCanvas.TextOut(aRect.Left + 2, lY, lText);
      end;
      1: begin
         lCanvas.Brush.Style:= bsSolid;
         if lPrev < lClose then
         begin
           lCanvas.Brush.Color:= clGreen;
           lCanvas.FillRect(aRect);
         end
         else
         if lPrev > lClose then
         begin
           lCanvas.Brush.Color:= clRed;
           lCanvas.FillRect(aRect);
         end;

         lText := Format('%.2f',[lClose]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
      2: begin
         lText := Format('%.2f',[lOpen]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
      3: begin
         lText := Format('%.2f',[lHigh]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
      4: begin
         lText := Format('%.2f',[lLow]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
      5: begin
         lText := Format('%.2f',[lPrev]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
      6: begin
         if lPrev < lClose then
           lCanvas.Font.Color:= clGreen
         else
         if lPrev > lClose then
           lCanvas.Font.Color:= clRed;

         lText := Format('%.2f%%',[((lClose / lPrev) - 1) * 100]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
    end;
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

