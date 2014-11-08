unit chartframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, ActnList, DateUtils, TAMultiSeries,
  TACustomSeries, TASeries, TAChartAxis, TAChartUtils, TATools, Math, types,
  candlestickchart, contnrs, newwindow, ohlc,
  symbols, fgl,
  loader;

type

  { TChartFrame }
  TIntArray = array of integer;

  TChartFrame = class(TFrame)
    actNewWindow: TAction;
    ActionList1: TActionList;
    HAChart: TChart;
    CandleStickChart: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ImageList1: TImageList;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    Splitter1: TSplitter;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    procedure actNewWindowExecute(Sender: TObject);
    procedure CandleStickChartAfterDrawBackWall(ASender: TChart;
      ACanvas: TCanvas; const ARect: TRect);
    procedure CandleStickChartAfterPaint(ASender: TChart);
    procedure CandleStickChartMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ChartToolset1DataPointHintTool1HintPosition(
      ATool: TDataPointHintTool; var APoint: TPoint);
    procedure Timer1Timer(Sender: TObject);
  private
    FFirstBarOfToday: double;
    FOnFeedBack: TOnFeedBack;
    FOnNeedData: TNotifyEvent;
    FSymbol: TSymbol;
    FHint: string;
    FHAChartSeries: TCandleStickChartSeries;
    FMovingAvg: TLineSeries;
    FOHLCSeries: TOpenHighLowCloseSeries;
    FLongArray: TIntArray;
    procedure SetHint(AOHLCRecord: TOHLCRecord);
  public
    constructor Create(ASymbol: TSymbol; TheOwner: TComponent);
    destructor destroy; override;
    procedure Display;
    procedure DataChanged(Sender: TObject);
    procedure DestroyNewWindow(Sender: TObject);
    procedure CloseNewWindow(Sender: TObject; var CloseAction: TCloseAction);
    property Symbol: TSymbol read FSymbol;
    property OnNeedData: TNotifyEvent read FOnNeedData write FOnNeedData;
    property OnFeedBack: TOnFeedBack read FOnFeedBack write FOnFeedBack;
  end;

implementation

{$R *.lfm}

{ TChartFrame }

const
  cGridColor = $111111;
  cAxisFontColor = $444444;

procedure TChartFrame.Display;
var
  I: Integer;
  lLowest: double;
  lHighest: double;
  lOhlc: TOHLCRecord;
  lHA: TOHLCRecord;
  lHA_1: TOHLCRecord;
  lSMA5: double;
  lSmaDiff: double;

begin
  if FHint = '' then
  begin
    SetHint(FSymbol.Data[FSymbol.Data.Count - 1]);
  end;

  // ------ CandleStick Chart ------
  FOHLCSeries.Clear;
  lLowest := 0;
  lHighest := 0;
  for I := 0 to FSymbol.Data.Count - 1 do
  begin
    if (FSymbol.Data[I].low < lLowest) or (lLowest = 0) then
      lLowest := FSymbol.Data[I].low;
    if (FSymbol.Data[I].high > lHighest) or (lHighest = 0) then
      lHighest := FSymbol.Data[I].high;
    lOhlc := FSymbol.Data[I];

    if FSymbol.SymbolType = stDaily then
      FOHLCSeries.AddXOHLC( I, lOhlc.open, lOhlc.high, lOhlc.low, lOhlc.close, lOhlc.date )
    else
      FOHLCSeries.AddXOHLC( I, lOhlc.open, lOhlc.high, lOhlc.low, lOhlc.close, lOhlc.Time );
  end;
  CandleStickChart.Extent.YMin := lLowest;
  CandleStickChart.Extent.YMax := lHighest;
  CandleStickChart.Extent.UseYMax := True;
  CandleStickChart.Extent.UseYMin := True;

  // ------ SMA 5 ----

  FMovingAvg.Clear;
  SetLength(FLongArray, 0);
  lSmaDiff := 0;
  for I := 0 to FSymbol.Data.Count - 1 do
  begin
    if I > 4 then
    begin
      lSMA5 :=
        (FSymbol.Data[I - 4].close +
        FSymbol.Data[I - 3].close +
        FSymbol.Data[I - 2].close +
        FSymbol.Data[I - 1].close +
        FSymbol.Data[I].close) / 5;
      if (lSmaDiff < 0) and ((FSymbol.Data[I].close - lSMA5) > 0) then
      begin
        SetLength(FLongArray, Length(FLongArray) + 1);
        FLongArray[Length(FLongArray) - 1] := I;
      end;
      lSmaDiff:= FSymbol.Data[I].close - lSMA5;
    end
    else
      lSMA5 := FSymbol.Data[I].close;

    FMovingAvg.AddXY(I, lSMA5);
  end;

  // ------ Heikin Ashi Chart ------
  FHAChartSeries.Clear;
  lLowest := 0;
  lHighest := 0;
  // default values for HA open and close
  lHA_1 := TOHLCRecord.Create;
  lHA := TOHLCRecord.Create;
  try
    lHA_1.open := FSymbol.Data[0].open;
    lHA_1.close := FSymbol.Data[0].close;
    for I := 0 to FSymbol.Data.Count - 1 do
    begin
      if (FSymbol.Data[I].low < lLowest) or (lLowest = 0) then
        lLowest := FSymbol.Data[I].low;
      if (FSymbol.Data[I].high > lHighest) or (lHighest = 0) then
        lHighest := FSymbol.Data[I].high;
      lOhlc := FSymbol.Data[I];

      lHA.open:= (lHA_1.open + lHA_1.close) / 2;
      lHA.close:= (lOhlc.open + lOhlc.high + lOhlc.low + lOhlc.close ) / 4;
      lHA.high:= Max(Max(lOhlc.open, lOhlc.close), lOhlc.high);
      lHA.low:= Min(Min(lOhlc.open, lOhlc.Close), lOhlc.low);
      // store last record for use in the next iteration
      lHA_1.open := lHA.open;
      lHA_1.close := lHA.close;

      if FSymbol.SymbolType = stDaily then
        FHAChartSeries.AddXOHLC( I, lHA.open, lHA.high, lHA.low, lHA.close, lOhlc.date )
      else
        FHAChartSeries.AddXOHLC( I, lHA.open, lHA.high, lHA.low, lHA.close, lOhlc.Time )
    end;
  finally
    lHA_1.Free;
    lHA.Free;
  end;
  HAChart.Extent.YMin := lLowest;
  HAChart.Extent.YMax := lHighest;
  HAChart.Extent.UseYMax := True;
  HAChart.Extent.UseYMin := True;

  if Symbol.SymbolType = stIntraday then
  begin
    for I:= FSymbol.Data.Count - 1 downto 0 do
    begin
      if FSymbol.Data[I].Date = FSymbol.Daily.Last.Date then
        FFirstBarOfToday := I
      else
        break;
    end;
  end;
end;

constructor TChartFrame.Create(ASymbol: TSymbol; TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSymbol := ASymbol;
  FSymbol.OnDataChanged := @DataChanged;

  // in intraday chart define 1st bar
  // with date equal to FSymbol.Daily.Last.Date
  FFirstBarOfToday := 0;

  FMovingAvg := TLineSeries.Create(Self);
  FMovingAvg.AxisIndexX := 1;
  FMovingAvg.AxisIndexY := 0;
  FMovingAvg.LinePen.Color := clBlue;
  FMovingAvg.LinePen.Width := 2;

  FOHLCSeries := TOpenHighLowCloseSeries.Create(Self);
  FOHLCSeries.AxisIndexX:= 0;
  FOHLCSeries.DownLinePen.Color:= clRed;
  FOHLCSeries.DownLinePen.EndCap:= pecSquare;
  FOHLCSeries.DownLinePen.Width:= 2;
  FOHLCSeries.LinePen.Color:= clLime;
  FOHLCSeries.LinePen.EndCap:= pecSquare;
  FOHLCSeries.LinePen.Width:= 2;
  FOHLCSeries.Source := ListChartSource1;
  FOHLCSeries.TickWidth:= 40;
  FOHLCSeries.ListSource.YCount:= 4;
  with CandleStickChart do
  begin
    TChartAxis(AxisList.Items[1]).Grid.Color:= cGridColor;
    TChartAxis(AxisList.Items[1]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[1]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[1]).Marks.LabelFont.Color:= cAxisFontColor;
    TChartAxis(AxisList.Items[0]).Grid.Color:= cGridColor;
    TChartAxis(AxisList.Items[0]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[0]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[0]).Marks.LabelFont.Color:= cAxisFontColor;
  end;

  with HAChart do
  begin
    TChartAxis(AxisList.Items[1]).Grid.Color:= cGridColor;
    TChartAxis(AxisList.Items[1]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[1]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[1]).Marks.LabelFont.Color:= cAxisFontColor;
    TChartAxis(AxisList.Items[0]).Grid.Color:= cGridColor;
    TChartAxis(AxisList.Items[0]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[0]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[0]).Marks.LabelFont.Color:= cAxisFontColor;
  end;

  FHAChartSeries := TCandleStickChartSeries.Create(Self);
  FHAChartSeries.AxisIndexX:= 0;
  FHAChartSeries.DownLinePen.Color:= clRed;
  FHAChartSeries.DownLinePen.EndCap:= pecSquare;
  FHAChartSeries.DownLinePen.Width:= 1;
  FHAChartSeries.LinePen.Color:= clLime;
  FHAChartSeries.LinePen.EndCap:= pecSquare;
  FHAChartSeries.LinePen.Width:= 1;
  FHAChartSeries.Source := ListChartSource2;
  FHAChartSeries.TickWidth:= 40;
  FHAChartSeries.ListSource.YCount:= 4;

  CandleStickChart.AddSeries(FOHLCSeries);
  CandleStickChart.AddSeries(FMovingAvg);
  HAChart.AddSeries(FHAChartSeries);
end;

procedure TChartFrame.ChartToolset1DataPointHintTool1HintPosition(
  ATool: TDataPointHintTool; var APoint: TPoint);
begin
  if FSymbol.Data.Count = 0 then
    exit;

  SetHint(FSymbol.Data[ATool.PointIndex]);
  HAChart.Invalidate;
  CandleStickChart.Invalidate;
end;

procedure TChartFrame.Timer1Timer(Sender: TObject);
begin
  if Assigned(FOnNeedData) then
    FOnNeedData(Self);
end;

procedure TChartFrame.DestroyNewWindow(Sender: TObject);
var
  lWin: TNewWindow;
  lChartFrame: TChartFrame;
begin
  if (Sender is TNewWindow) then
  begin
    lWin := (Sender as TNewWindow);
    lChartFrame := lWin.Controls[0] as TChartFrame;

    // sólo se libera el símbolo si es intraday
    // porque fué instanciado en actNewWindowExecute
    if lChartFrame.Symbol.SymbolType = stIntraday then
    begin
      lChartFrame.Symbol.Daily := nil;
      lChartFrame.Symbol.Free;
    end;
    lChartFrame.Free;
  end;
end;

procedure TChartFrame.CloseNewWindow(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TChartFrame.SetHint(AOHLCRecord: TOHLCRecord);
begin
  if Self.Symbol.SymbolType = stDaily then
  begin
    FHint := Format('O: %f, H: %f, L: %f, C: %f, %s, Vol: %d',
      [AOHLCRecord.open,
      AOHLCRecord.high,
      AOHLCRecord.low,
      AOHLCRecord.close,
      AOHLCRecord.date,
      AOHLCRecord.Volume])
  end
  else
  if Self.Symbol.SymbolType = stIntraday then
  begin
    FHint := Format('O: %f, H: %f, L: %f, C: %f, %s %s, Vol: %d',
      [AOHLCRecord.open,
      AOHLCRecord.high,
      AOHLCRecord.low,
      AOHLCRecord.close,
      AOHLCRecord.date,
      AOHLCRecord.Time,
      AOHLCRecord.Volume])
  end;
end;

procedure TChartFrame.DataChanged(Sender: TObject);
begin
  Display;
end;

procedure TChartFrame.CandleStickChartMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  HAChart.Invalidate;
  CandleStickChart.Invalidate;
end;

procedure TChartFrame.CandleStickChartAfterPaint(ASender: TChart);
var
  ext: TDoubleRect;
  lLeft, lTop, lBottom, lRight: Integer;
  lStr: string;
  lYPos: Integer;
  lXPos: Integer;
  lFontHeight: Integer;
  lTextWidth: Integer;
  lIndex: Integer;
  lChartPos: double;
  lLong: Integer;
  I: Integer;

begin
  if FSymbol.Data.Count = 0 then
    exit;

  ext := ASender.CurrentExtent;
  lLeft := ASender.XGraphToImage(ext.a.x);
  lTop := ASender.YGraphToImage(ext.b.y);
  lBottom := ASender.YGraphToImage(ext.a.y);
  lRight := ASender.XGraphToImage(ext.b.x);
  lXPos := ASender.ScreenToClient(Mouse.CursorPos).X;
  lIndex := Round(ASender.XImageToGraph(lXPos));

  // OHLC and Date values
  if ASender = CandleStickChart then
  begin
    ASender.Canvas.Brush.Style:= bsClear;
    ASender.Canvas.Font.Color:= clWhite;
    ASender.Canvas.TextOut(lLeft + 2, lTop, FHint);
  end;

  // Moving Average values
  if ASender.Name = 'CandleStickChart' then
  begin
    if FMovingAvg.Count > 5 then
    begin
      if (lIndex > FMovingAvg.Count - 1) or (lIndex < 0) then
      begin
        lIndex := FMovingAvg.Count - 1;
      end;

      lStr := Format('SMA 5: %.2f', [FMovingAvg.GetYValue(lIndex)]);
      lFontHeight := ASender.Canvas.GetTextHeight(lStr);
      ASender.Canvas.Font.Color:= clBlue;
      ASender.Canvas.TextOut(lLeft + 2, lTop + 2 + lFontHeight, lStr);
    end;

    // Historical Volatility
    lStr := Format('HV 40: %.2f, HV 20: %.2f, HV 10: %.2f', [FSymbol.HV40, FSymbol.HV20, FSymbol.HV10]);
    lFontHeight := ASender.Canvas.GetTextHeight(lStr);
    ASender.Canvas.Font.Color:= clGray;
    ASender.Canvas.TextOut(lLeft + 2, lTop + 2 + (lFontHeight * 2), lStr);
  end;

  // close
  begin
    lIndex:= FSymbol.Data.Count - 1;
    lChartPos:= FSymbol.Data[lIndex].close;
    lStr := Format('%.2f', [lChartPos]);
    lFontHeight:= ASender.Canvas.Font.GetTextHeight(lStr);
    lTextWidth:= ASender.Canvas.Font.GetTextWidth(lStr);
    if FSymbol.Data[lIndex].close < FSymbol.Data[lIndex].open then
      ASender.Canvas.Brush.Color:=clRed
    else
      ASender.Canvas.Brush.Color:=clLime;

    ASender.Canvas.FillRect(
      lRight + 2,
      Round(ASender.YGraphToImage(lChartPos) - (lFontHeight / 2)),
      lRight + 8 + lTextWidth,
      Round(ASender.YGraphToImage(lChartPos) + (lFontHeight / 2))
      );

    ASender.Canvas.Brush.Style:= bsClear;
    ASender.Canvas.Font.Color:= clBlack;
    ASender.Canvas.Font.Style:= [fsBold];
    ASender.Canvas.TextOut(
      lRight + 4,
      Round(ASender.YGraphToImage(lChartPos) - (lFontHeight / 2)),
      lStr);
  end;

  // cursor line Y
  lYPos := ASender.ScreenToClient(Mouse.CursorPos).Y;
  if (lYPos >= lTop) and (lYPos <= lBottom) then
  begin
    ASender.Canvas.Pen.Color:= clGray;
    ASender.Canvas.Line(lLeft, lYPos, lRight, lYPos);

    // cursor pos
    lStr := Format('%f', [ASender.YImageToGraph(lYPos)]);
    lFontHeight:= ASender.Canvas.Font.GetTextHeight(lStr);
    ASender.Canvas.Brush.Style:= bsSolid;
    ASender.Canvas.Brush.Color:= clBlack;
    ASender.Canvas.Font.Color:= clYellow;
    ASender.Canvas.Font.Style:= [fsBold];
    ASender.Canvas.TextOut(lRight + 4, lYPos - (lFontHeight div 2), lStr);
  end;

  // cursor line X
  lXPos := ASender.ScreenToClient(Mouse.CursorPos).X;
  if (lXPos > 0) and (lXPos < lRight) then
  begin
    ASender.Canvas.Pen.Color:= clGray;
    ASender.Canvas.Line(lXPos, lTop, lXPos, lBottom);
  end;

  // open en daily
  if (Symbol.SymbolType = stIntraday) and (ASender = CandleStickChart) then
  begin
    lIndex:= FSymbol.Daily.Data.Count - 1;
    lChartPos:= FSymbol.Daily.Data[lIndex].Open;
    lXPos := ASender.XGraphToImage(FFirstBarOfToday);
    lYPos := ASender.YGraphToImage(lChartPos);
    lRight := ASender.XGraphToImage(ASender.CurrentExtent.b.x);
    ASender.Canvas.Brush.Style:= bsSolid;
    ASender.Canvas.Brush.Color:= $059E9E;
    ASender.Canvas.Pen.Width:= 2;
    ASender.Canvas.Pen.Color:= $059E9E;
    ASender.Canvas.Pen.Style:= psSolid;
    ASender.Canvas.Line(lXPos, lYpos, lRight, lYPos);
  end;

  // longs
  if (Length(FLongArray) > 0) then
  begin
    for I := 0 to Length(FLongArray) - 1 do
    begin
      lLong := FLongArray[I];
      lChartPos:= FSymbol.Data[lLong].Close;
      lXPos := ASender.XGraphToImage(lLong);
      lYPos := ASender.YGraphToImage(lChartPos);
      lRight := ASender.XGraphToImage(ASender.CurrentExtent.b.x);
      ASender.Canvas.Brush.Style:= bsSolid;
      ASender.Canvas.Brush.Color:= clYellow;
      ASender.Canvas.Pen.Width:= 2;
      ASender.Canvas.Pen.Color:= clYellow;
      ASender.Canvas.Pen.Style:= psSolid;
      ASender.Canvas.FillRect(lXPos - 4, lYpos - 4, lXPos + 4, lYPos + 4);
    end;
  end;
end;

procedure TChartFrame.actNewWindowExecute(Sender: TObject);
var
  lWin: TNewWindow;
  lChart: TChartFrame;
  lSymbol: TSymbol;
  lData: string;
  lThread: TGetDataThread;

begin
  lSymbol := TSymbol.Create;
  lSymbol.Name:= FSymbol.Name + ' (30 min)';
  lSymbol.FilePath:= 'http://www.ceciliastrada.com.ar/cgi-bin/intraday.bf/intraday?sym=' + FSymbol.Symbol;
  lSymbol.OnDataChanged:= @DataChanged;
  lSymbol.SymbolType:= stIntraday;
  lSymbol.Daily := FSymbol;


  lWin := TNewWindow.Create(nil);
  lWin.OnDestroy:= @DestroyNewWindow;
  lWin.OnClose := @CloseNewWindow;
  lWin.Caption:= lSymbol.Name;
  lChart := TChartFrame.Create(lSymbol, nil);
  lChart.ToolBar1.Visible:= False;
  lChart.OnFeedBack:= FOnFeedBack;
  lChart.Parent := lWin;
  lChart.Align:= alClient;
  lChart.OnNeedData:= FOnNeedData;
  lWin.Show;

  if Assigned(FOnNeedData) then
    FOnNeedData(lChart);
end;

procedure TChartFrame.CandleStickChartAfterDrawBackWall(ASender: TChart;
  ACanvas: TCanvas; const ARect: TRect);
var
  lTextWidth: Integer;
  lTextHeight: Integer;
  lX: Integer;
  lY: Integer;
begin
  ACanvas.Font.Size := 32;
  ACanvas.Font.Style:= [fsBold];
  ACanvas.Font.Color:= $303030;
  lTextWidth := ACanvas.GetTextWidth(FSymbol.Name);
  lTextHeight:= ACanvas.GetTextHeight(FSymbol.Name);
  lX := Round(((ARect.Right - ARect.Left) / 2) - (lTextWidth / 2));
  lY := Round(((ARect.Bottom - ARect.Top) / 2) - (lTextHeight / 2));
  ACanvas.TextOut(lX, lY, FSymbol.Name);
end;

destructor TChartFrame.destroy;
begin
  inherited;
end;

end.


