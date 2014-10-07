unit chartframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, ActnList, DateUtils,
  TAMultiSeries,
  TACustomSeries, TASeries,
  TAChartAxis, TAChartUtils,
  TATools, Math, types,
  candlestickchart,
  contnrs,
  newwindow,
  ohlc,
  volatility;

type

  { TChartFrame }

  TChartFrame = class(TFrame)
    actNewWindow: TAction;
    actRefresh: TAction;
    ActionList1: TActionList;
    HAChart: TChart;
    CandleStickChart: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ImageList1: TImageList;
    ListChartSource1: TListChartSource;
    ListChartSource2: TListChartSource;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure actNewWindowExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure CandleStickChartAfterDrawBackWall(ASender: TChart;
      ACanvas: TCanvas; const ARect: TRect);
    procedure CandleStickChartAfterPaint(ASender: TChart);
    procedure CandleStickChartMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ChartToolset1DataPointHintTool1HintPosition(
      ATool: TDataPointHintTool; var APoint: TPoint);
  private
    FWinList: TObjectList;
    FFile: string;
    FHint: string;
    FHV10: double;
    FHV20: double;
    FHV40: double;
    FOHLCArray: TOHLCArray;
    FHAChart: TCandleStickChart;
    FMovingAvg: TLineSeries;
    FOHLCSeries: TOpenHighLowCloseSeries;
    procedure CloseNewWindow(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetHint(AOHLCRecord: TOHLCRecord);
    procedure PrepareArray(AFile: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor destroy; override;
    procedure Display(AFile: string);
  end;

implementation

{$R *.lfm}

{ TChartFrame }


procedure TChartFrame.Display(AFile: string);
var
  I: Integer;
  lLowest: double;
  lHighest: double;
  lOhlc: TOHLCRecord;
  lHA: TOHLCRecord;
  lHA_1: TOHLCRecord;
  lSMA5: double;

begin
  FFile := AFile;
  PrepareArray(AFile);
  if FHint = '' then
  begin
    SetHint(FOHLCArray[Length(FOHLCArray) - 1]);
  end;

  // ------ CandleStick Chart ------
  FOHLCSeries.Clear;
  lLowest := 0;
  lHighest := 0;
  for I := 0 to Length(FOHLCArray) - 1 do
  begin
    if (FOHLCArray[I].low < lLowest) or (lLowest = 0) then
      lLowest := FOHLCArray[I].low;
    if (FOHLCArray[I].high > lHighest) or (lHighest = 0) then
      lHighest := FOHLCArray[I].high;
    lOhlc := FOHLCArray[I];

    FOHLCSeries.AddXOHLC( I, lOhlc.open, lOhlc.high, lOhlc.low, lOhlc.close, lOhlc.date );
  end;
  CandleStickChart.Extent.YMin := lLowest;
  CandleStickChart.Extent.YMax := lHighest;
  CandleStickChart.Extent.UseYMax := True;
  CandleStickChart.Extent.UseYMin := True;

  // ------ SMA 5 ----

  FMovingAvg.Clear;
  for I := 0 to Length(FOHLCArray) - 1 do
  begin
    if I > 4 then
    begin
      lSMA5 :=
        (FOHLCArray[I - 4].close +
        FOHLCArray[I - 3].close +
        FOHLCArray[I - 2].close +
        FOHLCArray[I - 1].close +
        FOHLCArray[I].close) / 5;
    end
    else
      lSMA5 := FOHLCArray[I].close;

    FMovingAvg.AddXY(I, lSMA5);
  end;

  // ------ Heikin Ashi Chart ------
  FHAChart.Clear;
  lLowest := 0;
  lHighest := 0;
  // default values for HA open and close
  lHA_1.open := FOHLCArray[0].open;
  lHA_1.close := FOHLCArray[0].close;
  for I := 0 to Length(FOHLCArray) - 1 do
  begin
    if (FOHLCArray[I].low < lLowest) or (lLowest = 0) then
      lLowest := FOHLCArray[I].low;
    if (FOHLCArray[I].high > lHighest) or (lHighest = 0) then
      lHighest := FOHLCArray[I].high;
    lOhlc := FOHLCArray[I];

    lHA.open:= (lHA_1.open + lHA_1.close) / 2;
    lHA.close:= (lOhlc.open + lOhlc.high + lOhlc.low + lOhlc.close ) / 4;
    lHA.high:= Max(Max(lOhlc.open, lOhlc.close), lOhlc.high);
    lHA.low:= Min(Min(lOhlc.open, lOhlc.Close), lOhlc.low);
    // store last record for use in the next iteration
    lHA_1.open := lHA.open;
    lHA_1.close := lHA.close;

    FHAChart.AddXOHLC( I, lHA.open, lHA.high, lHA.low, lHA.close, lOhlc.date );
  end;
  HAChart.Extent.YMin := lLowest;
  HAChart.Extent.YMax := lHighest;
  HAChart.Extent.UseYMax := True;
  HAChart.Extent.UseYMin := True;
end;

procedure TChartFrame.PrepareArray(AFile: string);
var
  lCSV: TStringList;
  lLine: TStringList;
  I: Integer;
  A: Integer;

begin
  lCSV := TStringList.Create;
  lLine := TStringList.Create;
  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.ShortDateFormat:='D-MMM-Y';
  DefaultFormatSettings.ShortMonthNames[1] := 'Jan';
  DefaultFormatSettings.ShortMonthNames[2] := 'Feb';
  DefaultFormatSettings.ShortMonthNames[3] := 'Mar';
  DefaultFormatSettings.ShortMonthNames[4] := 'Apr';
  DefaultFormatSettings.ShortMonthNames[5] := 'May';
  DefaultFormatSettings.ShortMonthNames[6] := 'Jun';
  DefaultFormatSettings.ShortMonthNames[7] := 'Jul';
  DefaultFormatSettings.ShortMonthNames[8] := 'Aug';
  DefaultFormatSettings.ShortMonthNames[9] := 'Sep';
  DefaultFormatSettings.ShortMonthNames[10] := 'Oct';
  DefaultFormatSettings.ShortMonthNames[11] := 'Nov';
  DefaultFormatSettings.ShortMonthNames[12] := 'Dec';
  try
    lCSV.LoadFromFile(AFile);
    A := 0;
    SetLength(FOHLCArray, 50);
    for I := lCsv.Count - 1 downto 0 do
    begin
      if I = 0 then
        continue;
      if I > Length(FOHLCArray) then
        continue;
      lLine.CommaText:= lCSV[I];
      FOHLCArray[A].open := StrToFloatDef(lLine[1], 0);
      FOHLCArray[A].high := StrToFloatDef(lLine[2], 0);
      FOHLCArray[A].low := StrToFloatDef(lLine[3], 0);
      FOHLCArray[A].close := StrToFloatDef(lLine[4], 0);
      FOHLCArray[A].date:= lLine[0];
      Inc(A);
    end;
    CandleStickChart.Invalidate;
    FHV10 := HV(FOHLCArray, 10);
    FHV20 := HV(FOHLCArray, 20);
    FHV40 := HV(FOHLCArray, 40);
  finally
    lLine.Free;
    lCSV.Free;
  end;
end;

constructor TChartFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FWinList := TObjectList.Create(True);

  FMovingAvg := TLineSeries.Create(nil);
  FMovingAvg.AxisIndexX := 1;
  FMovingAvg.AxisIndexY := 0;
  FMovingAvg.LinePen.Color := clBlue;
  FMovingAvg.LinePen.Width := 2;

  FOHLCSeries := TOpenHighLowCloseSeries.Create(nil);
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
    TChartAxis(AxisList.Items[1]).Grid.Color:= $444444;
    TChartAxis(AxisList.Items[1]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[1]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[1]).Marks.LabelFont.Color:= clWhite;
    TChartAxis(AxisList.Items[0]).Grid.Color:= $444444;
    TChartAxis(AxisList.Items[0]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[0]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[0]).Marks.LabelFont.Color:= clWhite;
  end;

  with HAChart do
  begin
    TChartAxis(AxisList.Items[1]).Grid.Color:= $444444;
    TChartAxis(AxisList.Items[1]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[1]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[1]).Marks.LabelFont.Color:= clWhite;
    TChartAxis(AxisList.Items[0]).Grid.Color:= $444444;
    TChartAxis(AxisList.Items[0]).Grid.Style:= psSolid;
    TChartAxis(AxisList.Items[0]).Grid.Visible:= True;
    TChartAxis(AxisList.Items[0]).Marks.LabelFont.Color:= clWhite;
  end;

  FHAChart := TCandleStickChart.Create(nil);
  FHAChart.AxisIndexX:= 0;
  FHAChart.DownLinePen.Color:= clRed;
  FHAChart.DownLinePen.EndCap:= pecSquare;
  FHAChart.DownLinePen.Width:= 1;
  FHAChart.LinePen.Color:= clLime;
  FHAChart.LinePen.EndCap:= pecSquare;
  FHAChart.LinePen.Width:= 1;
  FHAChart.Source := ListChartSource2;
  FHAChart.TickWidth:= 40;
  FHAChart.ListSource.YCount:= 4;

  CandleStickChart.AddSeries(FOHLCSeries);
  CandleStickChart.AddSeries(FMovingAvg);
  HAChart.AddSeries(FHAChart);
end;

procedure TChartFrame.ChartToolset1DataPointHintTool1HintPosition(
  ATool: TDataPointHintTool; var APoint: TPoint);
begin
  SetHint(FOHLCArray[ATool.PointIndex]);
  HAChart.Invalidate;
  CandleStickChart.Invalidate;
end;

procedure TChartFrame.CloseNewWindow(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FWinList.Remove(Sender);
  CloseAction := caFree;
end;

procedure TChartFrame.SetHint(AOHLCRecord: TOHLCRecord);
begin
  FHint := Format('O: %f, H: %f, L: %f, C: %f, %s',
    [AOHLCRecord.open,
    AOHLCRecord.high,
    AOHLCRecord.low,
    AOHLCRecord.close,
    AOHLCRecord.date]);
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
begin
  // OHLC and Date values
  ext := ASender.CurrentExtent;
  lLeft := ASender.XGraphToImage(ext.a.x);
  lTop := ASender.YGraphToImage(ext.b.y);
  lBottom := ASender.YGraphToImage(ext.a.y);
  lRight := ASender.XGraphToImage(ext.b.x);
  ASender.Canvas.Brush.Style:= bsClear;
  ASender.Canvas.Font.Color:= clWhite;
  ASender.Canvas.TextOut(lLeft + 2, lTop, FHint);

  // Moving Average values
  lXPos := ASender.ScreenToClient(Mouse.CursorPos).X;

  lIndex := Round(ASender.XImageToGraph(lXPos));

  if (lIndex > FMovingAvg.Count - 1) or (lIndex < 0) then
  begin
    lIndex := FMovingAvg.Count - 1;
  end;

  lStr := Format('SMA 5: %.2f', [FMovingAvg.GetYValue(lIndex)]);
  lFontHeight := ASender.Canvas.GetTextHeight(lStr);
  ASender.Canvas.Font.Color:= clBlue;
  ASender.Canvas.TextOut(lLeft + 2, lTop + 2 + lFontHeight, lStr);

  // Historical Volatility
  lStr := Format('HV 40: %.2f, HV 20: %.2f, HV 10: %.2f', [FHV40, FHV20, FHV10]);
  lFontHeight := ASender.Canvas.GetTextHeight(lStr);
  ASender.Canvas.Font.Color:= clGray;
  ASender.Canvas.TextOut(lLeft + 2, lTop + 2 + (lFontHeight * 2), lStr);

  // close
  if ASender.Name = 'CandleStickChart' then
  begin
    lIndex:= Length(FOHLCArray) - 1;
    lChartPos:= FOHLCArray[lIndex].close;
    lStr := Format('%.2f', [lChartPos]);
    lFontHeight:= ASender.Canvas.Font.GetTextHeight(lStr);
    lTextWidth:= ASender.Canvas.Font.GetTextWidth(lStr);
    if FOHLCArray[lIndex].close < FOHLCArray[lIndex].open then
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
      lRight + 6,
      Round(ASender.YGraphToImage(lChartPos) - (lFontHeight / 2)),
      lStr);
  end;

  // cursor line
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
end;

procedure TChartFrame.actRefreshExecute(Sender: TObject);
var
  I: Integer;
  lWin: TNewWindow;
begin
  SetLength(FOHLCArray, 0);
  Display(FFile);

  for I := 0 to FWinList.Count - 1 do
  begin
    lWin := FWinList[I] as TNewWindow;
    ((lWin.Controls[0]) as TChartFrame).actRefresh.Execute;
  end;
end;

procedure TChartFrame.actNewWindowExecute(Sender: TObject);
var
  lWin: TNewWindow;
  lChart: TChartFrame;
begin
  lWin := TNewWindow.Create(nil);
  lWin.OnClose := @CloseNewWindow;
  lWin.Caption:= FFile;
  FWinList.Add(lWin);
  lChart := TChartFrame.Create(lWin);
  lChart.Parent := lWin;
  lChart.Align:= alClient;
  lChart.Display(FFile);
  lWin.Show;
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
  lTextWidth := ACanvas.GetTextWidth(FFile);
  lTextHeight:= ACanvas.GetTextHeight(FFile);
  lX := Round(((ARect.Right - ARect.Left) / 2) - (lTextWidth / 2));
  lY := Round(((ARect.Bottom - ARect.Top) / 2) - (lTextHeight / 2));
  ACanvas.TextOut(lX, lY, FFile);
end;

destructor TChartFrame.destroy;
begin
  FWinList.Free;
  FMovingAvg.Free;
  FHAChart.Free;
  FOHLCSeries.Free;
  inherited;
end;


end.


