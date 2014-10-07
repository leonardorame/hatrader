unit candlestickchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAMultiSeries, TADrawUtils, TAChartUtils, TAGeometry, TAMath,
  TACustomSeries, TASeries, Math, Types;

type

  { TCandleStickChart }

  TCandleStickChart = class(TOpenHighLowCloseSeries)
  public
    procedure Draw(ADrawer: IChartDrawer); override;
  end;

implementation

{ TCandleStickChart }

procedure TCandleStickChart.Draw(ADrawer: IChartDrawer);

  function MaybeRotate(AX, AY: Double): TPoint;
  begin
    if IsRotated then
      Exchange(AX, AY);
    Result := ParentChart.GraphToImage(DoublePoint(AX, AY));
  end;

  procedure DoLine(AX1, AY1, AX2, AY2: Double);
  begin
    ADrawer.Line(MaybeRotate(AX1, AY1), MaybeRotate(AX2, AY2));
  end;

  function GetGraphPointYIndex(AIndex, AYIndex: Integer): Double;
  begin
    if AYIndex = 0 then
      Result := GetGraphPointY(AIndex)
    else
      Result := AxisToGraphY(Source[AIndex]^.YList[AYIndex - 1]);
  end;

  procedure DoRect(AX1, AY1, AX2, AY2: Double);
  var
    r: TRect;
  begin
    with ParentChart do begin
      r.TopLeft := MaybeRotate(AX1, AY1);
      r.BottomRight := MaybeRotate(AX2, AY2);
    end;
    if r.top = r.bottom then
    begin
      r.Top := r.Top - 1;
      r.Bottom:= r.Bottom + 1;
    end;
    ADrawer.FillRect(r.Left, r.Top, r.Right, r.Bottom);
    ADrawer.Rectangle(r);
  end;

var
  my: Cardinal;
  ext2: TDoubleRect;
  i: Integer;
  x, tw, yopen, yhigh, ylow, yclose: Double;
  p: TPen;
begin
  my := MaxIntValue([YIndexOpen, YIndexHigh, YIndexLow, YIndexClose]);
  if IsEmpty or (my >= Source.YCount) then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  PrepareGraphPoints(ext2, true);

  for i := FLoBound to FUpBound do begin
    x := GetGraphPointX(i);
    yopen := GetGraphPointYIndex(i, YIndexOpen);
    yhigh := GetGraphPointYIndex(i, YIndexHigh);
    ylow := GetGraphPointYIndex(i, YIndexLow);
    yclose := GetGraphPointYIndex(i, YIndexClose);
    tw := GetXRange(x, i) * PERCENT * TickWidth;

    if (DownLinePen.Color = clTAColor) or (yopen <= yclose) then
      p := LinePen
    else
      p := DownLinePen;
    ADrawer.BrushColor:= P.Color;
    // set border black
    ADrawer.SetPenParams(p.Style, P.Color);
    DoLine(x, yhigh, x, ylow);
    DoRect(x - tw, yopen, x + tw, yclose);
  end;
end;


end.

