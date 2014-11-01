unit symbolgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, grids, symbols, Graphics;

type

  { TSymbolGrid }

  TSymbolGrid = class(TStringGrid)
  private
    FSymbols: TSymbols;
  protected
    procedure DrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public
    constructor Create(ASymbols: TSymbols; AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TSymbolGrid }

constructor TSymbolGrid.Create(ASymbols: TSymbols; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSymbols := ASymbols;
  ParentColor:= True;
  AlternateColor := StringToColor('1118481');
  AutoFillColumns:= True;
  Color := StringToColor('1973790');
  ColCount := 7;
  ExtendedSelect := False;
  FixedColor := StringToColor('3355443');
  FixedCols := 0;
  Font.Color := StringToColor('9605778');
  GridLineWidth := 0;
  Options := [goFixedVertLine, goFixedHorzLine, goHorzLine, goRowSelect, goThumbTracking, goSmoothScroll];
  ParentFont := False;
  RowCount := 2;
  TabOrder := 0;
  TitleFont.Color := StringToColor('9605778');
  with Columns.Add do
  begin
    Title.Alignment := taCenter;
    Title.Caption := 'Symbol';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := 'Last';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := 'Open';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := 'High';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := 'Low';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := 'Prev';
    Width := 83;
  end;

  with Columns.Add do
  begin
    Alignment:= taCenter;
    Title.Alignment := taCenter;
    Title.Caption := '%';
    Width := 83;
  end;

  OnDrawCell:= @DrawCell;
end;

destructor TSymbolGrid.Destroy;
begin
  RowCount:= 0;
  inherited Destroy;
end;

procedure TSymbolGrid.DrawCell(Sender: TObject; aCol,
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
    Canvas.Brush.Style:= bsSolid;
    Canvas.Brush.Color:= $442200;
    Canvas.FillRect(aRect);
    Canvas.Font.Color := $AFAFAF;
  end
  else
    Canvas.Font.Color := $AFAFAF;
  if ARow > 0 then
  begin
    lCanvas := Canvas;
    lCellHeigh := aRect.Bottom - aRect.Top;
    lSymbol := TSymbol(Objects[0, ARow]);
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
         if lPrev = 0 then
           lPrev := lClose;

         lText := Format('%.2f%%',[((lClose / lPrev) - 1) * 100]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
    end;
  end;
end;


end.

