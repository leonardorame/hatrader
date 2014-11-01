unit cclgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, grids, symbols, Graphics;

type

  { TCCLGrid }

  TCCLGrid = class(TStringGrid)
  private
    FSymbols: TCCLSymbols;
  protected
    procedure DrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public
    constructor Create(ASymbols: TCCLSymbols; AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TCCLGrid }

constructor TCCLGrid.Create(ASymbols: TCCLSymbols; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSymbols := ASymbols;
  ParentColor:= True;
  AlternateColor := StringToColor('1118481');
  //AutoFillColumns:= True;
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

  OnDrawCell:= @DrawCell;
end;

destructor TCCLGrid.Destroy;
begin
  RowCount:= 0;
  inherited Destroy;
end;

procedure TCCLGrid.DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  lSymbol: TCCLSymbol;
  lX: Integer;
  lY: Integer;
  lCellHeigh: Integer;
  lCanvas: TCanvas;
  lText: string;
  lLast: double;
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
    lSymbol := TCCLSymbol(Objects[0, ARow]);
    lLast := lSymbol.Last.Close;
    case ACol of
      0: begin
         lText := lSymbol.Name;
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lCanvas.TextOut(aRect.Left + 2, lY, lText);
      end;

      1: begin
         lText := Format('%.2f',[lLast]);
         lY := aRect.Top + Round((lCellHeigh / 2) - (lCanvas.TextHeight(lText) / 2));
         lX := aRect.Left + ((aRect.Right - aRect.Left) - (lCanvas.TextWidth(lText) + 4));
         lCanvas.TextOut(lX, lY, lText);
      end;
    end;
  end;
end;


end.

