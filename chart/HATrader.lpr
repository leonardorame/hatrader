program HATrader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, chartframe, tachartlazaruspkg, candlestickchart, main, newwindow,
  volatility, ohlc, symbols, loader;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THeikinAshiTrader, HeikinAshiTrader);
  Application.Run;
end.

