program HATrader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, chartframe, tachartlazaruspkg, candlestickchart, main, newwindow,
  volatility, ohlc;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THeikinAshiTrader, HeikinAshiTrader);
  Application.Run;
end.

