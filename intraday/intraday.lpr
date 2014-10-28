program intraday;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  BrookApplication, Brokers, main, dailydata, alldata, ccl;

begin
  BrookApp.Run;
end.
