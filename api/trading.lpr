program trading;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  BrookApplication, Brokers, dm, v1.acciones.fecha, v1.opciones.fecha;

begin
  Application.CreateForm(TBrookDataModule1, BrookDataModule1);
  Application.Run;
end.