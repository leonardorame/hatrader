unit volatility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ohlc, math;

function HV(AArr: TOHLCArray; ADays: Integer): double;

implementation

function HV(AArr: TOHLCArray; ADays: Integer): double;
var
  I: Integer;
  A: Integer;
  lPct: array of double;
  lFrom: Integer;
  lTo: Integer;
begin
  //lStr := TStringList.Create;
  // obtenemos la variación entre precios:
  SetLength(lPct, ADays);
  A := 0;
  // el array comienza en los valores más antiuos
  lTo := AArr.Count - 1;
  for I := lTo - 1 downto 1 do
  begin
    lPct[A] := (AArr[I].close / AArr[I + 1].close) - 1;
    //lStr.Add(Format('I: %d, Close: %f, Pct: %.4f', [I, AArr[I].Close, lPct[A]]));
    inc(A);
    if A = Length(lPct) then
      break;
  end;

  // Una vez obtenido el array,
  //lStr.SaveToFile('salida.txt');
  //lStr.Free;
  Result := stddev(lPct) * sqrt(254) * 100;
end;

end.

