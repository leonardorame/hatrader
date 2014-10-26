unit ohlc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FGL;

type

  { TOHLCRecord }

  TOHLCRecord = class
  private
    FOpen: double;
    FHigh: double;
    FLow: double;
    FClose: double;
    FDate: string;
    FPrev: double;
    FTime: string;
    FVolume: Integer;
  public
    property Open: double read FOpen write FOpen;
    property High: double read FHigh write FHigh;
    property Low: double read FLow write FLow;
    property Close: double read FClose write FClose;
    property Date: string read FDate write FDate;
    property Time: string read FTime write FTime;
    property Volume: Integer read FVolume write FVolume;
    property Prev: double read FPrev write FPrev;
  end;

  TOHLCArray = specialize TFPGList<TOHLCRecord>;


implementation

end.

