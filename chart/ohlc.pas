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
    FTime: string;
  public
    property Open: double read FOpen write FOpen;
    property High: double read FHigh write FHigh;
    property Low: double read FLow write FLow;
    property Close: double read FClose write FClose;
    property Date: string read FDate write FDate;
    property Time: string read FTime write FTime;
  end;

  TOHLCArray = specialize TFPGList<TOHLCRecord>;


implementation

end.

