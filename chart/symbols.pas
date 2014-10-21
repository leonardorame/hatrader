unit symbols;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, ohlc, SysUtils, StrUtils,
  volatility;

type

  { TSymbol }

  TSymbol = class
  private
    FFilePath: string;
    FName: string;
    FOHLCArray: TOHLCArray;
    FHV10: double;
    FHV20: double;
    FHV40: double;
  public
    procedure PrepareArray(AData: string);
    procedure SetDataLength(ALen: Integer);
    property Name: string read FName write FName;
    property FilePath: string read FFilePath write FFilePath;
    property Data: TOHLCArray read FOHLCArray write FOHLCArray;
    property HV10: double read FHV10 write FHV10;
    property HV20: double read FHV20 write FHV20;
    property HV40: double read FHV40 write FHV40;
  end;

  { TGSymbolList }

  generic TGSymbolList<T> = class(specialize TFPGList<T>)
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadData;
    procedure AddSymbol(AName, APath: string);
  end;

  TSymbols = specialize TGSymbolList<TSymbol>;

implementation

{ TSymbol }

procedure TSymbol.PrepareArray(AData: string);
var
  lCSV: TStringList;
  lLine: TStringList;
  I: Integer;
  A: Integer;

begin
  lCSV := TStringList.Create;
  lLine := TStringList.Create;
  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.ShortDateFormat:='D-MMM-Y';
  DefaultFormatSettings.ShortMonthNames[1] := 'Jan';
  DefaultFormatSettings.ShortMonthNames[2] := 'Feb';
  DefaultFormatSettings.ShortMonthNames[3] := 'Mar';
  DefaultFormatSettings.ShortMonthNames[4] := 'Apr';
  DefaultFormatSettings.ShortMonthNames[5] := 'May';
  DefaultFormatSettings.ShortMonthNames[6] := 'Jun';
  DefaultFormatSettings.ShortMonthNames[7] := 'Jul';
  DefaultFormatSettings.ShortMonthNames[8] := 'Aug';
  DefaultFormatSettings.ShortMonthNames[9] := 'Sep';
  DefaultFormatSettings.ShortMonthNames[10] := 'Oct';
  DefaultFormatSettings.ShortMonthNames[11] := 'Nov';
  DefaultFormatSettings.ShortMonthNames[12] := 'Dec';
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    lCSV.Text:= AData;
    if lCSV.Count = 0 then
      exit;
    A := 0;
    SetDataLength(50);
    for I := lCsv.Count - 1 downto 0 do
    begin
      if I = 0 then
        continue;
      if I > Length(FOHLCArray) then
        continue;
      lLine.CommaText:= lCSV[I];
      FOHLCArray[A].open := StrToFloatDef(lLine[1], 0);
      FOHLCArray[A].high := StrToFloatDef(lLine[2], 0);
      FOHLCArray[A].low := StrToFloatDef(lLine[3], 0);
      FOHLCArray[A].close := StrToFloatDef(lLine[4], 0);
      FOHLCArray[A].date:= lLine[0];
      Inc(A);
    end;
    //CandleStickChart.Invalidate;
    FHV10 := HV(FOHLCArray, 10);
    FHV20 := HV(FOHLCArray, 20);
    FHV40 := HV(FOHLCArray, 40);
  finally
    lLine.Free;
    lCSV.Free;
  end;
end;

procedure TSymbol.SetDataLength(ALen: Integer);
begin
  SetLength(FOHLCArray, ALen);
end;

{ TSymbols }

constructor TGSymbolList.Create;
begin
  inherited Create;
end;

destructor TGSymbolList.Destroy;
begin
  inherited Destroy;
end;

procedure TGSymbolList.LoadData;
var
  lList: TStringList;
  lLine: TStringList;
  I: Integer;
  lName: string;
  lPath: string;
begin
  lList := TStringList.Create;
  lLine := TStringList.Create;
  try
    lList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'files.list');
    // Las líneas del archivo tienen el formato:
    // "SYMBOLO";"PATH"
    for I := 0 to lList.Count - 1 do
    begin
      lLine.Delimiter:= ';';
      lLine.DelimitedText := lList[I];
      lName := AnsiReplaceStr(lLine[0], '"', '');
      lPath := AnsiReplaceStr(lLine[1], '"', '');
      AddSymbol(lName, lPath);
    end;
  finally
    lLine.Free;
    lList.Free;
  end;
end;

procedure TGSymbolList.AddSymbol(AName, APath: string);
var
  lSymbol: TSymbol;
begin
  lSymbol := TSymbol.Create;
  lSymbol.FilePath := APath;
  lSymbol.Name := AName;
  Add(lSymbol);
end;

end.

