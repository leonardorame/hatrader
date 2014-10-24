unit symbols;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, ohlc, SysUtils, StrUtils,
  volatility;

type

  { TSymbol }

  TSymbolType = (stDaily, stIntraday);

  TSymbol = class
  private
    FFilePath: string;
    FName: string;
    FOHLCArray: TOHLCArray;
    FHV10: double;
    FHV20: double;
    FHV40: double;
    FOnDataChanged: TNotifyEvent;
    FSymbol: string;
    FSymbolType: TSymbolType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PrepareArray(AData: string);
    property Name: string read FName write FName;
    property Symbol: string read FSymbol write FSymbol;
    property FilePath: string read FFilePath write FFilePath;
    property Data: TOHLCArray read FOHLCArray;
    property HV10: double read FHV10 write FHV10;
    property HV20: double read FHV20 write FHV20;
    property HV40: double read FHV40 write FHV40;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property SymbolType: TSymbolType read FSymbolType write FSymbolType;
  end;

  { TGSymbolList }

  generic TGSymbolList<T> = class(specialize TFPGList<T>)
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadData;
    procedure AddSymbol(AName, ASymbol, APath: string);
  end;

  TSymbols = specialize TGSymbolList<TSymbol>;

implementation

{ TSymbol }

constructor TSymbol.Create;
begin
  FSymbolType:= stDaily;
  FOHLCArray := TOHLCArray.Create;
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
end;

destructor TSymbol.Destroy;
var
  lOHLC: TOHLCRecord;
begin
  for lOHLC in FOHLCArray do
    lOHLC.Free;
  FOHLCArray.Free;
  inherited Destroy;
end;

procedure TSymbol.PrepareArray(AData: string);
var
  lCSV: TStringList;
  lLine: TStringList;
  I: Integer;
  A: Integer;
  lOHLC: TOHLCRecord;

const
  cMaxSize = 50;

begin
  for lOHLC in FOHLCArray do
    lOHLC.Free;
  FOHLCArray.Clear;
  lCSV := TStringList.Create;
  lLine := TStringList.Create;
  try
    lCSV.Text:= AData;
    if lCSV.Count = 0 then
      exit;
    A := 0;
    for I := lCsv.Count - 1 downto 0 do
    begin
      if (FSymbolType = stDaily) and (I = 0) then
        continue;
      if I > cMaxSize then
        continue;
      lLine.CommaText:= lCSV[I];
      lOHLC := TOHLCRecord.Create;
      lOHLC.open := StrToFloatDef(lLine[1], 0);
      lOHLC.high := StrToFloatDef(lLine[2], 0);
      lOHLC.low := StrToFloatDef(lLine[3], 0);
      lOHLC.close := StrToFloatDef(lLine[4], 0);
      lOHLC.date:= lLine[0];
      if FSymbolType = stIntraday then
        lOHLC.time:= lLine[5];
      FOHLCArray.Add(lOHLC);
      Inc(A);
    end;
    //CandleStickChart.Invalidate;
    FHV10 := HV(FOHLCArray, 10);
    FHV20 := HV(FOHLCArray, 20);
    FHV40 := HV(FOHLCArray, 40);

    if Assigned(FOnDataChanged) then
       FOnDataChanged(Self);
  finally
    lLine.Free;
    lCSV.Free;
  end;
end;

{ TSymbols }

constructor TGSymbolList.Create;
begin
  inherited Create;
end;

destructor TGSymbolList.Destroy;
var
  lSymbol: TSymbol;
begin
  for lSymbol in Self do
  begin
    lSymbol.Free;
  end;
  inherited Destroy;
end;

procedure TGSymbolList.LoadData;
var
  lList: TStringList;
  lLine: TStringList;
  I: Integer;
  lName: string;
  lPath: string;
  lSymbol: string;
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
      lSymbol := AnsiReplaceStr(lLine[1], '"', '');
      lPath := AnsiReplaceStr(lLine[2], '"', '');
      AddSymbol(lName, lSymbol, lPath);
    end;
  finally
    lLine.Free;
    lList.Free;
  end;
end;

procedure TGSymbolList.AddSymbol(AName, ASymbol, APath: string);
var
  lSymbol: TSymbol;
begin
  lSymbol := TSymbol.Create;
  lSymbol.FilePath := APath;
  lSymbol.Name := AName;
  lSymbol.Symbol:= ASymbol;
  Add(lSymbol);
end;

end.

