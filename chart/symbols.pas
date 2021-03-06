unit symbols;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, ohlc, SysUtils, StrUtils,
  //fphttpclient,
  httpsend,
  volatility;

type

  { TSymbol }

  TSymbolType = (stDaily, stIntraday);

  TSymbol = class
  private
    FDaily: TSymbol;
    FFilePath: string;
    FName: string;
    FOHLCArray: TOHLCArray;
    FHV10: double;
    FHV20: double;
    FHV40: double;
    FOnDataChanged: TNotifyEvent;
    FSymbol: string;
    FSymbolType: TSymbolType;
    FLast: TOHLCRecord;
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
    property Last: TOHLCRecord read FLast write FLast;
    property Daily: TSymbol read FDaily write FDaily;
  end;

  { TCCLSymbol }

  TCCLSymbol = class(TSymbol)
  private
    FFactor: integer;
    FLocal: TSymbol;
    FUsa: TSymbol;
  public
    property Factor: integer read FFactor write FFactor;
    property Local: TSymbol read FLocal write FLocal;
    property Usa: TSymbol read FUsa write FUsa;
  end;

  { TGSymbolList }

  generic TGSymbolList<T> = class(specialize TFPGList<T>)
  public
    constructor Create;
    destructor Destroy; override;
    function Find(ASymbol: string): TSymbol;
    procedure LoadInitialData;
    procedure UpdateSymbolData(AData: string);
  end;

  TSymbols = specialize TGSymbolList<TSymbol>;
  TCCLSymbols = specialize TGSymbolList<TCCLSymbol>;

implementation

{ TSymbol }

constructor TSymbol.Create;
begin
  FSymbolType:= stDaily;
  FOHLCArray := TOHLCArray.Create;
  FLast := TOHLCRecord.Create;
  FDaily := nil;
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
  FLast.Free;
  inherited Destroy;
end;

procedure TSymbol.PrepareArray(AData: string);
var
  lCSV: TStringList;
  lLine: TStringList;
  I: Integer;
  lOHLC: TOHLCRecord;
  lStr: string;

begin
  // si el contenido de AData es un
  // mensaje de error, comienza
  // con <html><head><....
  // en ese caso salimos sin procesar.
  if Pos('<', AData) > 0 then
    exit;

  for lOHLC in FOHLCArray do
    lOHLC.Free;
  FOHLCArray.Clear;
  lCSV := TStringList.Create;
  lLine := TStringList.Create;
  try
    lCSV.Text:= AData;
    for I := lCsv.Count - 1 downto 0 do
    begin
      lLine.CommaText:= lCSV[I];
      lOHLC := TOHLCRecord.Create;
      lOHLC.open := StrToFloatDef(lLine[1], 0);
      lOHLC.high := StrToFloatDef(lLine[2], 0);
      lOHLC.low := StrToFloatDef(lLine[3], 0);
      lOHLC.close := StrToFloatDef(lLine[4], 0);
      lOHLC.date:= lLine[0];
      if FSymbolType = stIntraday then
      begin
        lOHLC.time:= lLine[5];
      end
      else
        lOHLC.Volume:= StrToIntDef(lLine[5], 0);

      FOHLCArray.Add(lOHLC);
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

function TGSymbolList.Find(ASymbol: string): TSymbol;
var
  lSymbol: TSymbol;
begin
  Result := nil;
  for lSymbol in Self do
  begin
    if lSymbol.Symbol = ASymbol then
    begin
      Result := lSymbol;
    end;
  end;
end;

procedure TGSymbolList.LoadInitialData;
var
  lStr: TStringList;
  lLine: TStringList;
  I: Integer;
  lSymbol: T;
  //lHttpClient: TFPHTTPClient;
begin
  //lHttpClient := TFPHTTPClient.Create(nil);
  lStr := TStringList.Create;
  lLine := TStringList.Create;
  lLine.Delimiter:= ',';
  try
    try
      //lHttpClient.Get('http://www.ceciliastrada.com.ar/cgi-bin/intraday.bf/all', lStr);
      HttpGetText('http://www.ceciliastrada.com.ar/cgi-bin/intraday.bf/all', lStr);
      for I := 0 to lStr.Count - 1 do
      begin
        lLine.DelimitedText:= lStr[I];
        lSymbol := T.Create;
        lSymbol.Name := lLine[1];
        lSymbol.Symbol:= lLine[1];
        lSymbol.SymbolType:= stDaily;
        lSymbol.FilePath:= 'http://www.ceciliastrada.com.ar/cgi-bin/intraday.bf/daily?sym=' + lSymbol.Symbol;
        lSymbol.Last.Date:= lLine[0];
        lSymbol.Last.Open:= StrToFloatDef(lLine[2], 0);
        lSymbol.Last.High:= StrToFloatDef(lLine[3], 0);
        lSymbol.Last.Low:= StrToFloatDef(lLine[4], 0);
        lSymbol.Last.Close:= StrToFloatDef(lLine[5], 0);
        lSymbol.Last.Volume:= StrToIntDef(lLine[6], 0);
        lSymbol.Last.Prev:= StrToFloatDef(lLine[7], 0);
        Add(lSymbol);
      end;
    except
      raise;
    end;
  finally
    lLine.Free;
    lStr.Free;
    //lHttpClient.Free;
  end;
end;

procedure TGSymbolList.UpdateSymbolData(AData: string);
var
  lStr: TStringList;
  lLine: TStringList;
  I: Integer;
  lSymbol: TSymbol;
begin
  lStr := TStringList.Create;
  lLine := TStringList.Create;
  lLine.Delimiter:= ',';
  try
    lStr.Text := AData;
    for I := 0 to lStr.Count - 1 do
    begin
      lLine.DelimitedText:= lStr[I];
      lSymbol := nil;
      for lSymbol in Self do
      begin
        if lLine.Count = 8 then
          if lSymbol.Symbol = lLine[1] then
          begin
            lSymbol.Symbol:= lLine[1];
            lSymbol.Last.Date:= lLine[0];
            lSymbol.Last.Open:= StrToFloatDef(lLine[2], 0);
            lSymbol.Last.High:= StrToFloatDef(lLine[3], 0);
            lSymbol.Last.Low:= StrToFloatDef(lLine[4], 0);
            lSymbol.Last.Close:= StrToFloatDef(lLine[5], 0);
            lSymbol.Last.Volume:= StrToIntDef(lLine[6], 0);
            lSymbol.Last.Prev:= StrToFloatDef(lLine[7], 0);
          end;
      end;
    end;
  finally
    lLine.Free;
    lStr.Free;
  end;
end;

end.

