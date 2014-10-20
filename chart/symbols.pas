unit symbols;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, ohlc, SysUtils, StrUtils;

type

  { TSymbol }

  TSymbol = class
  private
    FFilePath: string;
    FName: string;
    FOHLCArray: TOHLCArray;
  public
    property Name: string read FName write FName;
    property FilePath: string read FFilePath write FFilePath;
    property Data: TOHLCArray read FOHLCArray write FOHLCArray;
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

