program jsonbolsar;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  fpjson, jsonparser;

type

  { TJsonBolsar }

  TJsonBolsar = class(TCustomApplication)
  private
    procedure Parse(AStringList: TStringList; ATipo: string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TJsonBolsar }

procedure TJsonBolsar.Parse(AStringList: TStringList; ATipo: string);
var
  lJSON: TJSONObject;
  lArray: TJSONArray;
  lParser: TJSONParser;
  I: Integer;
begin
  lParser := TJSONParser.Create(AStringList.Text);
  try
    lJSON := TJSONObject(lParser.Parse);
    for I := 0 to lJSON.Arrays['d'].Count - 1 do
    begin
      if TJsonObject(lJSON.Arrays['d'][I]).Strings['TablaNombre'] = ATipo then
      begin
        lJson := TJsonObject(lJSON.Arrays['d'][I]);
        break;
      end;
    end;
    lArray := lJSON.Arrays['aTabla'];
    Writeln(
      'Sym' + #9#9 +
      'Last' + #9 +
      'BQty.' + #9 +
      'Bid' + #9 +
      'Ask' + #9 +
      'AQty.' + #9 +
      'Vol' + #9 +
      'Time');
    for I := 0 to lArray.Count - 1 do
    begin
      Writeln(
        TJsonObject(lArray[I]).Strings['Simbolo'] + #9 +
        Format('%.3f', [TJsonObject(lArray[I]).Floats['PrecioUltimo']]) + #9 +
        TJsonObject(lArray[I]).Strings['CantidadNominalCompra'] + #9 +
        Format('%.3f', [TJsonObject(lArray[I]).Floats['PrecioCompra']]) + #9 +
        Format('%.3f', [TJsonObject(lArray[I]).Floats['PrecioVenta']]) + #9 +
        TJsonObject(lArray[I]).Strings['CantidadNominalVenta'] + #9 +
        TJsonObject(lArray[I]).Strings['VolumenNominal'] + #9 +
        TJsonObject(lArray[I]).Strings['HoraCotizacion']);
    end;
  finally
    lParser.Free;
  end;

end;

procedure TJsonBolsar.DoRun;
var
  ErrorMsg: String;
  lStr: TStringList;
  lIn: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  lStr := TStringList.Create;
  While not EOF do
  begin
    ReadLn(lIn);
    lStr.Text := lStr.Text + lIn;
  end;
  writeln;
  try
    Parse(lStr, ParamStr(1));
  finally
    lStr.Free;
  end;



  // stop program loop
  Terminate;
end;

constructor TJsonBolsar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TJsonBolsar.Destroy;
begin
  inherited Destroy;
end;

procedure TJsonBolsar.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TJsonBolsar;
begin
  Application:=TJsonBolsar.Create(nil);
  Application.Title:='JSON Bolsar Parser';
  Application.Run;
  Application.Free;
end.

