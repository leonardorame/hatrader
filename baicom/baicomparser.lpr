program baicomparser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fgl
  { you can add units after this };

type

  { TBaicomParser }

  { TOHLC }

  TOHLC = class
  private
    FClose: double;
    FHigh: double;
    FLow: double;
    FOpen: double;
    FSym: string;
    FVarPct: string;
  public
    function AddString(AString: string): boolean;
    property Sym: string read FSym write FSym;
    property VarPct: string read FVarPct write FVarPct;
    property Open: double read FOpen write FOPen;
    property High: double read FHigh write FHigh;
    property Low: double read FLow write FLow;
    property Close: double read FClose write FClose;
  end;

  TOHLCList = specialize TFPGList<TOHLC>;

  TBaicomParser = class(TCustomApplication)
  private
    procedure parse(AString: TStringList);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

function TOHLC.AddString(AString: string): boolean;
begin
  Result := True;
  AString := Copy(Astring, Pos('>', AString) + 1, Length(AString));
  if Copy(AString, 1, 2) = '<a' then
  begin
    AString := Copy(AString, Pos('>', Copy(AString, 2, Length(AString))) + 2, Length(AString));
  end;
  AString := Copy(AString, 1, Pos('<', AString) - 1);
  if FSym = '' then
    FSym:= AString
  else
  if FClose = 0 then
    FClose := StrToFloat(AString)
  else
  if FVarPct = '' then
    FVarPct:= AString
  else
  if FHigh = 0 then
    FHigh := StrToFloat(AString)
  else
  if FLow = 0 then
    FLow := StrToFloat(AString)
  else
  if FOpen = 0 then
    FOpen := StrToFloat(AString)
  else
    Result := False;
end;

{ TBaicomParser }

procedure TBaicomParser.parse(AString: TStringList);
var
  I: Integer;
  lParsing: Boolean;
  lList: TOHLCList;
  lOHLC: TOHLC;
  lStr: string;
begin
  lParsing := False;
  lList := TOHLCList.Create;
  for I := 0 to AString.Count - 1 do
  begin
    lStr := AString[I];
    if Copy(AString[I], 1, 3) = '<th' then
      continue;
    if Trim(AString[I]) = '<tr>' then
    begin
      if lOHLC <> nil then
        lList.Add(lOHLC);
      lParsing := True;
      lOHLC := TOHLC.Create;
      continue;
    end;
    try
    if lParsing then
      lParsing := lOHLC.AddString(AString[I]);

    except
      on E: Exception do
      begin
        Write('Error: ' + E.message);
        Write(Astring[I]);
      end;
    end;
  end;

  lStr := Copy(AString.Text, Pos('Actualizaci', AString.Text) + 22, 19);
  Writeln('Actualizacion:' + lStr);
  for lOHLC in lList do
    if lOHLC.Sym = '' then
      continue
    else
    Writeln(Format('"%s";%f;%f;%f;%f', [
      lOHLC.Sym,
      lOHLC.Open,
      lOHLC.High,
      lOHLC.Low,
      lOHLC.Close]));
  lList.Free;
end;

procedure TBaicomParser.DoRun;
var
  lStr: TStringList;
  lIn: string;
begin
  lStr := TStringList.Create;
  While not EOF do
  begin
    ReadLn(lIn);
    lStr.Text := lStr.Text + lIn;
  end;
  writeln;
  try
    Parse(lStr);
  finally
    lStr.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TBaicomParser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBaicomParser.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TBaicomParser;
begin
  Application:=TBaicomParser.Create(nil);
  Application.Title:='Baicom Parser';
  Application.Run;
  Application.Free;
end.

