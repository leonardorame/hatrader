unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  Classes,
  SysUtils,
  fgl,
  strutils,
  fphttpclient,
  sqldb,
  mysql55conn;

type

  { TOHLC }

  TOHLC = class
  private
    FClose: double;
    FDate: string;
    FHigh: double;
    FLow: double;
    FOpen: double;
    FSym: string;
    FVarPct: string;
    FTime: string;
    FPrevClose: double;
    FVol: string;
    FCant: Integer;
  public
    function AddString(AString: string): boolean;
    property Sym: string read FSym write FSym;
    property VarPct: string read FVarPct write FVarPct;
    property Open: double read FOpen write FOPen;
    property High: double read FHigh write FHigh;
    property Low: double read FLow write FLow;
    property Close: double read FClose write FClose;
    property PrevClose: double read FPrevClose write FPrevClose;
    property Date: string read FDate write FDate;
    property Time: string read FTime write FTime;
    property Vol: string read FVol write FVol;
    property Cant: Integer read FCant write FCant;
  end;

  TOHLCList = specialize TFPGList<TOHLC>;


  { TMyAction }

  TMyAction = class(TBrookAction)
  private
    procedure Parse(AStr: TStringList);
    procedure AddToDaily(ADate, ASym, AOpen, AHigh, ALow, AClose: string);
    procedure AddToRT(ADate, ASym, AOpen, AHigh, ALow, AClose, ATime: string);
    function Procesar(AString: TStringList): string;
  public
    procedure Get; override;
  end;

implementation

{ TOHLC }

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
    FClose := StrToFloatDef(AString, 0)
  else
  if FVarPct = '' then
    FVarPct:= AString
  else
  if FHigh = 0 then
    FHigh := StrToFloatDef(AString, 0)
  else
  if FLow = 0 then
    FLow := StrToFloatDef(AString, 0)
  else
  if FOpen = 0 then
    FOpen := StrToFloatDef(AString, 0)
  else
  if FPrevClose = 0 then
    FPrevClose := StrToFloatDef(AString, 0)
  else
  if FVol = '' then
    FVol := Astring
  else
  if FCant = 0 then
    FCant := StrToIntDef(AString, 0)
  else
  if FTime = '' then
    FTime := AString
  else
    Result := False;
end;

{ TMyAction }

procedure TMyAction.Parse(AStr: TStringList);
var
  lDate: TDateTime;
  lDateStr: string;
  lSym: string;
  lOpen: string;
  lHigh: string;
  lLow: string;
  lClose: string;
  lTime: string;
  I: Integer;
  lLine: TStringList;
begin
  // 1er linea: Actualizacion:2014-09-23 17:59:22
  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.ShortDateFormat:='YYYY-MM-DD';

  lDate := StrToDate(Copy(AStr[0], 15, 10));

  DefaultFormatSettings.DateSeparator:='-';
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
  lDateStr := FormatDateTime('D-MMM-Y', lDate);
  lLine := TStringList.Create;
  lLine.Delimiter:=';';
  try
    for I := 1 to AStr.Count - 1 do
    begin
      lLine.DelimitedText:= AStr[I];
      lSym := AnsiReplaceStr(lLine[0], '"', '');
      lOpen := lLine[1];
      lHigh := lLine[2];
      lLow := lLine[3];
      lClose := lLine[4];
      lDateStr := lLine[5];
      lTime := lLine[6];
      AddToDaily(lDateStr, lSym, lOpen, lHigh, lLow, lClose);
      AddToRT(lDateStr, lSym, lOpen, lHigh, lLow, lClose, lTime);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TMyAction.AddToDaily(ADate, ASym, AOpen, AHigh, ALow, AClose: string);
var
  searchResult : TSearchRec;
  lStr: TStringList;
  lLine: TStringList;
  lIndex: Integer;
  I: Integer;
  lData: string;
  lExt: string;

const
  //cDir = '/home/martin/devel/mercados/chart/';
  cDir = '../quotes/';

begin
  // Try to find regular files matching Unit1.d* in the current dir
  if FindFirst(cDir + '*.csv', faAnyFile, searchResult) = 0 then
  begin
    repeat
      // excepciones
      if UpperCase(ASym) = 'YPFD' then ASym := 'YPF';
      if pos('.ADR', UpperCase(ASym)) = 0 then
        lExt := '.BA.CSV'
      else
        lExt := '.CSV';

      if UpperCase(ASym) + lExt = UpperCase(searchResult.Name) then
      begin
        Write(searchResult.Name);
        lStr := TStringList.Create;
        lLine := TStringList.Create;;
        lLine.Delimiter := ',';
        lData := Format('%s,%s,%s,%s,%s,0', [ADate, AOpen, AHigh, ALow, AClose]);
        try
          lStr.LoadFromFile(cDir + searchResult.Name);
          lIndex := -1;
          for I := 1 to lStr.Count - 1 do
          begin
            lLine.DelimitedText := lStr[I];
            if lLine[0] = ADate then
            begin
              lIndex := I;
              break;
            end;
          end;
          if lIndex = -1 then
          begin
            lStr.Insert(1, lData);
          end
          else
            lStr[lIndex] := lData;
          lStr.SaveToFile(cDir + searchResult.Name);
        finally
          lLine.Free;
          lStr.Free;
        end;
      end;
    until FindNext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);
  end;
end;

procedure TMyAction.AddToRT(ADate, ASym, AOpen, AHigh, ALow, AClose, ATime: string);
var
  lMySqlConn: TMySQL55Connection;
  lTransaction: TSQLTransaction;
  lQuery: TSQLQuery;
begin
  if Trim(ATime) = '' then
    exit;
  if Trim(ADate) = '' then
    exit;

  lMySqlConn := TMySQL55Connection.Create(nil);
  lTransaction := TSQLTransaction.Create(nil);
  lQuery := TSQLQuery.Create(nil);
  try
    lMySqlConn.DatabaseName:= 'cecilia2_quotes';
    lMySqlConn.UserName:= 'cecilia2';
    lMySqlConn.Password:= 'qQ5Qo5c0h3';
    lMySqlConn.HostName:= 'localhost';
    lMySqlConn.Transaction := lTransaction;
    lTransaction.StartTransaction;
    lQuery.DataBase := lMySqlConn;
    lQuery.SQL.Text:= 'insert into realtime(date, time, symbol, last) ' +
      'values(:date, :time, :symbol, :last)';
    lQuery.ParamByName('date').AsString:= ADate;
    lQuery.ParamByName('time').AsString:= ATime;
    lQuery.ParamByName('symbol').AsString:= ASym;
    lQuery.ParamByName('last').AsString:= AClose;
    try
      lQuery.ExecSQL;
    except
      // no hacer nada!
    end;
    lTransaction.Commit;
  finally
    lQuery.Free;;
    lTransaction.Free;;
    lMySqlConn.Free;
  end;
end;

function TMyAction.Procesar(AString: TStringList): string;
var
  I: Integer;
  lParsing: Boolean;
  lList: TOHLCList;
  lOHLC: TOHLC;
  lStr: string;
  lDate: string;

begin
  Result := '';
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
    if lParsing then
    begin
      lParsing := lOHLC.AddString(AString[I]);
    end;
  end;

  lStr := Copy(AString.Text, Pos('Actualizaci', AString.Text) + 22, 19);
  Result := Result + 'Actualizacion:' + lStr + #13#10;
  lDate := Copy(lStr, 1, 10);
  for lOHLC in lList do
  begin
    try
    if lOHLC.Sym = '' then
      continue
    else
    Result := Result + Format('"%s";%f;%f;%f;%f;%s;%s', [
      lOHLC.Sym,
      lOHLC.Open,
      lOHLC.High,
      lOHLC.Low,
      lOHLC.Close,
      lDate,
      lOHLC.Time]) + #13#10;

    except
      //Write('error');
    end;
  end;
  lList.Free;
end;

procedure TMyAction.Get;
var
  lStr: TStringList;
  lParam: string;
begin
  lStr := TStringList.Create;
  With TFPHTTPClient.Create(Nil) do
  try
    AllowRedirect := False;
    lParam := HttpRequest.QueryFields.Values['url'];
    lStr.Text := Get('http://' + lParam);
    lStr.Text := Procesar(lStr);
    Parse(lStr);
    Write('Done!');
  finally
    lStr.Free;
    Free;
  end;
end;

initialization
  TMyAction.Register('/quotes');

end.
