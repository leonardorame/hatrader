unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dateutils,
  BrookAction,
  fphttpclient,
  sqldb,
  mysql55conn;

type

  { TMyAction }

  TMyAction = class(TBrookAction)
  private
    procedure AddToDaily(ADate: TDate; ASymbol, AOpen, AHigh, ALow, AClose, AVol: string);
    procedure Parse(AData: TStringList; ASymbol: string);
    procedure GetQuotes(ASymbol, AURL: string);
  public
    procedure Get; override;
  end;

implementation

{ TMyAction }

procedure TMyAction.AddToDaily(ADate: TDate; ASymbol, AOpen, AHigh, ALow,
  AClose, AVol: string);
var
  lMySqlConn: TMySQL55Connection;
  lTransaction: TSQLTransaction;
  lQuery: TSQLQuery;
begin
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
    lQuery.SQL.Text:= 'replace into daily(date, symbol, open, high, low, close, volume) ' +
      'values(:date, :symbol, :open, :high, :low, :close, :volume)';
    lQuery.ParamByName('date').AsDate:= ADate;
    lQuery.ParamByName('symbol').AsString:= ASymbol;
    lQuery.ParamByName('open').AsString:= AOpen;
    lQuery.ParamByName('high').AsString:= AHigh;
    lQuery.ParamByName('low').AsString:= ALow;
    lQuery.ParamByName('close').AsString:= AClose;
    lQuery.ParamByName('volume').AsString:= AVol;
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

procedure TMyAction.Parse(AData: TStringList; ASymbol: string);
var
  lDate: TDate;
  lOpen: string;
  lHigh: string;
  lLow: string;
  lClose: string;
  lVol: string;
  I: Integer;
  lLine: TStringList;
begin
  Write('Adding ' + ASymbol + '...');
  DefaultFormatSettings.ShortDateFormat:='D-MMM-Y';
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
  lLine := TStringList.Create;
  lLine.Delimiter:=',';
  try
    for I := 1 to AData.Count - 1 do
    begin
      lLine.DelimitedText:= AData[I];
      lDate := ScanDateTime('D-MMM-Y', lLine[0]);
      lOpen := lLine[1];
      lHigh := lLine[2];
      lLow := lLine[3];
      lClose := lLine[4];
      lVol := lLine[5];
      AddToDaily(lDate, ASymbol, lOpen, lHigh, lLow, lClose, lVol);
    end;
  finally
    lLine.Free;
  end;
end;

procedure TMyAction.GetQuotes(ASymbol, AURL: string);
var
  lStr: TStringList;

begin
  // http://www.google.com/finance/historical?q=NASDAQ%3AGGAL&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv
  lStr := TStringList.Create;
  With TFPHTTPClient.Create(Nil) do
  try
    AllowRedirect := False;
    lStr.Text := Get(AURL);
    Parse(lStr, ASymbol);
  finally
    Free;
    lStr.Free;
  end;
end;

procedure TMyAction.Get;
begin
  // obtiene datos historicos de Google Finance
  Write('Start');
  GetQuotes('MERV', 'http://www.google.com/finance/historical?q=BCBA%3AIM25&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('COME', 'http://www.google.com/finance/historical?q=BCBA%3ACOME&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('EDN', 'http://www.google.com/finance/historical?q=BCBA%3AEDN&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('GGAL', 'http://www.google.com/finance/historical?q=BCBA%3AGGAL&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('INDU', 'http://www.google.com/finance/historical?q=BCBA%3AINDU&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('APBR', 'http://www.google.com/finance/historical?q=BCBA%3AAPBR&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('ERAR', 'http://www.google.com/finance/historical?q=BCBA%3AERAR&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('TS', 'http://www.google.com/finance/historical?q=BCBA%3ATS&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('YPFD', 'http://www.google.com/finance/historical?q=BCBA%3AYPFD&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('PAM', 'http://www.google.com/finance/historical?q=BCBA%3APAMP&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('MIRG', 'http://www.google.com/finance/historical?q=BCBA%3AMIRG&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('ALUA', 'http://www.google.com/finance/historical?q=BCBA%3AALUA&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('GGAL.ADR', 'http://www.google.com/finance/historical?q=NASDAQ%3AGGAL&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  GetQuotes('PBR.ADR', 'http://www.google.com/finance/historical?q=NYSE%3APBR&ei=1OUeVOi8GIrKsQetp4GoCg&output=csv');
  Write('Done!');
end;

initialization
  TMyAction.Register('*');

end.
