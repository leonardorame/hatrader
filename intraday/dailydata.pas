unit dailydata;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  sysutils,
  dateutils,
  sqldb,
  mysql55conn;

type

  { TDaily }

  TDaily = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TDaily }

procedure TDaily.Get;
var
  lMySqlConn: TMySQL55Connection;
  lTransaction: TSQLTransaction;
  lQuery: TSQLQuery;
  lSymbol: string;
begin
  lSymbol := HttpRequest.QueryFields.Values['sym'];
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
    lQuery.SQL.Text:=
      'select '+
      '  d.date, d.open, d.high, d.low, d.close, d.volume ' +
      'from daily d ' +
      'where ' +
      '  d.symbol = :sym ' +
      'order by date(d.date) desc ' +
      'limit 50';

    lQuery.ParamByName('sym').AsString:= lSymbol;
    lQuery.Open;

    while not lQuery.EOF do
    begin
      Write(Format('%s,%s,%s,%s,%s,%s', [
        FormatDateTime('YYYY-MM-DD', lQuery.Fields[0].AsDateTime),
        lQuery.Fields[1].AsString,
        lQuery.Fields[2].AsString,
        lQuery.Fields[3].AsString,
        lQuery.Fields[4].AsString,
        lQuery.Fields[5].AsString
      ]) + #13);
      lQuery.Next;
    end;
  finally
    lQuery.Free;;
    lTransaction.Free;;
    lMySqlConn.Free;
  end;
end;

initialization
  TDaily.Register('/daily');

end.
