unit alldata;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  sysutils,
  dateutils,
  sqldb,
  mysql55conn;

type

  { TAll }

  TAll = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TAll }

procedure TAll.Get;
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
    lQuery.SQL.Text:=
      'select '+
      ' d.date, d.symbol, d.open, d.high, d.low, d.close, d.volume, ' +
      ' (select prev.close from daily prev where prev.symbol=d.symbol order by prev.date desc limit 1,1) as prev ' +
      'from daily d ' +
      'join ' +
      '  (select d2.symbol, max(d2.date) as date ' +
      '  from daily d2 ' +
      '  group by d2.symbol) as max using (symbol) ' +
      'where max.date = d.date';

    lQuery.Open;

    while not lQuery.EOF do
    begin
      Write(Format('%s,%s,%s,%s,%s,%s,%s,%s', [
        FormatDateTime('YYYY-MM-DD', lQuery.Fields[0].AsDateTime),
        lQuery.Fields[1].AsString,
        lQuery.Fields[2].AsString,
        lQuery.Fields[3].AsString,
        lQuery.Fields[4].AsString,
        lQuery.Fields[5].AsString,
        lQuery.Fields[6].AsString,
        lQuery.Fields[7].AsString
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
  TAll.Register('/all');

end.
