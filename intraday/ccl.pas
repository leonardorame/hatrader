unit ccl;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  sysutils,
  dateutils,
  sqldb,
  mysql55conn;

type

  { TCCL }

  TCCL = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TCCL }

procedure TCCL.Get;
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
      'SELECT ' + 
      '  d.symbol, d.date, d.close / d2.close * 10 as ccl ' +
      'FROM daily d ' + 
      'join daily d2 on (d2.symbol = ''GGAL.ADR'' and d2.date=d.date) ' + 
      'where d.symbol = ''GGAL'' and d.date = (select max(date) from daily where symbol=d.symbol)';

    lQuery.Open;

    while not lQuery.EOF do
    begin
      Write(Format('%s,%s,%.2f', [
        lQuery.Fields[0].AsString,
        FormatDateTime('YYYY-MM-DD', lQuery.Fields[1].AsDateTime),
        lQuery.Fields[2].AsFloat
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
  TCCL.Register('/ccl');

end.
