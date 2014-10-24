unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  sysutils,
  sqldb,
  mysql55conn;

type

  { TMyAction }

  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TMyAction }

procedure TMyAction.Get;
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
      '  rt.date, '+
      '  ( '+
      '    select last '+
      '    from realtime rt2 '+
      '    where '+
      '      rt2.symbol = rt.symbol and '+
      '      rt2.date = rt.date and '+
      '      hour(rt2.time) = hour(rt.time) and '+
      '      minute(rt2.time) >= CASE WHEN MINUTE(rt.time) < 30 THEN ''00'' ELSE ''30'' END and '+
      '      minute(rt2.time) < CASE WHEN MINUTE(rt.time) < 30 THEN ''30'' ELSE ''59'' END '+
      '	order by rt2.time '+
      '    limit 1 '+
      '  ) as "Open", '+
      '  max(rt.last) as High, '+
      '  min(rt.last) as Low, '+
      '  ( '+
      '    select last '+
      '    from realtime rt3 '+
      '    where '+
      '      rt3.symbol = rt.symbol and '+
      '      rt3.date = rt.date and '+
      '      hour(rt3.time) = hour(rt.time) and '+
      '      minute(rt3.time) >= CASE WHEN MINUTE(rt.time) < 30 THEN ''00'' ELSE ''30'' END and '+
      '      minute(rt3.time) < CASE WHEN MINUTE(rt.time) < 30 THEN ''30'' ELSE ''59'' END '+
      '	order by rt3.time desc '+
      '    limit 1 '+
      '  )as "Close", '+
      '  CONCAT(CAST(HOUR(rt.time) AS CHAR(2)), '':'', (CASE WHEN MINUTE(rt.time) < 30 THEN ''00'' ELSE ''30'' END)) as Time '+
      'from realtime rt '+
      'where '+
      '  rt.symbol = :sym '+
      'group by '+
      '  1,6 '+
      'order by rt.date desc, rt.time desc';

    lQuery.ParamByName('sym').AsString:= lSymbol;
    lQuery.Open;

    while not lQuery.EOF do
    begin
      Write(Format('%s,%s,%s,%s,%s,%s', [
        lQuery.Fields[0].AsString,
        lQuery.Fields[1].AsString,
        lQuery.Fields[2].AsString,
        lQuery.Fields[3].AsString,
        lQuery.Fields[4].AsString,
        lQuery.Fields[5].AsString,
        lQuery.Fields[6].AsString
      ]));
      lQuery.Next;
    end;
  finally
    lQuery.Free;;
    lTransaction.Free;;
    lMySqlConn.Free;
  end;
end;

initialization
  TMyAction.Register('*');

end.
