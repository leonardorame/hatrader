unit dm;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, Classes, SysUtils, pqconnection, sqldb;

type

  { TBrookDataModule1 }

  TBrookDataModule1 = class(TBrookDataModule)
    PQConnection1: TPQConnection;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    function GetJsonFromQuery(AQuery: string): string;
  end;

var
  BrookDataModule1: TBrookDataModule1;

implementation

{$R *.lfm}

{ TBrookDataModule1 }

procedure TBrookDataModule1.DataModuleCreate(Sender: TObject);
begin
  PQConnection1.HostName:= '127.0.0.1';
  PQConnection1.DatabaseName:= 'trading';
  PQConnection1.UserName:= 'postgres';
  PQConnection1.Password:= 'postgres';
  PQConnection1.Connected:= True;
end;

function TBrookDataModule1.GetJsonFromQuery(AQuery: string): string;
var
  lQuery: TSQLQuery;
begin
  lQuery := TSQLQuery.Create(nil);
  try
    lQuery.DataBase := PQConnection1;
    lQuery.ParseSQL:= false;
    lQuery.SQL.Text:= 'select json_agg(resultset)::text as json from('
      + AQuery
      + ') as resultset';
    lQuery.Open;
    Result := lQuery.Fields[0].AsString;
  finally
    lQuery.Free;
  end;

end;

end.

