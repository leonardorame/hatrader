unit v1.acciones.fecha;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  dm;

type

  { TAcciones }

  TAcciones = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TAcciones }

procedure TAcciones.Get;
var
  lSql: string;
  lFecha: string;
begin
  lFecha := Variable['fecha'];
  lSql :=
  'with summary as( ' +
  '  select el.simbolo, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, datetime, ad.open, ad.high, ad.low,  lrt.last, ad.vol, lrt.mensajenro, ' +
  '  row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm ' +
  '  from acciones_realtime lrt ' +
  '  join especies_acciones el on el.idsimbolo=lrt.idsimbolo ' +
  '  join acciones_daily ad on ad.idsimbolo=lrt.idsimbolo ' +
  '  where lrt.datetime::date = ''' + lFecha +''' ' +
  '  and lrt.datetime::date = ad.fecha ' +
  '  order by el.simbolo ' +
  ') ' +
  'select  s.simbolo, s.bid_qty, s.bid, s.ask, s.ask_qty, s.datetime, s.open, s.high, s.low, s.last, s.vol from summary s ' +
  'where s.sm = 1 ' +
  'order by s.simbolo';
  HttpResponse.ContentType := 'application/json';
  Write(BrookDataModule1.GetJsonFromQuery(lSql));
end;

initialization
  TAcciones.Register('/v1/acciones/:fecha');

end.
