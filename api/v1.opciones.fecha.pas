unit v1.opciones.fecha;

{$mode objfpc}{$H+}

interface

uses
  BrookAction,
  dm;

type

  { TOpciones }

  TOpciones = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TOpciones }

procedure TOpciones.Get;
var
  lSql: string;
  lFecha: string;
  lAccion: string;
begin
  lFecha := Variable['fecha'];
  lAccion := Variable['accion'];
  if lAccion <> '' then
    lSql :=
      '  with summary as( ' +
      '     select el.simbolo, ad.close as und_last, ' +
      '     case ' +
      '       when is_atm(ad.idsimbolo, el.strike, ad.close) then ''ATM'' ' +
      '       when is_otm(ad.idsimbolo, el.strike, ad.close) then ''OTM'' ' +
      '       when is_itm(ad.idsimbolo, el.strike, ad.close) then ''ITM''  ' +
      '     end as clasificacion, ' +
      '     el.tipo, el.strike, el.vencimiento, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime, ' +
      '     lrt.iv_bid, lrt.iv_ask, lrt.iv, lrt.hv_al_opex, ' +
      '     row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm ' +
      '     from opciones_realtime lrt ' +
      '     join especies_opciones el on el.idsimbolo=lrt.idsimbolo ' +
      '     join acciones_daily ad on el.idsubyacente=ad.idsimbolo ' +
      '     join especies_acciones ea on ea.idsimbolo = ad.idsimbolo ' +
      '     where lrt.datetime::date = ''' + lFecha + ''' and ad.fecha=lrt.datetime::date ' +
      '     and ea.simbolo = ''' + laccion + '''' +
      '     order by el.simbolo, el.tipo, datetime desc, mensajenro desc ' +
      '  ) ' +
      '  select  s.* from summary s ' +
      '  where s.sm = 1'
  else
  lSql :=
    '  with summary as( ' +
    '     select el.simbolo, ad.close as und_last, ' +
    '     case ' +
    '       when is_atm(ad.idsimbolo, el.strike, ad.close) then ''ATM'' ' +
    '       when is_otm(ad.idsimbolo, el.strike, ad.close) then ''OTM'' ' +
    '       when is_itm(ad.idsimbolo, el.strike, ad.close) then ''ITM''  ' +
    '     end as clasificacion, ' +
    '     el.tipo, el.strike, el.vencimiento, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime, ' +
    '     lrt.iv_bid, lrt.iv_ask, lrt.iv, lrt.hv_al_opex, ' +
    '     row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm ' +
    '     from opciones_realtime lrt ' +
    '     join especies_opciones el on el.idsimbolo=lrt.idsimbolo ' +
    '     join acciones_daily ad on el.idsubyacente=ad.idsimbolo ' +
    '     join especies_acciones ea on ea.idsimbolo = ad.idsimbolo ' +
    '     where lrt.datetime::date = ''' + lFecha + ''' and ad.fecha=lrt.datetime::date ' +
    '     order by el.simbolo, el.tipo, datetime desc, mensajenro desc ' +
    '  ) ' +
    '  select  s.* from summary s ' +
    '  where s.sm = 1';

  HttpResponse.ContentType := 'application/json';
  Write(BrookDataModule1.GetJsonFromQuery(lSql));
end;

initialization
  TOpciones.Register('/v1/opciones/:fecha/:accion');

end.
