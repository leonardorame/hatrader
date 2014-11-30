create table badlar(
  fecha date not null, 
  valor numeric not null, 
  primary key(fecha)
  );

CREATE OR REPLACE FUNCTION blackscholes(callputflag character, s numeric, x numeric, t numeric, r numeric, v numeric)
  RETURNS numeric AS
$BODY$
declare
  d1 numeric;
  d2 numeric;
  PI numeric := 3.141592653589793238462643;
  RESULT numeric;
BEGIN
  RESULT := 0;
  d1 = (ln(S / X) + (r + (v * v / 2.0)) * T) / (v * Sqrt(T));
  d2 = d1 - v * Sqrt(t);
  if (callputflag = 'C') then
    result = S * CND(d1) - X * Exp(-r * T) * CND(d2);
  elsif (callputflag = 'V') then
    result = X * Exp(-r * T) * CND(-d2) - S * CND(-d1);
  end if;

RETURN RESULT;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

alter table opciones_realtime add column hv_to_opex numeric;
alter table opciones_realtime add column iv_bid numeric;
alter table opciones_realtime add column iv_ask numeric;
alter table opciones_realtime add column iv numeric;

drop type opcion_type;
create type opcion_type as (idsimbolo integer, idsubyacente integer, subyacente varchar(10), hv numeric, iv_bid numeric, iv_ask numeric, iv numeric);

DROP FUNCTION fill_opciones_rt(character varying, integer, double precision, integer, double precision, double precision, timestamp without time zone, integer);

CREATE OR REPLACE FUNCTION fill_opciones_rt(_simbolo character varying, _bid_qty integer, _bid double precision, _ask_qty integer, _ask double precision, _last double precision, _hora timestamp without time zone, _mensajenro integer)
  RETURNS void AS
$BODY$
declare 
  _vencimiento date;
  _strike numeric;
  _tipo char;
  _mes varchar(2);
  _data opcion_type%ROWTYPE;
  _badlar numeric;
begin
  _badlar = (select valor from badlar order by fecha desc limit 1);
  for _data in (
    select 
      op.idsimbolo, 
      op.idsubyacente, 
      ea.simbolo as subyacente,
      case
        when (op.vencimiento - now()::date) < 40 then hv(ea.simbolo, (op.vencimiento - now()::date)) 
        else hv(ea.simbolo, 40)
      end as hv,
      case
        when _bid > 0 then iv(op.tipo::varchar, ad.close, op.strike, _badlar, (op.vencimiento - now()::date)/365.0, _bid::numeric)
        else 0
      end as iv_bid, 
      case
        when _ask > 0 then iv(op.tipo::varchar, ad.close, op.strike, _badlar, (op.vencimiento - now()::date)/365.0, _ask::numeric)
        else 0
      end as iv_ask, 
      case
        when _last > 0 then iv(op.tipo::varchar, ad.close, op.strike, _badlar, (op.vencimiento - now()::date)/365.0, _last::numeric)
        else 0
      end as iv
    from especies_opciones op 
    join especies_acciones ea on ea.idsimbolo=op.idsubyacente
    join acciones_daily ad on ad.idsimbolo=ea.idsimbolo
    where 
      op.simbolo=_simbolo and 
      op.vencimiento >= now()::date and
      ad.fecha = now()::date) 
    limit 1 
  loop
    if(_data.idsimbolo is not null) then
      insert into opciones_realtime(idsimbolo, bid, bid_qty, ask, ask_qty, last, datetime, mensajenro, hv_al_opex, iv_bid, iv_ask, iv) 
      values(_data.idsimbolo, _bid, _bid_qty, _ask, _ask_qty, _last, _hora, _mensajenro, 
        round(_data.hv, 3), round(_data.iv_bid, 3), round(_data.iv_ask, 3), round(_data.iv, 3));
    end if;
  end loop;
end
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;