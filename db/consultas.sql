select json_agg(resultset) as json from(with summary as(
  select el.simbolo, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime, ad.open, ad.high, ad.low, ad.vol, ad.prev_close, lrt.mensajenro,
  row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm  
  from acciones_realtime lrt
  join especies_acciones el on el.idsimbolo=lrt.idsimbolo
  join acciones_daily ad on ad.idsimbolo=lrt.idsimbolo
  where lrt.datetime::date = (select fecha from acciones_daily order by fecha desc limit 1)
  and lrt.datetime::date = ad.fecha
  order by el.simbolo
)
select  s.* from summary s
where s.sm = 1
order by s.simbolo) as resultset;

with summary as(
   select 
   el.idsimbolo, el.simbolo, ea.idsimbolo as idunderlying, ea.simbolo as underlying, ad.close,
   el.vencimiento-ad.fecha as datedif, 
   case 
     when (ad.close < el.strike * 0.95) then 'OTM'
     when (ad.close >= el.strike * 0.95) and (ad.close <= el.strike * 1.05) then 'ATM'
     when (ad.close > el.strike * 1.05) then 'ITM'
   end as clasificacion,
   el.tipo, el.strike, el.vencimiento, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime,
   row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm
   from opciones_realtime lrt
   join especies_opciones el on el.idsimbolo=lrt.idsimbolo
   join acciones_daily ad on el.idsubyacente=ad.idsimbolo
   join especies_acciones ea on ea.idsimbolo=el.idsubyacente
   where lrt.datetime::date = '2014-11-27' and ad.fecha=lrt.datetime::date and ad.close <> 0
   order by el.simbolo, el.tipo, datetime desc, mensajenro desc
),
hist_vol as(
   select 
     idsimbolo,
     hv(underlying, datedif) as hv
   from summary
   group by idsimbolo, underlying, datedif
)
select s.*, hv.hv from summary s
join hist_vol hv on hv.idsimbolo = s.idsimbolo
where s.sm = 1
order by simbolo, tipo, strike


select hv('GGAL', 40);
select Round(iv('C', 6.48, 7.12, 0.2051, (22.0/365.0), 0.2), 3);
select Round(blackscholes('C', 19.25, 21.00, 0.2051, (22.0/365.0), 0.40), 3)


