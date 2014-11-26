with summary as(
  select el.simbolo, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime, lrt.mensajenro,
  row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm  
  from acciones_realtime lrt
  join especies_acciones el on el.idsimbolo=lrt.idsimbolo
  --where lrt.datetime::date = current_date::date
  order by datetime desc, mensajenro desc 
)
select  s.* from summary s
where s.sm = 1
order by s.simbolo

with summary as(
   select el.simbolo, lrt.bid_qty,lrt.bid, lrt.ask, lrt.ask_qty, lrt.last, datetime,
   row_number() over(partition by el.simbolo order by datetime desc, mensajenro desc) as sm
   from opciones_realtime lrt
   join especies_opciones el on el.idsimbolo=lrt.idsimbolo
   where  el.simbolo in ('PBRC82.4DI', 'GFGC21.0DI', 'GFGC22.0DI', 'GFGC23.0DI', 'GFGC24.0DI') 
   order by el.simbolo, datetime desc, mensajenro desc
)
select  s.* from summary s
where s.sm = 1


