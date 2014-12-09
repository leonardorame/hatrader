with foo as (
  select 
    fecha,
    close,
    coalesce(close - lag(close, -1) over (order by fecha desc), 0) as "Price Change",
    coalesce(ln(close / lag(close, -1) over (order by fecha desc)), 0) as "Log Change"
  from acciones_daily
  where idsimbolo = 65
  order by fecha desc limit 25
),
std_dev as (
  select 
    fecha,
    stddev(foo."Log Change") over (rows between 19 preceding and current row) as stddev
    from foo
)
select
  foo.fecha,
  foo.close as "Close",
  round(foo."Price Change", 2) as "Price Change",
  round(foo."Log Change", 4) as "Log Change",
  round(std_dev.stddev, 4) as "StdDev Log Change",
  round(foo.close*std_dev.stddev, 3) as "1 StdDev",
  round(lag(foo.close*std_dev.stddev, -1) over (), 3) as "Prev 1 StdDev",
  round(foo."Price Change" / lag(foo.close*std_dev.stddev, -1) over (), 2) as "Current Spike"
from std_dev, foo
where std_dev.fecha = foo.fecha
order by foo.fecha;