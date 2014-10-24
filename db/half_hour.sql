select
  rt.date,
  (
    select last
    from realtime rt2
    where
      rt2.symbol = rt.symbol and
      rt2.date = rt.date and
      hour(rt2.time) = hour(rt.time) and
      minute(rt2.time) >= CASE WHEN MINUTE(rt.time) < 30 THEN '00' ELSE '30' END and
      minute(rt2.time) < CASE WHEN MINUTE(rt.time) < 30 THEN '30' ELSE '59' END
	order by rt2.time
    limit 1
  ) as "Open",
  max(rt.last) as High, 
  min(rt.last) as Low,   
  (
    select last
    from realtime rt3
    where
      rt3.symbol = rt.symbol and
      rt3.date = rt.date and
      hour(rt3.time) = hour(rt.time) and
      minute(rt3.time) >= CASE WHEN MINUTE(rt.time) < 30 THEN '00' ELSE '30' END and
      minute(rt3.time) < CASE WHEN MINUTE(rt.time) < 30 THEN '30' ELSE '59' END
	order by rt3.time desc
    limit 1
  )as "Close",
  CONCAT(CAST(HOUR(rt.time) AS CHAR(2)), ':', (CASE WHEN MINUTE(rt.time) < 30 THEN '00' ELSE '30' END)) as Time
from realtime rt
where 
  rt.symbol = 'GGAL' /*and
  rt.date = '2014-10-23' */
group by 
  1,6
order by rt.date desc, rt.time desc