drop table tmp_test;
create temp table tmp_test(id serial, value numeric, primary key(id));
insert into tmp_test(value) values(47.58);
insert into tmp_test(value) values(47.78);
insert into tmp_test(value) values(48.09);
insert into tmp_test(value) values(47.52);
insert into tmp_test(value) values(48.47);
insert into tmp_test(value) values(48.38);
insert into tmp_test(value) values(49.30);
insert into tmp_test(value) values(49.61);
insert into tmp_test(value) values(50.03);
insert into tmp_test(value) values(51.65);
insert into tmp_test(value) values(51.65);
insert into tmp_test(value) values(51.57);
insert into tmp_test(value) values(50.60);
insert into tmp_test(value) values(50.45);
insert into tmp_test(value) values(50.83);
insert into tmp_test(value) values(51.08);
insert into tmp_test(value) values(51.26);
insert into tmp_test(value) values(50.89);
insert into tmp_test(value) values(50.51);
insert into tmp_test(value) values(51.42);
insert into tmp_test(value) values(52.09);
insert into tmp_test(value) values(55.83);
insert into tmp_test(value) values(55.79);
insert into tmp_test(value) values(56.20);

with foo as (
  select 
    id,
    value,
    coalesce(value - lag(value, 1) over (order by id), 0) as "Price Change",
    coalesce(lag(value, -1) over (order by id) - value, 0) as "Next Price Change",
    ln(value / lag(value, 1) over (order by id)) as "Log Change"
  from tmp_test
  order by id limit 20 offset 1
),
std_dev as (select round(stddev(foo."Log Change"), 4) as stddev from foo)
select
  foo.id,
  foo.value as "Close",
  round(foo."Price Change", 2) as "Price Change",
  round(foo."Log Change", 3) as "Log Change",
  round(foo."Next Price Change", 2) as "Next Price Change",
  round(std_dev.stddev, 4) as "StdDev Log Change",
  round(foo.value*std_dev.stddev, 3) as "1 StdDev",
  round((foo."Next Price Change" / (foo.value*std_dev.stddev)), 2) as "Current Spike"
from std_dev, foo
order by foo.id desc limit 1
