select
  t.*
from (
  select
    t2.*,
    rownum as rowidx
  from (
    select
      t1.*
    from
      '&tablename' t1
    where
      1 = 1
    order by
      myid) t2) t
where
  t.rowidx > 10
  and t.rowidx < 20
order by
  t.rowidx;
