select
  t.*
from (
  select
    t1.myid,
    rownum as rowidx
  from
    '&tablename' t1
  where
    1 = 1) t;
