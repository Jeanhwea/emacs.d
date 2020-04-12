select
  t2.*
from (
  select
    myid,
    rownum as rowidx
  from
    '&tablename' t1
  where
    1 = 1) t2;
