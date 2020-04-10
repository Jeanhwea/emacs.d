select
  t.tabname || ',' || t.tabcmt
from (
  select
    t1.table_name as tabname, --> Table Name
    substr(replace(replace(t2.comments, chr(13), ''), chr(10), '_rn_'), 1, 40)
      as tabcmt --> Table Comments
  from
    user_tables t1,
    user_tab_comments t2
  where
    t1.table_name = t2.table_name
  order by
    tabname) t;
