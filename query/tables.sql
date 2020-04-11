select
  '&lpre' || t.tabname || '&fsep' || substr(t.tabcmt, 1, 40) as rowdata
from (
  select
    t1.table_name as tabname, --> Table Name
    replace(replace(t2.comments, chr(13), ''), chr(10), '&lsep') as tabcmt --> Table Comments
  from
    user_tables t1,
    user_tab_comments t2
  where
    t1.table_name = t2.table_name
  order by
    tabname) t;
