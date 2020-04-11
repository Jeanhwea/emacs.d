select
  t.isnul || '&fsep' || t.isuniq || '&fsep' || t.ispk || '&fsep' || t.colname
    || '&fsep' || t.coltype || '&fsep' || t.collen || '&fsep' || t.colpcs ||
    '&fsep' || substr(t.colcmt, 1, 40) as csvrow
from (
  select
    t1.column_id as colid, --> Column Id
    (
      select
        'p' from user_cons_columns t2, user_constraints t3
      where
        t3.constraint_name = t2.constraint_name
        and t1.column_name = t2.column_name
        and t1.table_name = t2.table_name
        and lower(t3.constraint_type) = 'p'
        and rownum <= 1) as ispk, --> Is Primary Key?
    (
      select
        'u' from user_cons_columns t4, user_constraints t5
      where
        t5.constraint_name = t4.constraint_name
        and t1.column_name = t4.column_name
        and t1.table_name = t4.table_name
        and lower(t5.constraint_type) = 'u'
      group by
        t5.constraint_name
      having
        count(1) = 1) as isuniq, --> Is Unique Key?
    decode(lower(t1.nullable), 'n', '*', '') as isnul, --> Nullable?
    t1.column_name as colname, --> Column Name
    t1.data_type as coltype, --> Column Type
    t1.data_length as collen, --> Column Length
    t1.data_precision as colpcs, --> Column Precision
    (
      select
        replace(replace(t6.comments, chr(13), ''), chr(10), '\n')
        from user_col_comments t6
      where
        t6.column_name = t1.column_name
        and t6.table_name = t1.table_name
        and rownum <= 1) as colcmt --> Column Comments
  from
    user_tab_columns t1
  where
    lower(t1.table_name) = lower('&tablename')) t
order by
  t.colid;
