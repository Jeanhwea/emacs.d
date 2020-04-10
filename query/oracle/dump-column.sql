select
  t.ispk || ',' || t.isuniq || ',' || t.isnul || ',' || t.colname || ',' ||
    t.coltype || ',' || t.collen || ',' || t.colpcs || ',' || t.colid
from (
  select
    (
      select
        'p'
      from
        user_cons_columns t2,
        user_constraints t3
      where
        t3.constraint_name = t2.constraint_name
        and t1.column_name = t2.column_name
        and t1.table_name = t2.table_name
        and lower(t3.constraint_type) = 'p'
        and rownum <= 1) as ispk, --> col1
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
          count(1) = 1) as isuniq, --> col2
      decode(lower(t1.nullable), 'n', '*', '') as isnul, --> col3
      t1.column_name as colname, --> col4
      t1.data_type as coltype, --> col5
      t1.data_length as collen, --> col6
      t1.data_precision as colpcs, --> col7
      (
        select
          replace(replace(t6.comments, chr(13), ''), chr(10), '\n')
          from user_col_comments t6
        where
          t6.column_name = t1.column_name
          and t6.table_name = t1.table_name
          and rownum <= 1) as colcmt, --> col8
      t1.column_id as colid --> col9
    from
      user_tab_columns t1
    where
      lower(t1.table_name) = lower('&tablename')) t;
