select
  t3.* (
    select
      t2.*,
      rownum as rowidx
    from (
      select
        t1.*,
      from
        '&tablename' t1
      where
        1 = 1
      order by
        myid) t2) t3
where
  t3.rowidx > 10
  and t3.rowidx < 20;
