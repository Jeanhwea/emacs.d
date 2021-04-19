SELECT
  T.*
FROM (
  SELECT
    T1.MYID,
    ROWNUM AS ROWIDX
  FROM
    '&tablename' T1
  WHERE
    1 = 1) T;
