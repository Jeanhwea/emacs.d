SELECT
  T.*
FROM (
  SELECT
    T2.*,
    ROWNUM AS ROWIDX
  FROM (
    SELECT
      T1.*
    FROM
      '&tablename' T1
    WHERE
      1 = 1
    ORDER BY
      MYID) T2) T
WHERE
  T.ROWIDX > 10
  AND T.ROWIDX < 20
ORDER BY
  T.ROWIDX;
