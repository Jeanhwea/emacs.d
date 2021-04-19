SELECT
  '&lpre' || T.ISPK || '&fsep' || T.ISUNIQ || '&fsep' || T.ISNUL ||
    '&fsep' || T.COLNAME || '&fsep' || T.COLTYPE || '&fsep' ||
    T.COLLEN || '&fsep' || T.COLPCS || '&fsep' || SUBSTR(T.COLCMT, 1, 40)
    AS ROWDATA
FROM (
  SELECT
    T1.COLUMN_ID AS COLID,
    (
      SELECT
        'p'
      FROM
        USER_CONS_COLUMNS T2,
        USER_CONSTRAINTS T3
      WHERE
        T3.CONSTRAINT_NAME = T2.CONSTRAINT_NAME
        AND T1.COLUMN_NAME = T2.COLUMN_NAME
        AND T1.TABLE_NAME = T2.TABLE_NAME
        AND LOWER(T3.CONSTRAINT_TYPE) = 'p'
        AND ROWNUM <= 1) AS ISPK,
      (
        SELECT
          'u'
        FROM
          USER_CONS_COLUMNS T4,
          USER_CONSTRAINTS T5
        WHERE
          T5.CONSTRAINT_NAME = T4.CONSTRAINT_NAME
          AND T1.COLUMN_NAME = T4.COLUMN_NAME
          AND T1.TABLE_NAME = T4.TABLE_NAME
          AND LOWER(T5.CONSTRAINT_TYPE) = 'u'
        GROUP BY
          T5.CONSTRAINT_NAME
        HAVING
          COUNT(1) = 1) AS ISUNIQ,
        DECODE(LOWER(T1.NULLABLE), 'n', '*', '') AS ISNUL,
        T1.COLUMN_NAME AS COLNAME,
        T1.DATA_TYPE AS COLTYPE,
        T1.DATA_LENGTH AS COLLEN,
        T1.DATA_PRECISION AS COLPCS,
        (
          SELECT
            REPLACE(REPLACE(T6.COMMENTS, CHR(13), ''), CHR(10), '&lsep')
          FROM
            USER_COL_COMMENTS T6
          WHERE
            T6.COLUMN_NAME = T1.COLUMN_NAME
            AND T6.TABLE_NAME = T1.TABLE_NAME
            AND ROWNUM <= 1) AS COLCMT
        FROM
          USER_TAB_COLUMNS T1
        WHERE
          LOWER(T1.TABLE_NAME) = LOWER('&tablename')) T
ORDER BY
  T.COLID;
