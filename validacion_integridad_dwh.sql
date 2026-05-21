-- ============================================================
-- VALIDACIÓN DE INTEGRIDAD REFERENCIAL — altadis_dwh
-- TFM: Sistema BI para Altadis | UNIR 2026
-- Ejecutar en BigQuery. Cada bloque devuelve 0 filas si OK.
--
-- CORRECCIONES APLICADAS DURANTE VALIDACIÓN (19 may 2026):
--   1. dim_producto: duplicado Natu122 resuelto (conserva ASL)
--   2. dim_fecha: fechas faltantes de fact_ventas y fact_oos agregadas
--   3. dim_fecha: fechas faltantes de dim_ruta agregadas
--   Nota: dim_ruta usa Route_DAY (no fecha_id) como columna de fecha
--         dim_estanco_cluster termina en 'r' (no 'e')
-- ============================================================


-- ============================================================
-- CORRECCIÓN 1: dim_producto — eliminar duplicado Natu122
-- Natu122 existe en dos formatos (ASL y ATA) en el catálogo,
-- pero las facts no registran formato a nivel de transacción.
-- Se conserva una sola entrada por product_code (ASL primero).
-- Documentar en Sección 7 — Limitaciones del TFM.
-- ============================================================

CREATE OR REPLACE TABLE `altadis_dwh.dim_producto` AS
SELECT DISTINCT product_code, size, format
FROM `altadis_dwh.producto`
QUALIFY ROW_NUMBER() OVER (PARTITION BY product_code ORDER BY format) = 1;


-- ============================================================
-- CORRECCIÓN 2 y 3: dim_fecha — completar fechas faltantes
-- Se reconstruye dim_fecha con todas las fechas únicas
-- presentes en fact_entregas, fact_ventas, fact_oos y dim_ruta.
-- ============================================================

CREATE OR REPLACE TABLE `altadis_dwh.dim_fecha` AS
SELECT DISTINCT fecha_id
FROM (
  SELECT fecha_id FROM `altadis_dwh.fact_entregas`
  UNION DISTINCT
  SELECT fecha_id FROM `altadis_dwh.fact_ventas`
  UNION DISTINCT
  SELECT fecha_id FROM `altadis_dwh.fact_oos`
  UNION DISTINCT
  SELECT Route_DAY AS fecha_id FROM `altadis_dwh.dim_ruta`
)
ORDER BY fecha_id;


-- ============================================================
-- 1. UNICIDAD DE CLAVES PRIMARIAS EN DIMENSIONES
-- Resultado esperado: 0 filas en todos los tests
-- ============================================================

-- 1.1 dim_estanco: Affiliated_Code único
SELECT affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.dim_estanco`
GROUP BY affiliated_code
HAVING COUNT(*) > 1;

-- 1.2 dim_producto: product_code único (tras corrección Natu122)
SELECT product_code, COUNT(*) AS n
FROM `altadis_dwh.dim_producto`
GROUP BY product_code
HAVING COUNT(*) > 1;

-- 1.3 dim_fecha: fecha_id único
SELECT fecha_id, COUNT(*) AS n
FROM `altadis_dwh.dim_fecha`
GROUP BY fecha_id
HAVING COUNT(*) > 1;

-- 1.4 dim_ruta: clave compuesta Route_DAY + affiliated_code única
SELECT Route_DAY, affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.dim_ruta`
GROUP BY Route_DAY, affiliated_code
HAVING COUNT(*) > 1;

-- 1.5 dim_estanco_cluster: affiliated_code único
SELECT affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.dim_estanco_cluster`
GROUP BY affiliated_code
HAVING COUNT(*) > 1;


-- ============================================================
-- 2. INTEGRIDAD REFERENCIAL: FACTS → DIMS
-- Resultado esperado: 0 filas en todos los tests
-- ============================================================

-- 2.1 fact_entregas → dim_estanco
SELECT f.affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.fact_entregas` f
LEFT JOIN `altadis_dwh.dim_estanco` d USING (affiliated_code)
WHERE d.affiliated_code IS NULL
GROUP BY f.affiliated_code;

-- 2.2 fact_entregas → dim_producto
SELECT f.product_code, COUNT(*) AS n
FROM `altadis_dwh.fact_entregas` f
LEFT JOIN `altadis_dwh.dim_producto` d USING (product_code)
WHERE d.product_code IS NULL
GROUP BY f.product_code;

-- 2.3 fact_entregas → dim_fecha
SELECT f.fecha_id, COUNT(*) AS n
FROM `altadis_dwh.fact_entregas` f
LEFT JOIN `altadis_dwh.dim_fecha` d USING (fecha_id)
WHERE d.fecha_id IS NULL
GROUP BY f.fecha_id;

-- 2.4 fact_ventas → dim_estanco
SELECT f.affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.fact_ventas` f
LEFT JOIN `altadis_dwh.dim_estanco` d USING (affiliated_code)
WHERE d.affiliated_code IS NULL
GROUP BY f.affiliated_code;

-- 2.5 fact_ventas → dim_producto
SELECT f.product_code, COUNT(*) AS n
FROM `altadis_dwh.fact_ventas` f
LEFT JOIN `altadis_dwh.dim_producto` d USING (product_code)
WHERE d.product_code IS NULL
GROUP BY f.product_code;

-- 2.6 fact_ventas → dim_fecha
SELECT f.fecha_id, COUNT(*) AS n
FROM `altadis_dwh.fact_ventas` f
LEFT JOIN `altadis_dwh.dim_fecha` d USING (fecha_id)
WHERE d.fecha_id IS NULL
GROUP BY f.fecha_id;

-- 2.7 fact_oos → dim_estanco
SELECT f.affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.fact_oos` f
LEFT JOIN `altadis_dwh.dim_estanco` d USING (affiliated_code)
WHERE d.affiliated_code IS NULL
GROUP BY f.affiliated_code;

-- 2.8 fact_oos → dim_producto
SELECT f.product_code, COUNT(*) AS n
FROM `altadis_dwh.fact_oos` f
LEFT JOIN `altadis_dwh.dim_producto` d USING (product_code)
WHERE d.product_code IS NULL
GROUP BY f.product_code;

-- 2.9 fact_oos → dim_fecha
SELECT f.fecha_id, COUNT(*) AS n
FROM `altadis_dwh.fact_oos` f
LEFT JOIN `altadis_dwh.dim_fecha` d USING (fecha_id)
WHERE d.fecha_id IS NULL
GROUP BY f.fecha_id;


-- ============================================================
-- 3. INTEGRIDAD REFERENCIAL: DIMS ENTRE SÍ
-- Resultado esperado: 0 filas en todos los tests
-- ============================================================

-- 3.1 dim_estanco_cluster → dim_estanco
SELECT c.affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.dim_estanco_cluster` c
LEFT JOIN `altadis_dwh.dim_estanco` d USING (affiliated_code)
WHERE d.affiliated_code IS NULL
GROUP BY c.affiliated_code;

-- 3.2 dim_ruta → dim_estanco
SELECT r.affiliated_code, COUNT(*) AS n
FROM `altadis_dwh.dim_ruta` r
LEFT JOIN `altadis_dwh.dim_estanco` d USING (affiliated_code)
WHERE d.affiliated_code IS NULL
GROUP BY r.affiliated_code;

-- 3.3 dim_ruta → dim_fecha (columna Route_DAY)
SELECT r.Route_DAY, COUNT(*) AS n
FROM `altadis_dwh.dim_ruta` r
LEFT JOIN `altadis_dwh.dim_fecha` d ON r.Route_DAY = d.fecha_id
WHERE d.fecha_id IS NULL
GROUP BY r.Route_DAY;


-- ============================================================
-- 4. VALIDACIONES DE NEGOCIO (informativas)
-- ============================================================

-- 4.1 fact_entregas: no deben existir unidades <= 0
SELECT 'fact_entregas: delivery_uds <= 0' AS test, COUNT(*) AS n
FROM `altadis_dwh.fact_entregas`
WHERE delivery_uds <= 0;

-- 4.2 fact_ventas: no deben existir unidades <= 0
SELECT 'fact_ventas: sales_uds <= 0' AS test, COUNT(*) AS n
FROM `altadis_dwh.fact_ventas`
WHERE sales_uds <= 0;

-- 4.3 Recuento general de filas por tabla
SELECT 'dim_estanco'        AS tabla, COUNT(*) AS filas FROM `altadis_dwh.dim_estanco`        UNION ALL
SELECT 'dim_producto',               COUNT(*)           FROM `altadis_dwh.dim_producto`       UNION ALL
SELECT 'dim_fecha',                  COUNT(*)           FROM `altadis_dwh.dim_fecha`          UNION ALL
SELECT 'dim_ruta',                   COUNT(*)           FROM `altadis_dwh.dim_ruta`           UNION ALL
SELECT 'dim_estanco_cluster',        COUNT(*)           FROM `altadis_dwh.dim_estanco_cluster` UNION ALL
SELECT 'fact_entregas',              COUNT(*)           FROM `altadis_dwh.fact_entregas`      UNION ALL
SELECT 'fact_ventas',                COUNT(*)           FROM `altadis_dwh.fact_ventas`        UNION ALL
SELECT 'fact_oos',                   COUNT(*)           FROM `altadis_dwh.fact_oos`;

-- 4.4 Rango de fechas por tabla
SELECT 'dim_fecha'      AS tabla, MIN(fecha_id)  AS desde, MAX(fecha_id)  AS hasta FROM `altadis_dwh.dim_fecha`      UNION ALL
SELECT 'fact_entregas',            MIN(fecha_id),           MAX(fecha_id)           FROM `altadis_dwh.fact_entregas`  UNION ALL
SELECT 'fact_ventas',              MIN(fecha_id),           MAX(fecha_id)           FROM `altadis_dwh.fact_ventas`    UNION ALL
SELECT 'fact_oos',                 MIN(fecha_id),           MAX(fecha_id)           FROM `altadis_dwh.fact_oos`;

-- ============================================================
-- FIN DEL SCRIPT
-- Bloques 1-3: resultado esperado 0 filas (DWH íntegro)
-- Bloque 4: informativo
-- ============================================================
