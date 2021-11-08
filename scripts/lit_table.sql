set joined_subquery_requires_alias=0

set output_format_pretty_max_rows=30

show tables

-- SELECT * FROM bc WHERE parent='interna'

-- SELECT * FROM bc WHERE child='DeCastro_2018_inhotim'

-- SELECT parent, COUNT(parent) AS cnt FROM (
--   SELECT child, parent FROM (
--     SELECT DISTINCT(child) AS child FROM bc WHERE parent IN ['ecology', 'legitimation', 'competition', 'founding', 'contagion', 'diffusion', 'density']
--   ) JOIN (SELECT parent, child FROM bc WHERE parent NOT IN ['cls_papers', 'cls_toread', 'sbcls_B', 'sbcls_A', 'sbcls_C', 'cls_orgform']) USING child) GROUP BY parent ORDER BY cnt DESC

  

  
  select count(distinct(concat(country_name, '-', counterpart_country_name))) from dots_prep where time_period='2020' limit 10

SELECT COUNT(DISTINCT(CONCAT(country_name, '-', counterpart_country_name))) FROM dots_prep WHERE time_period='1985' 


SELECT DISTINCT(CONCAT(country_name, '-', counterpart_country_name)) AS pair FROM dots_prep WHERE time_period='1985'

SELECT DISTINCT(CONCAT(country_name, '-', counterpart_country_name)) AS pair FROM dots_prep WHERE time_period='2020'





SELECT pair, ctr_1985, ctr_2020
FROM (
  SELECT DISTINCT(CONCAT(country_name, '-', counterpart_country_name)) AS pair,
		 notEmpty('asd')+1 AS ctr_1985
    FROM dots_prep
   WHERE time_period='1985')
   LEFT JOIN (
     SELECT DISTINCT(CONCAT(country_name, '-', counterpart_country_name)) AS pair,
		    notEmpty('asd')+2 AS ctr_2020
       FROM dots_prep
      WHERE time_period='2020')
      USING pair
      

SELECT NULL 

SELECT tpl.1 AS country_name, tpl.2 AS country_code FROM (
SELECT DISTINCT(country_name, country_code) AS tpl FROM dots_prep GROUP BY country_name, country_code)


SELECT DISTINCT(CONCAT(country_name, '-', counterpart_country_name)) AS pair, notEmpty('asd')+1 AS ctr_1985 FROM dots_prep WHERE time_period='1985'


SELECT tpl.1 AS ccd, tpl.2 AS ctrccd FROM (SELECT  Distinct(country_code, counterpart_country_code) AS tpl from dots_prep where time_period = '2020' group by country_code, counterpart_country_code)


SELECT notEmpty('asd'), country_name FROM dots_prep

SELECT DISTINCT(country_name) from dots_prep

-- **

insert into dots
select country_name, country_code, indicator_name, indicator_code, counterpart_country_name, counterpart_country_code, toInt16(time_period) as year, value, status from dots_prep
WHERE LENGTH(time_period)=4


-- CREATE TABLE dots (country_name String, country_code Int8, indicator_name String, indicator_code String, counterpart_country_name String, counterpart_country_code String, year Int16, value Float32, status String) engine=MergeTree() partition BY year ORDER BY tuple()

SELECT COUNT(*) FROM dots
