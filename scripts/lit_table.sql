set joined_subquery_requires_alias=0

set output_format_pretty_max_rows=100

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

-- ** inserting yearly data 

CREATE TABLE dots (country_name String, country_code Int16, indicator_name String, indicator_code String, counterpart_country_name String, counterpart_country_code String, year Int16, value Float32, status String) engine=MergeTree() partition BY year ORDER BY tuple()


insert into dots
select country_name, country_code, indicator_name, indicator_code, counterpart_country_name, counterpart_country_code, toInt16(time_period) as year, value, status from dots_prep
WHERE LENGTH(time_period)=4



SELECT COUNT(*) FROM dots

SELECT * from dots where country_code=134

SELECT * FROM dots LIMIT 10

or (country_code='138' and counterpart_country_code='134')

SELECT indicator_code, indicator_name, cnt FROM (
SELECT tpl.1 AS indicator_name, tpl.2 AS indicator_code FROM (
  SELECT DISTINCT(indicator_name, indicator_code) AS tpl FROM dots GROUP BY indicator_name, indicator_code)
) JOIN (SELECT indicator_code, COUNT(indicator_code) AS cnt FROM dots GROUP BY indicator_code
) USING indicator_code
									       
									       
select country_code, counterpart_country_code, concat(toString(country_code), '-', toString(counterpart_country_code)), year, indicator_code, value from dots where year='2000'

SELECT country_code, counterpart_country_code, concat(toString(country_code), '-', toString(counterpart_country_code)) AS link1, concat(toString(counterpart_country_code), '-', toString(country_code)) AS link2, year, indicator_code, value FROM dots WHERE year='2000'

SELECT parent, COUNT(parent) AS cnt FROM (
SELECT child, parent FROM (
  SELECT child FROM bc WHERE parent='private-museum')
  JOIN (SELECT child, parent FROM bc)
  USING child
  ) GROUP BY parent ORDER BY cnt DESC



SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['state-support', 'subsidies', 'finance', 'policy', 'donation', 'tax-deduction', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child

SELECT DISTINCT(child) FROM (
SELECT child, parent FROM (
  SELECT child, parent FROM bc
   WHERE parent IN ['inequality', 'cls_papers', 'cls_toread']) JOIN 
   (SELECT DISTINCT(child) FROM bc WHERE parent='private-museum') USING child)




'incentives', 'philanthropy',

show tables

SELECT * FROM wdi WHERE varx='popul'

SELECT COUNT(DISTINCT(variable)) FROM wdi WHERE varx='popul'

SELECT variable, COUNT(variable) FROM wdi WHERE varx='popul' GROUP BY variable



SELECT age, COUNT(age) FROM wdi WHERE varx='popul' AND percentile='p0p100' AND year>1985 GROUP BY age
-- seems as if all age groups are equally well covered
-- but only for popul generally 

DESCRIBE wdi

SELECT DISTINCT(variable), varx FROM wdi WHERE ilike(varx, '%weal%' )

SELECT country, value FROM wdi WHERE variable='mnweal999i' AND year > 1985


