-- 1bii

SELECT DISTINCT country.name AS country
FROM   country
       JOIN ( SELECT e1.country AS country
              FROM   encompasses AS e1
                     CROSS JOIN encompasses AS e2
              WHERE  e1.country = e2.country 
              AND    e1.continent <> e2.continent 
            ) AS code
       ON code.country = country.code 


-- 1C

SELECT country.name AS country,
       COUNT(iata_code) FILTER(WHERE elevation IS NULL) AS unknown,
       COUNT(iata_code) FILTER(WHERE elevation < 200) AS low,
       COUNT(iata_code) FILTER(WHERE elevation >= 200) AS high
FROM   country
       LEFT JOIN airport
       ON airport.country = country.code 
GROUP BY country.name


-- 1d

SELECT country.name AS country,
       city.name AS city,
       population,
       RANK() OVER( PARTITION BY country.code
                    ORDER BY population DESC NULLS LAST) as rank
FROM   country
       JOIN city 
       ON country.code = city.country;

-- 1E

SELECT *
FROM city
EXCEPT
SELECT *
FROM city 
WHERE elevation = elevation;

SELECT *
FROM city 
WHERE NOT EXISTS (SELECT *
                  FROM city AS same_city
                  WHERE same_city.elevation=city.elevation)

SELECT * 
FROM city
WHERE COALESCE(elevation, 42) <> COALESCE(elevation, 67);

SELECT * 
FROM city 
GROUP BY name, country, province, population, elevation 
HAVING COUNT(elevation) = 0;