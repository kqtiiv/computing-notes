-- Q1 returns (country,capital,population)
SELECT   name AS country, 
         capital, 
         population 
FROM     country
WHERE    population >= 100000000
ORDER BY population DESC
;

-- Q2 returns (continent,country)
SELECT   continent.name AS continent, 
         country.name AS country
FROM     encompasses 
         JOIN country
         ON   country.code = encompasses.country
         JOIN continent
         ON   continent.name = encompasses.continent 
ORDER BY continent,
         country
;

-- Q3 returns (organization_name,country_name)
SELECT   organization.name AS organization_name, 
         country.name AS country_name
FROM     is_member
         JOIN country 
         ON country.code = is_member.country
         JOIN organization
         ON organization.abbreviation = is_member.organization
WHERE    type = 'member'
ORDER BY organization_name,
         country_name
;

-- Q4 returns (name,type)
SELECT name, 'Country' AS type 
FROM country
UNION
SELECT name, 'Province' AS type 
FROM province
UNION 
SELECT name, 'City' AS type 
FROM city 
ORDER BY name, type 
;

-- Q5 returns (country,population)
SELECT name AS country, 
       population
FROM   country
WHERE  code NOT IN 
            (SELECT country1
             FROM borders
             UNION
             SELECT country2
             FROM borders)
ORDER BY population DESC,
         country 
;

-- Q6 returns (country,no_neighbours,border_length)
SELECT country.name AS country, 
       COUNT(country2) AS no_neighbours,
       SUM(length) AS border_length
FROM   borders 
       JOIN country 
       ON country.code = borders.country1 
       OR country.code = borders.country2
GROUP BY country 
ORDER BY country 
;

-- Q7 returns (organization,no_members,population)
SELECT organization.name AS organization_name,
       COUNT(is_member.country) AS no_members,
       SUM(country.population) AS population 
FROM   is_member
       JOIN organization
       ON organization.abbreviation = is_member.organization
       JOIN country 
       ON country.code = is_member.country
WHERE  type = 'member'
GROUP BY organization_name
HAVING   COUNT(is_member.country) >= 20
ORDER BY organization_name
;

-- Q8 returns (name,member,observer)
SELECT country.name AS name,
       COUNT(type) FILTER(WHERE type = 'member') AS member,
       COUNT(type) FILTER(WHERE type = 'observer') AS observer
FROM   is_member
       JOIN organization
       ON organization.abbreviation = is_member.organization
       RIGHT JOIN country 
       ON country.code = is_member.country
GROUP BY country.name
ORDER BY country.name  
;

-- Q9 returns (country)
SELECT DISTINCT country 
FROM is_member AS countries
WHERE NOT EXISTS 
      (SELECT organization
       FROM is_member
       WHERE country = 'KP'
       AND organization NOT IN 
                        (SELECT organization
                         FROM is_member AS organizations
                         WHERE organizations.country = countries.country))
ORDER BY country 
;

-- Q10 returns (country_name,city_name,population,rank,cum_pc)
SELECT country.name AS country_name,
       city.name AS city_name,
       city.population AS population,
       RANK() OVER(PARTITION BY country.name
                   ORDER BY city.population DESC NULLS LAST) AS rank,
       ROUND(100*SUM(CAST(city.population AS DECIMAL(10, 2))) OVER
            (PARTITION BY country.name
             ORDER BY city.population DESC NULLS LAST, city.name
             ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) /
             SUM(city.population) OVER(PARTITION BY country.name), 1) 
       AS cum_pc
FROM country 
     JOIN city
     ON city.country = country.code
ORDER BY country_name,
         rank,
         city_name
;
