CREATE FUNCTION order_total(id int) RETURNS int AS $$ SELECT 1 $$ LANGUAGE sql;
CREATE TABLE orders (id int);
SELECT order_total(1) FROM orders;
