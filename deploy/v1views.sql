-- Deploy yeslets:v1views to pg

BEGIN;

CREATE OR REPLACE VIEW "1".users AS
SELECT name, email FROM users;

CREATE OR REPLACE view "1".signals AS 
SELECT users.name,
       action,
       topic
FROM signals 
LEFT JOIN users ON signals.user_id = users.id;

COMMIT;
