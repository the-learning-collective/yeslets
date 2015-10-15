-- Revert yeslets:v1views from pg

BEGIN;

DROP VIEW "1".users;
DROP VIEW "1".signals;

COMMIT;
