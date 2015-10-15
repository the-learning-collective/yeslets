-- Revert yeslets:appschema from pg

BEGIN;

DROP SCHEMA yeslets;

COMMIT;
