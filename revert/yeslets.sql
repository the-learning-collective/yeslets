-- Revert yeslets:yeslets from pg

BEGIN;

  DROP TABLE users CASCADE;
  DROP TABLE signals CASCADE;
  DROP TABLE yesletses CASCADE;

COMMIT;
