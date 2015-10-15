-- Deploy yeslets:yeslets to pg

BEGIN;

CREATE TABLE users
(
  id serial NOT NULL PRIMARY KEY,
  name text NOT NULL,
  email text NOT NULL,
  password text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP );

CREATE TABLE signals (
  id serial NOT NULL primary key,
  user_id integer NOT NULL REFERENCES users(id),
  action text NOT NULL,
  topic text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP );

CREATE TABLE yesletses (
  id serial NOT NULL PRIMARY KEY,
  signal_id integer NOT NULL REFERENCES signals(id),
  user_id integer NOT NULL REFERENCES users(id),
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP );

COMMIT;
