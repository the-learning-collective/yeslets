-- Deploy yeslets:yeslets to pg

BEGIN;

CREATE TABLE users
(
  id serial NOT NULL PRIMARY KEY,
  name text NOT NULL,
  email text NOT NULL,
  password text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
 );

CREATE TABLE signals (
  id serial NOT NULL primary key,
  user text NOT NULL,
  action text NOT NULL,
  topic text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT user_fkey FOREIGN KEY (user)
    REFERENCES user (name) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);

CREATE TABLE yesletses (
  id serial NOT NULL PRIMARY KEY,
  signal_id integer NOT NULL REFERENCES signals(id),
  user text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT user_fkey FOREIGN KEY (user)
    REFERENCES user (name) MATCH SIMPLE
    ON UPDATE NO ACTION ON DELETE NO ACTION
);

COMMIT;
