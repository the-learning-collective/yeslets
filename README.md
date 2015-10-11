
#Making a Postgres database!

First, install postgres or see if it's installed by typing `psql`. If you're on a Mac, use `brew` to install, not any other wacky way you find on Google!

Create a database and a user for the database. Start `psql`.
```(sql)
    CREATE USER yeslets
    CREATE DATABASE yeslets WITH OWNER = yeslets

Use the schema to make tables like so: `schema > psql yeslets'

Start `psql yeslets`.

Add the pgcrypto extension with `CREATE EXTENSION pgcrypto`.

To add a user: 
```(sql)
    INSERT INTO users (name, email, password, created_at) VALUES
	( 'Ziggy'
	, 'ziggy@example.com'
        , crypt('misterbuttons', gen_salt('md5'))
        , 'now');
```


	
