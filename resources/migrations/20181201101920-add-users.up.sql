CREATE TABLE users
(id SERIAL PRIMARY KEY,
 telegram_id integer UNIQUE,
 location FLOAT[],
 phone_number VARCHAR(15),
 created_at timestamp with time zone DEFAULT NOW());
