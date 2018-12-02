CREATE TABLE dispatcher
(id SERIAL PRIMARY KEY,
 name VARCHAR(30),
 login VARCHAR(10),
 password  VARCHAR(20),
 created_at timestamp with time zone);
