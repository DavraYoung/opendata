CREATE TABLE crew
(id SERIAL PRIMARY KEY,
 name VARCHAR(30),
 on_call BOOLEAN,
 loc_msg_id integer,
 chat_id integer,
 created_at timestamp with time zone DEFAULT NOW());
