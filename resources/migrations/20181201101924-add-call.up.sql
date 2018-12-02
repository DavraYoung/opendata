CREATE TABLE call
(id SERIAL PRIMARY KEY,
 user_id SERIAL REFERENCES users (id) ON DELETE CASCADE,
 crew_id SERIAL REFERENCES crew (id) ON DELETE RESTRICT,
 is_finished BOOLEAN,
 created_at timestamp with time zone DEFAULT NOW());