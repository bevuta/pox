DROP TABLE IF EXISTS users;

CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL UNIQUE
);

DROP TABLE IF EXISTS tasks;

CREATE TABLE tasks (
       id SERIAL PRIMARY KEY,
       revision INTEGER NOT NULL DEFAULT 1,
       name TEXT NOT NULL,
       description TEXT,
       creator_id INTEGER NOT NULL REFERENCES users (id),
       updater_id INTEGER REFERENCES users (id),
       assignee_id INTEGER NOT NULL REFERENCES users (id),
       assigner_id INTEGER NOT NULL REFERENCES users (id),
       created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
       updated_at TIMESTAMP WITH TIME ZONE,
       priority INTEGER NOT NULL DEFAULT 0,
       category TEXT,
       done BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE LANGUAGE plpgsql;

CREATE FUNCTION update_timestamp() RETURNS trigger AS $update_timestamp$
    BEGIN
        NEW.updated_at := NOW();
        RETURN NEW;
    END;
$update_timestamp$ LANGUAGE plpgsql;

CREATE TRIGGER update_timestamp BEFORE UPDATE ON tasks FOR EACH ROW
EXECUTE PROCEDURE update_timestamp();

CREATE TABLE notification_handlers (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL UNIQUE
);

CREATE TABLE user_notifications (
       user_id INTEGER NOT NULL REFERENCES users (id),       
       handler_id INTEGER NOT NULL REFERENCES notification_handlers (id),
       params TEXT
);