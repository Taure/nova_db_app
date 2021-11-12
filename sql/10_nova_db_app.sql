CREATE DATABASE nova_db_app;

\c nova_db_app;

CREATE TABLE message
(
    id UUID PRIMARY KEY,
    payload VARCHAR
);