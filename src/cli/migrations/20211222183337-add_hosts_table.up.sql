CREATE TABLE hosts (
       id INTEGER PRIMARY KEY,
       name CHARACTER VARYING(255),
       addr CHARACTER VARYING(17)
);
--;;
CREATE UNIQUE INDEX hosts_name_idx ON hosts(name);
