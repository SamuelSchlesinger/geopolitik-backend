create table users (
  id text primary key unique,
  username text unique not null,
  password text not null,
  creation_date timestamp with time zone not null
);
