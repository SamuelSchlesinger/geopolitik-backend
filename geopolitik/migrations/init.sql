create table executed_migrations (
  file_name text primary key unique not null,
  creation_date timestamp with time zone not null
);

create table users (
  id text primary key unique,
  username text unique not null,
  password text not null,
  creation_date timestamp with time zone not null
);

create table articles (
  id text primary key unique,
  name text not null,
  owner text unique not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null
);

create table drafts (
  id text primary key unique not null,
  article text not null references articles(id) on delete cascade,
  author text not null references users(id) on delete cascade,
  contents text not null,
  creation_date timestamp with time zone not null
);

create table sessions (
  id text primary key unique not null,
  owner text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null,
  token text unique not null
);
