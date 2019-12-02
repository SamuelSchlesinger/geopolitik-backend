create table users (
  id text primary key unique,
  username text unique not null,
  password text not null,
  creation_date timestamp with time zone not null
);

create table articles (
  id text primary key unique,
  name text not null,
  owner text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null
);

create table drafts (
  id text primary key unique not null,
  article text not null references articles(id) on delete cascade,
  author text not null referenced users(id) on delete cascade,
  contents text not null,
  creation_date timestamp with time zone not null
);

create table sessions (
  id text primary key unique not null,
  owner text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null,
  data text unique not null
);
