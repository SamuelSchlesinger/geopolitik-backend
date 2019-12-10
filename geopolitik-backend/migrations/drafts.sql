create table drafts (
  id text primary key unique not null,
  article text not null references articles(id) on delete cascade,
  author text not null references users(id) on delete cascade,
  contents text not null,
  creation_date timestamp with time zone not null
);
