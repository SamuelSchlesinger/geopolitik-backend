create table articles (
  id text primary key unique,
  name text not null,
  owner text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null
);

create unique index name_owner_pair_unique on articles(name, owner);
