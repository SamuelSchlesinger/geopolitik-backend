create table expertises (
  id text primary key unique not null,
  description text not null references articles(id) on delete cascade,
  creation_date timestamp with time zone not null 
);

create table experts (
  expert text not null references users(id) on delete cascade,
  expertise text not null references expertises(id) on delete cascade,
  creation_date timestamp with time zone not null
);
