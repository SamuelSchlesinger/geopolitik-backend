create table locations (
  id text primary key unique,
  name text not null,
  description text unique not null references articles(id) on delete cascade,
  spot geography(point, 4326) not null,
  creation_date timestamp with time zone not null
);
