create table comments (
  id text primary key unique,
  author text unique not null references users(id) on delete cascade,
  content text not null
);
