-- morally, we want to say entity references tagTable(tag)(id), but alas,
-- postgreSQL does not have dependent types...

create table links (
  id text primary key unique,
  tag text not null,
  draft text not null references drafts(id) on delete cascade,
  entity text not null
);
