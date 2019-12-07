create table collaborators (
  id text primary key unique,
  article text not null references articles(id) on delete cascade,
  collaborator text not null references users(id) on delete cascade,
  creation_date timestamp with time zone not null
);

create unique index article_collaborator_pair_unique on collaborators(article, collaborator);
