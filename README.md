# Geopolitik

Geopolitik is a website for people to publish their articles on topics
relating to geopolitics. This can include news, culture, sports, economics,
business, geography.

To run this on mac, you need to have PostgreSQL running locally on port 5432 with a database
called "geopolitik", and you need to have the postgis extension installed, both of which are
available via brew.

```bash
export GEOPOLITIK_LOCATION=<this-folder>
stack run migrate
```

What is going on here? Well, we have a simple migration script that simply reads which migrations
you haven't run and runs them. It reads them out of the manifest file, so make sure you
add your migration to that if you add a new one. There is a complimentary script to migrate,
drop, which you can run like
```bash
stack run drop
```
This drops all of the tables that are involved in the geopolitik app.
