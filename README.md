# Geopolitik

Geopolitik is a website for people to publish their articles on topics
relating to geopolitics. This can include news, culture, sports, economics,
business, geography.

## Installing/Running

To run this on mac, you need to have PostgreSQL running locally on port 5432 with a database
called "geopolitik", and you need to have the PostGIS extension installed, both of which are
available via brew.
```bash
export GEOPOLITIK_LOCATION=<this-folder>
stack run migrate
```
If you want to drop the various tables that this adds to that postgres database, you can run this script
and get rid of them all. 
```bash
stack run drop
```
Finally, to run the server on port 8080, you can run the command:
```bash
stack run geopolitik
```

## Using

Currently, there is no frontend to speak of. That being said, if you have the
server running, you can poke it in a number of ways. One is curl, with a
cookie file. This is well documented on the internet and you can always poke
me here if you are having troubles. The other way is a number of html pages
which serve as basic tests of some of the endpoints. These go in a sequence
from [signup](http://localhost:8080/signup.html).

## Architecture

![Module Structure](mods.png)
