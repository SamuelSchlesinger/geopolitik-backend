# Geopolitik

Geopolitik is a website for people to publish their articles on topics
relating to geopolitics. This can include news, culture, sports, economics,
business, geography. This repository contains an incomplete implementation 
of the backend for this, though there is an Elm project in here that is waiting
for any awesome contributor to come use the APIs I wrote and make a front end.

## Installing/Running

To run this on any unix machine, you need to have PostgreSQL 
running locally on port 5432 with a database called "geopolitik", and you need to have the 
PostGIS extension installed, both of which are available via brew.
To begin, run the commands:
```bash
export GEOPOLITIK_LOCATION=<this-folder>
stack run migrate
```
I have the former in my bash profile.
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

## Module Structure

I try to keep this photo updated with the current dependency graph of the
modules in this project, so you can have an idea of how to explore it. That
being said, I won't be describing the architecture here, as it is so prone
to change and I don't want to have to update documentation no one is presently
reading every time I change the architecture.

![Module Structure](mods.png)
