---
date: 2014-10-28
title: Fast pagination on PostgreSQL
description: Fast pagination on PostgreSQL
author: Chris Done
tags: postgresql
---

During the implementation of [IRCBrowse](http://ircbrowse.net/) I
discovered that Postgres's built-in offset is not very fast.

Here are the characteristics of my data:

    ircbrowse=> \d event
                                        Table "public.event"
      Column   |           Type           |
    -----------+--------------------------+
     timestamp | timestamp with time zone |
     type      | text                     |
     nick      | text                     |
     text      | text                     |
     network   | integer                  |
     channel   | integer                  |
     id        | bigint                   |
    Indexes:
        "event_unique" UNIQUE CONSTRAINT,
        btree (network, channel, "timestamp", nick, type, text)
        "event_unique_id" UNIQUE CONSTRAINT, btree (id)
        "event_channel_idx" btree (channel)
        "event_nick_idx" btree (nick)
        "event_timestamp_idx" btree ("timestamp")
        "event_type_idx" btree (type)

And the size:

    ircbrowse=> select count(*) from event;
      count
    ----------
     28673917

Channel 1 is the biggest:

    ircbrowse=> select count(*) from event where channel = 1;
      count
    ----------
     19340467

When you're working with data on this scale (large, but not “big
data”), PostgreSQL handles it beautifully. But the speed of
`OFFSET`/`LIMIT` is not great:

    ircbrowse=> explain analyze select * from event where channel = 1
                                order by id offset 500000 limit 30;
    QUERY PLAN
    Limit  (cost=5648.81..5818.28 rows=30 width=85)
           (actual time=0.301..0.309 rows=30 loops=1)
       ->  Index Scan using event_unique_id on event
       (cost=0.00..81914674.39 rows=14501220 width=85)
       (actual time=0.020..0.288 rows=1030 loops=1)
             Filter: (channel = 1)

I think that this index scan is simply too expensive. Notice that I'm ordering by id which has a unique btree index on it. Check out the speed:

    ircbrowse=> select * from event where channel = 1
                order by id offset 1000 limit 30;
    Time: 0.721 ms
    ircbrowse=> select * from event where channel = 1
                order by id offset 500000 limit 30;
    Time: 191.926 ms

You might think less than a second to sift through 500,000 rows of a
28million row table is pretty good, but I think it sucks. It's also
deceptive. Let's increase it to 1,000,000 rows (of 19,000,00):

    ircbrowse=> select * from event where channel = 1
                order by id offset 1000000 limit 30;
    Time: 35022.464 ms

This is getting worse and worse! It's probably linear in its poor
performance.

However, there's a solution. Use an index table. A separate table
which contains foreign keys pointing to this table:

    ircbrowse=> \d event_order_index
    Table "public.event_order_index"
     Column |  Type   | Modifiers
    --------+---------+-----------
     id     | integer | not null
     origin | integer | not null
     idx    | integer | not null
    Indexes:
        "event_order_id_origin" UNIQUE CONSTRAINT, btree (id, origin)
        "event_order_idx" btree (id)
        "event_order_idx_idx" btree (idx)
        "event_order_origin_dx" btree (origin)

Now you can have a pagination index for channel 1:

    ircbrowse=> select * from event_order_index where idx = 1000 limit 1;
     id | origin | idx
    ----+--------+------
      1 |      1 | 1000

(I used idx=1000 for channel 1, 2000 for channel 2, etc. so that I
would have space for other numerical indexes for the same channel.)

Now you can make a very efficient query for the same data as above:

    ircbrowse=> SELECT idx.id,e.timestamp,e.network,e.channel,
    ircbrowse=> e.type,e.nick,e.text FROM event e,
    ircbrowse-> event_order_index idx
    ircbrowse-> WHERE e.id = idx.origin and idx.idx = 1000 and
    ircbrowse=> idx.id > 1000000 and idx.id < 1000030
    ircbrowse-> ORDER BY e.id asc
    ircbrowse-> LIMIT 30;
    Time: 1.001 ms

This is more or less constant time.

And you can see this in action on the site. Takes about 30ms to load
and render the page if I run this on the server:

    $ time curl 'http://ircbrowse.net/browse/haskell?events_page=234'

    real	0m0.031s
    user	0m0.000s
    sys     0m0.004s

Of course, sending a request in your browser will take longer due to
the connection overhead and assets, but generally the goal was for it
to be very snappy. The old ircbrowse.com (by another individual, who
kindly let me have the name) was very slow indeed. You'd see the page
loading the data incrementally from the database.

Anyhoo, thought that was a decent, practical PostgreSQL-specific
optimization regarding pagination. Hope it was worth writing up.
