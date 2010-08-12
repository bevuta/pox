# Pox

Pox is a client/server based application that manages tasks. It can be
used by multiple users. Features include:

* A purely textual interface (based on a subset of Markdown)
* Task delegation
* Modular notification system
* Powerful task grouping and filtering capabilities to generate ad-hoc to-do lists

## Implementation

Pox is implemented in [Chicken Scheme](http://www.call-cc.org/) and
known to work with version 4.5.0 so far. It can easily be compiled and
installed using the `chicken-install` command. Note that you need the
[hato mail suite](http://code.google.com/p/hato/) which can't be
obtained from the standard egg repository. You'll also need a
PostgreSQL server and prepare a database with the included
`schema.sql` as there is no automatic setup routine for pox, yet. To
start up the server, you still need a checkout of the source
repository at the moment. Copy the `init.scm-dist` file and name it
`init.scm`. Modify it to suit your needs (i.e. set up the database
connection) and then execute `./run.scm production`. Pox should now
listen on port 7040, ready to take requests.

Since there is no user management facility in pox, yet, you'll have to
insert your users into the database manually for now. Once you did
that, you can try to access the pox web interface through your
browser, enter a user name and start dabbling!

(more in-depth docs will follow)

## License

See LICENSE for licensing information.
