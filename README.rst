Rsync-demo
==========

When promoting OCaml, you can show the usual toy examples of recursive datastructures elegantly matching recursive functions, but if you want to showcase the support for 'programming in the large', you need something larger. 

This little project combines several language features in one small, but non-trivial codebase.

If you want upgrade this to a industrial rsync-like implementation,
you will need to consider this:

- Don't use the rsync algorithm: there are far better solutions to this problem.
  (rsync's trump is that it is easy to understand and code)
  
- I use Marshal module to which is conceptually comfortable, but ill-suited for
  putting ints and strings on a channel.

- IO is done with channels, you probably want to go down to file descriptors.
  If you want to use it in a server, you probably want to move to Async or Lwt.

- the strong hash is an md5 because ocaml provides it. You want something else
  (a better implementation, another hash or both)

- the weak hash is adler32 in it's simplest implementation. 
  You want a better one.

- you want to use OUnit for the tests. 

- (and lots more ...)


Compiling
---------

> ocamlbuild -use-ocamlfind syncer.native

I added a script for coverage (using bisect). 
A possible scenario is

> ./coverage.sh
> ./syncer.byte --test
> bisect-report -I _build -html coverage bisect0001.out
> firefox coverage/index.html

have fun,

Romain 
