# Research & experimental code for Humming Consensus

For research related to Humming Consensus: errata &amp; corrections,
source code for future work, etc.

## 1. Papers and presentations

*April 2016* Reviewed paper for the
[Workshop on Principles and Practice of Consistency for Distributed Data 2016](http://www2.ucsc.edu/papoc-2016/index.shtml),
a satellite workshop of
[EuroSys 2016](http://eurosys16.doc.ic.ac.uk):
"Coordinating Distributed System Configuration Changes with Humming Consensus"
* [paper (PDF format)](http://snookles.com/scott/publications/papoc2016-slf.pdf)

*March 2016* Presentation at the
[Erlang Factory San Francisco 2016 conference](http://www.erlang-factory.com/sfbay2016):
"Build big with tiny tools: immutability, checksums, and CRDTs"
* [presentation summary](http://www.erlang-factory.com/sfbay2016/scott-lystig-fritchie)
* [slides (PDF format)](http://www.erlang-factory.com/static/upload/media/1459269795976037scottlystigfritchiebuildbigwithtinytoolsimmutabilitychecksumsandcrdts.pdf)
* [video](https://www.youtube.com/watch?v=mqOpgoUuAXE)

*November 2015* Presentation at the
[RICON 2015 conference](http://ricon.io):
"Managing Chain Replication Metadata with Humming Consensus".
* [slides (PDF format)](http://ricon.io/speakers/slides/Scott_Fritchie_Ricon_2015.pdf)
* [video](https://www.youtube.com/watch?v=yR5kHL1bu1Q)

*October 2015* Design document: "Chain Replication metadata management in Machi, an immutable file store: introducing the 'humming consensus' algorithm"
* [paper (PDF format)](https://github.com/basho/machi/blob/master/doc/high-level-chain-mgr.pdf)
* [Machi documentation overview](https://github.com/basho/machi/tree/master/doc)

## 2. Humming Consensus implementations

At the moment, there is only a single implementation of Humming
Consensus: embedded inside of
[Machi, a distributed blob/file store](https://github.com/basho/machi).  Additional implementations
will be listed here, when they exist.

### 2.1 Machi

* Machi's source repository:
  [at GitHub](https://github.com/basho/machi)
* The [Erlang](http://www.erlang.org/) source module that implements
  Humming Consensus:
  [machi_chain_manager1.erl](https://github.com/basho/machi/blob/master/src/machi_chain_manager1.erl)
* Hands-on tutorial to experiment with Machi's HC implementation:
  [doc/humming-consensus-demo.md](https://github.com/basho/machi/blob/master/doc/humming-consensus-demo.md)

Please see the `README` files and the documentation in the source
repository's `doc` directory for additional information to help orient
yourself.

The source code for 
[machi_chain_manager1.erl](https://github.com/basho/machi/blob/master/src/machi_chain_manager1.erl)
is not very beautiful.  The Git commit log shows that it has undergone
a huge amount of change.  Writing it was a learning experience.  In
the beginning, I (Scott) didn't know if the technique would work.
I think I've learned a lesson in why grad student research code
usually isn't pretty: the research & discovery process itself isn't
always pretty.  `^_^`

Having said that, this implementation is also fairly well tested using
QuickCheck and other property-based testing techniques.  The
implementation of Humming Consensus is entangled with Chain
Replication and some other parts of Machi ... but as far as I have
been able to tell, this implementation does indeed work as I'd hoped
that it would.  It can thrash & flap for longer than is necessary in A
100% Perfect World, but the code isn't 100% perfect yet.  In my
opinion, it is good enough to put into production and see what
happens.

### 2.2 HC-lite

This source repo will attempt to create simplified version of Humming
Consensus, as described in the PaPOC paper mentioned above.

Development will take place on Git branches with an `hc-lite` prefix.
If you are viewing this document via GitHub's Web site, then you are
probably viewing the `master` branch of this document.  If you wish to
spy on the latest HC-lite development, please look at the Git branches
with `hc-lite` in the name.

## 3. Contacting Scott

Contact me via my Twitter
handle,
`＠sl``fri tchie` or by email at
` ``nospam ＠``snookles``。com``  ` ... please
remove any extra backtick
quotes and extra spaces.
