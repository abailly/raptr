Raptr
=====

> raptr (n.m.): old norse for raft

Raptr is a burgeonning implementation of a [Raft](https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf) cluster, based on
[Kontiki](https://github.com/NicolasT/kontiki),
that aims at providing an out-of-the-box library for building linearizable distributed data storage systems communicating over plain
HTTP(S), in Haskell. 

[![Build Status](https://travis-ci.org/capital-match/raptr.png?branch=master)](https://travis-ci.org/capital-match/raptr)

# Usage

TBD

# Requirements

* Raptr should be easily embeddable within a host system and impose minimal requirements in terms of infrastructure and dependency,
  hence the use of plain HTTP as transport protocol and opaque `ByteString`s as "commands" to be linearized by the system,
* Raptr should be safe and correct, e.g. live up to the promises of *linearizability* and *resilience* of underlying protocol:
    * Linearizable means that concurrent writes appear to take effect instantaneously at some point between start and end of write
      request by a client,
    * Resilient means *committed* data must survive irrecoverable crash of a minority of servers in a cluster,

# Contributing

Pull requests are welcomed.
