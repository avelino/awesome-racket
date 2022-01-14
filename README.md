# [Awesome Racket](https://awesome-racket.com)

<a href="https://awesome-racket.com/"><img align="right" src="https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/Racket-logo.svg/240px-Racket-logo.svg.png" alt="awesome-racket" title="awesome-racket" /></a>

A curated list of **Awesome Racket**, libraries and software. Inspired by [awesome-go](https://github.com/avelino/awesome-go).

[![Build Status](https://github.com/avelino/awesome-racket/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/avelino/awesome-racket/actions/workflows/ci.yml?query=branch%3Amain)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/sindresorhus/awesome)

<a href="https://www.producthunt.com/posts/awesome-racket?utm_source=badge-featured&utm_medium=badge&utm_souce=badge-awesome-racket" target="_blank"><img src="https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=326738&theme=light" alt="awesome-racket - A curated list of awesome Racket language | Product Hunt" style="width: 250px; height: 54px;" width="250" height="54" /></a>

[Check the complete package list](https://pkgs.racket-lang.org/)

[The Racket repository](https://github.com/racket/racket)

### Contents

- [Awesome Racket](#awesome-racket)
		- [Contents](#contents)
	- [Audio and Music](#audio-and-music)
	- [Compilers](#compilers)
	- [Data Structures](#data-structures)
	- [Database Drivers](#database-drivers)
	- [Emulators](#emulators)
	- [GUI Development](#gui-development)
	- [Game Development](#game-development)
	- [Games](#games)
	- [Machine Learning](#machine-learning)
	- [Macros](#macros)
	- [Messaging](#messaging)
	- [Third-party APIs](#third-party-apis)
	- [Testing](#testing)
	- [Video](#video)
	- [Web Frameworks](#web-frameworks)

## Audio and Music

_Libraries for manipulating audio._

- [3s](https://github.com/jeapostrophe/3s) - Positional sound and mixing for lux and other applications.
- [RSound](https://github.com/jbclements/RSound) - A framework for manipulating and playing sounds using the portaudio library. Runs on Windows, Mac OS X, and linux.
- [libopenal-racket](https://github.com/lehitoskin/libopenal-racket) - Racket wrapper to the OpenAL library first written by gcr in 2012.
- [midi-readwrite](https://github.com/jbclements/midi-readwrite) - Library to read .mid files in racket
- [openal](https://github.com/jeapostrophe/openal) - FFI for OpenAL.
- [osc](https://github.com/jbclements/osc) - Open Sound Control data definitions.
- [portaudio](https://github.com/jbclements/portaudio) - Bindings for portaudio, a cross-platform audio library.
- [rsc3](https://github.com/quakehead/rsc3) - SuperCollider client ported to Racket.
- [rtmidi](https://github.com/jbclements/rtmidi) - Provides racket bindings for the RtMidi library, thus enabling racket programs to send and receive MIDI events.
- [sonic-pi](https://github.com/jbclements/sonic-pi) - For now, this package starts scsynth just like sonic pi does, and can make a few sounds.
- [taglib](https://github.com/takikawa/taglib-racket) - Bindings to the taglib C library, which provides simple access to audio file metadata.
- [wavelet-transform-haar-1d](https://github.com/jbclements/wavelet-transform-haar-1d) - A library to perform forward and reverse 1-d Haar Wavelet transforms.

## Compilers

_Tools for compiling Racket to other languages._

- [abstract-compilation](https://github.com/philnguyen/abstract-compilation) - DSL reducing boiler plates for doing abstract compilation.
- [disassemble](https://github.com/samth/disassemble) - Disassembler for Racket.
- [minipascal](https://github.com/soegaard/minipascal) - MiniPascal as a Racket language.
- [racketscript](https://github.com/racketscript/racketscript) - A lightweight Racket to JavaScript compiler with some batteries included.
- [urlang](https://github.com/soegaard/urlang) - Write JavaScript with Racket syntax. Bonus: Use Racket to define macros for JavaScript constructs.
- [wasm-adventure](https://github.com/euhmeuh/wasm-adventure) - A WebAssembly DSL.
- [whalesong-tools](https://github.com/vishesh/drracket-whalesong) - DrRacket tool for compiling with Whalesong.
- [wracket](https://github.com/sschauss/wracket) - Lisp-like language to WebAssembly build with racket.
- [zordoz](https://github.com/bennn/zordoz) - Explorer for .zo bytecode files.
- [lens](https://github.com/jackfirth/lens) - A Racket package for creating and composing pure functional lenses.

## Data Structures

_Generic datastructures and algorithms._

- [algebraic](https://github.com/dedbox/racket-algebraic) - Algebraic structures for untyped Racket.
- [dssl](https://github.com/tov/dssl) - Data Structures Student Language: an extension of ASL for easier imperative programming.
- [dssl2](https://github.com/tov/dssl2) - A language for data structures students.
- [gls](https://github.com/Kalimehtar/gls) - Generic Little (Object, Type, Anything, etc) System - multiple dispatch on types.
- [graph](https://github.com/stchang/graph) - Generic graph library.
- [opt](https://gitlab.com/RayRacine/opt) - Optional and Either data type utilities. Provides util function for Typed Racket's Option type as well as defines an Either type.
- [phc-adt](https://github.com/jsmaniac/phc-adt) - Algebraic Data Types for Typed/Racket, with features tailored to compiler writing. The data types do not have to be declared before they are used, like prefab structs and symbols. Behind the scenes, this library remembers all the data types in a file, and uses it to implicitly pre-declare them. Mostly stable, although some things may change a bit in the future.
- [quad-tree](https://github.com/dented42/racket-quad-tree) - A fairly simple quad-tree implementation. Nothing terribly fancy. Currently rather unstable.
- [try](https://gitlab.com/RayRacine/try) - A Typed Racket Try datatype and routines for computations that throw exceptions.

## Database Drivers

_Libraries for connecting and operating databases._

- [binary-class-dbf](https://github.com/Kalimehtar/binary-class-dbf) - Interface to *.dbf files (dBase, Foxpro, etc).
- [db](https://github.com/racket/db) - Database connectivity (main distribution).
- [dbm](https://github.com/jeapostrophe/dbm) - An interface to UNIX dbm files using a libdbm FFI.
- [deta](https://github.com/Bogdanp/deta) - A functional database mapper.
- [fra](https://github.com/jeapostrophe/fra) - Purely functional implementation of relational algebra.
- [mongodb](https://github.com/jeapostrophe/mongodb) - A native Racket interface to MongoDB & BSON.
- [rackdis](https://github.com/eu90h/rackdis) - Redis bindings.
- [racquel](https://github.com/brown131/racquel) - Racquel is an object/relational mapper for Racket.
- [redis](https://github.com/stchang/redis) - A redis client for Racket.
- [sql](https://github.com/rmculpepper/sql) - an S-expression notation for SQL.
- [sqlite-table](https://github.com/jbclements/sqlite-table) - A quick way to create and query sqlite tables. Basically a simplified wrapper for a subset of the db library.

## Emulators

_Racket programs emulating other computers and architectures_

- [6502](https://github.com/soegaard/6502) - An emulator/assembler/disassembler for 6502.
- [virtual-mpu](https://github.com/euhmeuh/virtual-mpu) - Universal Emulator & Assembler for Old Microprocessors.

## GUI Development

_Libraries for cross platform GUI development_

- [MrEd Designer](https://github.com/Metaxal/MrEd-Designer) - MrEd Designer is WYSIWYG program to create GUI applications for Racket. (code generator).
- [The Racket Graphical Interface Toolkit](https://docs.racket-lang.org/gui/index.html) - Racket GUI library (core distribution).
- [gui-easy](https://github.com/Bogdanp/racket-gui-easy) - A declarative API on top of `racket/gui`.
- [gui-widget-mixins](https://pkgs.racket-lang.org/package/gui-widget-mixins) - Tool tips, cue text and validation for text-field% GUI widgets in Racket.
- [map-widget](https://pkgs.racket-lang.org/package/map-widget) - A Racket GUI Widget to display maps based on OpenStreetMap tiles
More at [packages tagged `GUI`](https://pkgd.racket-lang.org/pkgn/search?q=&tags=gui).

## Game Development

_Awesome game development libraries._

- [apse](https://github.com/jeapostrophe/apse) - A Pixel Sprite Editor.
- [game-engine](https://github.com/srfoster/game-engine) - Scratchpad for racket game stuff.
- [get-bonus](https://github.com/get-bonus/get-bonus) - An experimental video game development environment.
- [mode-lambda](https://github.com/jeapostrophe/mode-lambda) - Sprite-based 2D graphics engine.
- [pict3d](https://github.com/jeapostrophe/pict3d) - A 3D engine with a purely functional API.
- [towers](https://github.com/Metaxal/towers) - Towers is an original 2-player board game.
- [vr-lang](https://github.com/thoughtstem/vr-lang) - Racket Lang for Virtual Reality (Aframe).

## Games

_Games written in Racket_

- [Racket games](https://github.com/racket/games) - games in main distribution.
- [r-cade](https://github.com/massung/r-cade) - Retro Game Engine for Racket.
- [space-invaders](https://github.com/soegaard/space-invaders) - A Racket remake of Mary Rose Cook's JavaScript version of Space Invaders.
- [web-sweeper](https://github.com/Halfwake/web-sweeper) - Stateless Server Side Mine Sweeper.

## Machine Learning

_Libraries for Machine Learning._

- [DeepRacket](https://github.com/charlescearl/DeepRacket) - A simple starting point for doing deep learning in Racket.
- [layer](https://github.com/cloudkj/layer) - Neural network inference the Unix way.
- [racket-knn](https://github.com/asbaker/racket-knn) - K Nearest Neighbors, KNN, is a lazy, supervised machine learning algorithm. This is an implementation in scheme using racket.
- [racket-ml](https://github.com/danking/racket-ml) - A collection of things I found useful for doing Machine Learning problem sets.
- [rml-core](https://github.com/johnstonskj/rml-core) - This Package is part of an expected set of packages implementing machine learning capabilities for Racket. The core of this package is the management of 'datasets', these datasets are assumed to be for training and testing of machine learning capabilities.
- [rml-decisiontrees](https://github.com/johnstonskj/rml-decisiontrees) - This Package is part of a set of packages implementing machine learning capabilities for Racket. This particular package implements support for classification of individuals using decision trees.
- [rml-knn](https://github.com/johnstonskj/rml-knn) - This Package is part of a set of packages implementing machine learning capabilities for Racket. This particular package implements the K-Nearest Neighbor approach for classification.
- [tesseract](https://github.com/lasfter/tesseracket) - Bindings for Google's Tesseract-OCR.

## Macros

_Awesome macros that make your life easier_

- [anaphoric](https://github.com/jsmaniac/anaphoric) - Anaphoric macros for Racket.
- [threading](https://github.com/lexi-lambda/threading) - Macros to flatten nested function calls.

## Messaging

_Libraries that implement messaging systems._

- [neuron-lib](https://github.com/dedbox/racket-neuron) - Implementation of neuron.
- [profj](https://github.com/mflatt/profj) - Kathy Gray's ProfessorJ language ported to modern DrRacket.
- [stomp](https://github.com/tonyg/racket-stomp) - STOMP messaging protocol codec and client.
- [zeromq-r](https://github.com/rmculpepper/racket-zeromq) - Bindings for ZeroMQ.
- [zmq](https://github.com/mordae/racket-zmq) - Minimal Racket ZeroMQ Bindings.

## Third-party APIs

_Libraries for accessing third party APIs._

- [aws](https://github.com/greghendershott/aws) - Amazon Web Services including S3, SDB, SES, SNS, SQS, CloudWatch, Glacier, Dynamo, and Route 53.
- [aws-cloudformation-deploy](https://github.com/cjdev/aws-cloudformation-deploy) AWS Cloudformation deployment scripting library.
- [comm-panel](https://github.com/thoughtstem/comm-panel) - Racket GUI widget for sending, receiving, listening, and broadcasting strings over AWS SQS.
- [google](https://github.com/tonyg/racket-google) - Google APIs (Drive, Plus, etc) for Racket.
- [racket-ovh](https://github.com/euhmeuh/racket-ovh) - Unofficial Racket wrapper for OVH API.
- [recaptcha](https://github.com/LiberalArtist/recaptcha) - Utilities for using reCAPTCHA with the web-server/formlets API.

## Testing

_Libraries for testing codebases and generating test data_

- [RackUnit](https://docs.racket-lang.org/rackunit/) - RackUnit is a unit-testing framework for Racket. It is designed to handle the needs of all Racket programmers, from novices to experts.
- [al2-test-runner](https://github.com/alex-hhh/al2-test-runner) - alternate rackunit test runner.
- [cover](https://github.com/florence/cover) - a code coverage tool for racket.
- [test-more](https://github.com/dstorrs/racket-test-more) - A Racket version of Perl's Test::More library.

## Video

_Racket tools for working with videos_

- [video](https://github.com/videolang/video) - Video is a DSL for describing videos.

## Web Frameworks

_Full stack web frameworks._

- [HoLy](https://github.com/nihirash/holy) - HoLy is simple a HTTP-server Library for Racket.
- [Rackt](https://github.com/rackt-org/rackt) - An ultrasmall (~70 loc) React wrapper written in RacketScript.
- [Routy](https://github.com/Junker/routy) - Routy is a lightweight high performance HTTP request router for Racket.
- [Spin](https://github.com/dmac/spin) - Write RESTful web apps in Racket.
- [frog](https://github.com/greghendershott/frog) - Frog is a static blog generator implemented in Racket, targeting Bootstrap and able to use Pygments.
- [koyo](https://github.com/Bogdanp/koyo) - A web development toolkit for Racket.
- [polkadot](https://github.com/2-3/polkadot) - A lightweight personal wiki in Racket.
- [racket-request](https://github.com/jackfirth/racket-request) - Package for simplifying HTTP requests and writing integration tests of REST-ful APIs in Racket.
- [riposte](https://github.com/vicampo/riposte) - Scripting language for testing JSON-based HTTP APIs.
- [vela](https://github.com/nuty/vela) - Simple web framework to build RESTful app in Racket.
- [web-galaxy](https://github.com/euhmeuh/web-galaxy) - A minimalist web framework for the Racket web-server.
- [web-server/servlet](http://docs.racket-lang.org/web-server/) - Running Web Servlets describes how to run the servlets you’ve written.
