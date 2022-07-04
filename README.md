# go-kstem

Quick & dirty golang port of Krovetz Stemmer aka kstem used in elasticsearch.

## Description

A mix of the original C/CPP version and the dictionary from the lucid version, 
to be 100% compatible with elastic kstem results.

The code was not optimized for Go and is basically just a ton of copy & paste to make it work in Go.

## LICENSE

[WTFPL](http://www.wtfpl.net/)

## Acknowledgments

Inspiration, code snippets, etc.
* [kstem (original C version)](http://lexicalresearch.com/software.html)
* [kstem (C version in github)](https://github.com/diazf/kstem/)
* [KrovetzStemmer (c++ version)](https://github.com/pisa-engine/KrovetzStemmer)
* [lucid-kstemmer (java version used in elasticsearch)](https://github.com/zapient/lucid-kstemmer)
