# Advanced R Solutions

This repository contains the source code for our [Advanced R Solutions](https://advanced-r-solutions.rbind.io/) project.

We attempt to provide worked solutions for the exercises provided in the book [Advanced R (2nd Ed)](https://adv-r.hadley.nz/).

We are currently in the process of working on the last chapters and more difficult exercises.

Please feel invited to contribute or say hi!

[@malte_grosser](https://twitter.com/malte_grosser), [@henningsway](https://twitter.com/henningsway) and [@hadleywickham](https://twitter.com/hadleywickham)

## CRC Publication
To zip files to for publisher:

```
mkdir crc
cp _book/_main.tex crc
cp -r _bookdown_files/*_files crc
cp -r images crc
cp -r emoji crc
cp krantz.cls crc
cp book.bib crc
rm -r crc/images/*.drawio
find crc/images -name '*.drawio' -delete
rm -r crc/images/cover

zip -r adv-r-source.zip crc
```
