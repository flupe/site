---
title: jibniz
subtitle: A javascript implementation of the IBNIZ VM
year: "2017"
labels:
  repo: flupe/jibniz
  license: MIT
---

**jibniz** is a javascript implementation of the IBNIZ virtual machine.
IBNIZ is an esoteric stack-based programming language created by _, in which every
instruction is one character long. The code is intended to be ran for each
pixel on a 256x256 screen, at every frame --- making it possible to produce
animations and interactive demos.

## Limitations

- Currently, this implementation **does not support audio**.
- WebGL is used for color conversion only. Ideally I would like to get rid of it
  and find an exact integer only formula.
- At some point I wanted to compile the entire IBNIZ programs to WASM, rather
  than build an interpreter. The problem is that IBNIZ programs are
  *unstructured*.  the `J` instruction allows you to jump *anywhere* in the
  program. This makes WASM a poor target for the language.
