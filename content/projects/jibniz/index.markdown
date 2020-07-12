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

- Currently, this implementation has **no support for audio**. At the time I remember
  the WebAudio API to be very poorly designed, and I did not understand how it was
  implemented in the official IBNIZ VM.
- I use WebGL for color conversion because I never figured out how to actually
  reliably convert YUV to RGB. The original C implementation uses SDL2
  Overlays or something, and I was not able to reverse engineer the conversion.
  I found floating-point formulas, hence the GLSL shader, etc.
- At some point I wanted to compile the entire IBNIZ programs to WASM, rather
  than build an interpreter. The problem is that IBNIZ programs are *unstructured*.
  the `J` instruction allows you to jump *anywhere* in the program.
