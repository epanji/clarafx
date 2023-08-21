# CLARAFX (WIP)


## Examples

_Example file [mysubfx.lisp](Examples/mysubfx.lisp)_

SBCL

```sh
$ sbcl --load Examples/mysubfx.lisp --eval '(sb-ext:quit)'
```

![FX](Examples/mysubfx.gif)


_Example file [mysubfx2.lisp](Examples/mysubfx2.lisp)_

SBCL

```sh
$ sbcl --load Examples/mysubfx2.lisp --eval '(sb-ext:quit)'
```

![FX2](Examples/mysubfx2.gif)


## Use effects from subtitle file

Subtitle file should already have correct karaoke time before using effects.

Make effects from existing file with 3 step:

1. Add effects to script info

```txt
[Script Info]
...
clarafx-1: shaking,default,62,5
clarafx-2: dropping,default,62,7
...
```

2. Choose dialogues matching with effects

```txt
[Events]
...
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,clarafx-1,10th
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,clarafx-1,10th
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,clarafx-2,10th
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,clarafx-1,10th
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,,10th
...
```

3. Write modified subtitle to different file

```sh
$ sbcl --eval '(require :asdf)' \
       --eval '(asdf:load-system "clarafx")' \
       --eval '(clarafx:write-subtitle-effect #p"source.ass" #p"output.ass")' \
       --eval '(sb-ext:quit)'
```

## Write your own effect

```lisp

(define-effect (my-own-effect var)
  (modifier ...)
  (modifier ...)
  ...)

(register-effect "my-own-effect" 'my-own-effect)

```

_Example file [mysubfx3.lisp](Examples/mysubfx3.lisp)_

![FX3](Examples/mysubfx3.gif)


