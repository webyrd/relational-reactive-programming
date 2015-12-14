# relational-reactive-programming

Beginning to explore interactive uses of miniKanren (Relational Reactive Programming).

Based on miniKanren-with-symbolic-constraints.

## Running

There are two files containing interactive examples: one which uses
Racket's full GUI toolkit (`interactive.rkt`), and one that uses the
older and simpler "legacy" graphics library.

### Full GUI examples

Open `interactive.rkt` in DrRacket.  Press the `Run` button.  Then call one of the example programs from the REPL:

```
(slider-run-n-appendo)
```

Use the slider to determine which answer
from `(run* (l s) (appendo l s '(a b c d e)))` to display.

---

### Legacy graphics examples

Open `legacy-interactive.rkt` in DrRacket.  Press the `Run` button.  Then call one of the example programs from the REPL:

---

```
(mult-by-x-pos)
```

Display all the factors of n, where n is the x position of the cursor, mod 50.

---

```
(draw-line-mult-by-x-pos)
```

Draw lines, whose x/y coordinates reflect all the factors of n,
where n is the x position of the cursor, mod 50.

---

```
(draw-line-mult-by-time)
```

Draw lines, whose x/y coordinates reflect all the factors of n, where
n is the current time in seconds, mod 50.  Uses busy-waiting to wait
for the second to increment.

---

```
(draw-line-mult-by-time-callback)
```

Draw lines, whose x/y coordinates reflect all the factors of n,
where n is the current time in seconds, mod 50.
Uses a timed callback to wait for the second to increment.

---

(scrub-run-n-appendo)

Use the x position of the mouse, mod 6, to determine which answer from
 ```(run* (l s) (appendo l s '(a b c d e)))``` to display.

---

```
(simple-clock-by-time)
```

Draw a line representing the seconds hand of a clock (current-seconds mod 60).
Doesn't use miniKanren.

---

Make sure to press the `Stop` button when you are finished!

---

TODO:

* Try implementing Wallingford, Elm, and FrTime demos.
* Exxplore the relational aspect much more.
