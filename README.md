# stardust

An interactive VM project in Typescript using `fp-ts`.

## Specification

Stardust is a register based virtual machine with basic input and output capabilities, meant to
run in the browser and to be easily understood.

### Registers

Stardust has registers, `I`, `S`, `A`, `B`, `C`, `D`, `W`, `X`, `Y`, and `Z`. Each can hold a 16-bit integer.

### Memory

Stardust has 64kb of RAM, making addresses `0x0000 – 0xffff` legal.

```
+---0x0000---+
General RAM (~32kb, actually 32704 bytes)
+--- 0x7fc0 ---+
Video RAM (32kb)
+---0xffc0---+
Interrupt Vectors
+---0xffe0---+
Memory Mapped IO Ports
+---0xffff---+
```

#### Interrupt Vectors

##### `0xffc0` : RESET

Reset vector for stardust. After booting will jump to the 2-byte BE address stored here.

##### `0xffc2` : VREF

Vector for when the screen has refreshed (Approx every 50th of a second).

#### `0xffc4` : IO

Vector for when an IO operation has occured.

#### `0xffc6` : Invalid

An invalid instruction was present in the instruction stream.

#### Memory Mapped IO

##### `0xffe0` : Keyboard

#### Video

Output is 16-bit color, 128x128 resolution. The video memory is stored starting
at address `0x7fc0` and goes until (but not including) `0xffc0`. Specific pixels
can be addressed via the following formula:

```
PixelAddress(x: 0-127, y: 0-127) = 0x7fc0 + (128 * 2 * y) + (x * 2);
```

### Operations

```
hlt                   # stop the world, end execution

jmp <1>               # jump to an address in register <1>
jeq <1> <2> <3>       # jump to an address in register <3> if <1> and <2> are equal
jne <1> <2> <3>       # same as above, except when not equal
jlt <1> <2> <3>       # same as above, except when less than
jgt <1> <2> <3>       # same as above, except when greater than

swp <1> <2>           # swap the values of two registers
val <1> #2            # load a big endian 32-bit value #2 into <1>

put <1> <2> #3        # store the bottom #3 bytes of the value in <1> at address <2>
get <1> <2> #3        # retrieve #3 bytes from address <1> and store it in the bottom #3 bytes of <2>

psh <1>               # put a value into the address at `S` and increment `S`
pop <2>               # pop a value from the address at `S` and decrement `S`

add <1> <2> <3>       # add <1> and <2>, storing the result in <3>
sub <1> <2> <3>       # subtract <2> from <1>, storing the result in <3>
div <1> <2> <3>       # divide <1> by <2>, storing the result in <3>
mul <1> <2> <3>       # multiply <1> and <2>, storing the result in <3>
mod <1> <2> <3>       # get the remainder of <1> / <2>, storing the result in <3>

and <1> <2> <3>       # calculate the bitwise `AND` of <1> and <2>, storing the result in <3>
ior <1> <2> <3>       # calculate the inclusive bitwise `OR` of <1> and <2>, storing the result in <3>
xor <1> <2> <3>       # calculate the exclusive bitwise `OR` of <1> and <2>, storing the result in <3>
```
