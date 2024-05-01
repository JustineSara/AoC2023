# AoC2023

My solutions to [Advent of Code 2023].

[Advent of Code 2023]: https://adventofcode.com/2023

## To run
Use :
```lein run -m t.day01```
in the terminal.

Change `day01` as needed.

## Lessons (learned?)

### Day 01

Using `Integer.` is a bad idea.\
Capital Letter ==> java class\
Capital Letter + "." ==> constructor of the java class :\
    - creates new object
    - Integer : in 32bits  ==> max is 2.10^9 : not that much with advent of code and no warning/error
Solution : better to use integer 64bit: Long\
Let's **not** use `Long.` because again new object... instead : **`parse-long`**



## License

Copyright © 2023 Justine Sara Pujos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
