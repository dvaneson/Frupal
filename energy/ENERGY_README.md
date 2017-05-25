## Frupal - Energy feature

At a minimum, download the data (a .txt file) and the executable (main) 

To execute:

```bash
./main
```
This simply allows you to see how much energy hero currently has.

```bash
./main sub somenumber
```

This subtract's somenumber(an arbitrary integer) from the total amount of energy the hero currently has.

```bash
./main add somenumber
```

This adds somenumber (arbitrary integer) to the total amount of energy the hero currently has.

```bash
./main reset somenumber
```

This sets/resets the total amount of energy to somenumber (arbitrary integer).

**NOTE: ** If energy is at 0 and you want to get the hero's energy to 100, for example, use ./main reset 100. Users can only use the add functionality while the energy is not 0.

To see a version of this guide in the command line interface:

```bash
./main help
```
