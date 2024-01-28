# diffcar

diffcar is a tool for comparing [Stunts](https://stunts.hu) CAR\*.RES
files. It prints nicely formatted, human-readable diffs of pairs of
files, comparing their contents field by field.

diffcar currently supports two commands:

- `diffcar single`, which takes two files and prints their differences.

- `diffcar report`, which takes two directories and prints diffs for
  pairs of files with the same name (ignoring case) found in them both.
  The output is printed to the terminal by default; an output file can
  be specified instead with the `-o` option.

The commands support a small handful of formatting options; using
`--help` with any command will show what is available.

More information on the structure of CAR\*.RES files and the meaning of
the fields can be found at the Stunts Wiki:

- <https://wiki.stunts.hu/wiki/Car_files#Car_behaviour_.28car.2A.res.29>

- <https://wiki.stunts.hu/wiki/Car_parameters>
