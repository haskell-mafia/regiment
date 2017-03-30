# regiment

> verb
> /ˈrɛdʒɪmɛnt/
>     organize according to a strict system or pattern.

A command line tool for sorting standardized separated files 
(in the sense of [RFC4180](https://tools.ietf.org/html/rfc4180)).

## Usage
--------

`sort` a standardized separated file.

```
# specify:
#   column to sort on (mandatory)
#   number-of-columns in file (mandatory)
#   field-separator (mandatory)
#
# default output is to stdout
# default newline is LF
# default format of input-file is delimited
# default upper bound for memory is (1024 * 1024) bytes
regiment sort --key 5 --number-columns 15 --field-separator ',' input-file
regiment sort -k 5 -c 15 -f ',' input-file

# specify multiple columns to sort on
regiment sort --key 1 --key 5 --number-columns 15 --field-separator ',' input-file
regiment sort -k 1 -k 5 -c 15 -f ',' input-file

# explicitly specify that format of input-file is standardized
regiment sort --key 1 --number-columns 15 --standardized --field-separator ',' input-file
regiment sort -k 1 -c 15 -f ',' --standardized input-file

# explicitly specify memory upper bound of 2 GB (default unit is MB - use G to specify GB)
regiment sort --mem-max 2G --key 1 --number-columns 15 --field-separator ',' input-file
regiment sort -m 2G -k 1 -c 15 -f ',' input-file
regiment sort -m 2000 -k 1 -c 15 -f ',' input-file

# explicitly specify newline -- one of LF, CR or CRLF (defaults to LF)
regiment sort --key 5 --number-columns 15 --crlf --field-separator ',' input-file
regiment sort -k 5 -c 15 -f ',' --crlf input-file

# explicitly specify path to output file -- defaults to stdout
regiment sort --key 5 --number-columns 15 --field-separator ',' --output "path/to/output-file" input-file
regiment sort -k 5 -c 15 -f ',' -o "path/to/output-file" input-file

# all the things
regiment sort -f ',' -k 1 -k 4 -k 5 -c 26 -m 10G --crlf --standardized -o "path/to/output-file" input-file
```

`split` a standardized separated file into a set of temporary files, each of which is sorted,
and is in regiment's [binary format](doc/temp-file-format.md)

```
# specify:
#   same options as for sort (except for --output)
#   a directory within which to write the sorted splits (mandatory)
#   NOTE: this directory must not exist, it will be created for you
regiment split <same opts as sort> --dir "path/to/output-dir" input-file
regiment split <same opts as sort> -d "path/to/output-dir" input-file
```

Given the format of an input standardized separated file, merge a set of sorted temporary files
(in regiment's [binary format](doc/temp-file-format.md)) into an output-file (that has the same format
as the input standardized separated file).

```
# specify:
#   directories containing sorted splits that require merging (typically outputs of running split)
#   output file (optional) -- defaults to stdout
regiment merge-tmps dir1 dir2 ... dirn

# explicity specify path to output file -- defaults to stdout
regiment merge-tmps --output "path/to/output-file" dir1 dir2 ... dirn
regiment merge-tmps -o "path/to/output-file" dir1 dir2 ... dirn
```

Relationship between `sort`, `split` and `merge-tmps`

```
regiment sort -k 1 -c 5 -f ',' --standardized input-file 

generates the same output as

regiment split -k 1 -c 5 -f ',' --standardized -d "/foo/bar/baz" input-file
regiment merge-tmps "/foo/bar/baz"
```

Note: `regiment` requires local storage roughly equivalent to the size of the inputs,
and follows unix `TMPDIR` conventions for that storage.
