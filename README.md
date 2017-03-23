# regiment

> verb
> /ˈrɛdʒɪmɛnt/
>     organize according to a strict system or pattern.

A command line tool for sorting standardized separated files 
(in the sense of [RFC4180](https://tools.ietf.org/html/rfc4180)).

## Usage
--------

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

Note: `regiment` requires local storage roughly equivalent to the size of the inputs, and follows unix `TMPDIR` conventions for that storage.
