# regiment
==========

> verb
> /ˈrɛdʒɪmɛnt/
>     organize according to a strict system or pattern.

A command line tool for sorting separated files (in the sense of [RFC4180](https://tools.ietf.org/html/rfc4180)).

## Usage
--------

TBC

```
# defaults to sort on column 1, field separator '|'
regiment sort input-file output-dir

# explicitly specify field separator
regiment sort --field-separator ',' input-file output-dir
regiment sort -F ',' input-file output-dir

# explicitly specify column to sort on
regiment sort --key 5 input-file output-dir
regiment sort -k 5 input-file output-dir

# explicitly specify multiple columns to sort on
regiment sort --key 1 5 input-file output-dir
regiment sort -k 1 5 input-file output-dir

# explicitly specify memory upper bound of 2 GB (default unit is MB - use G to specify GB)
regiment sort --mem-max 2G input-file output-dir
regiment sort -m 2G input-file output-dir
regiment sort -m 2000 input-file output-dir

# all the things
regiment sort -F ',' -k 1 4 5 -m 10G input-file output-dir
```
