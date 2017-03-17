# regiment

> verb
> /ˈrɛdʒɪmɛnt/
>     organize according to a strict system or pattern.

A command line tool for sorting standardized separated files 
(in the sense of [RFC4180](https://tools.ietf.org/html/rfc4180)).

## Usage
--------

```
# specify column to sort on (mandatory)
# default field separator is '|' and default format of input-file is delimited
regiment sort --key 5 input-file output-dir
regiment sort -k 5 input-file output-dir

# specify multiple columns to sort on
regiment sort --key 1 --key 5 input-file output-dir
regiment sort -k 1 -k 5 input-file output-dir

# explicitly specify field separator
regiment sort --field-separator ',' --key 1 input-file output-dir
regiment sort -F ',' -k 1 input-file output-dir

# explicitly specify that format of input-file is standardized
regiment sort --format standardized --key 1 input-file output-dir
regiment sort -f standardized -k 1 input-file output-dir

# explicitly specify memory upper bound of 2 GB (default unit is MB - use G to specify GB)
regiment sort --mem-max 2G input-file output-dir
regiment sort -m 2G input-file output-dir
regiment sort -m 2000 input-file output-dir

# all the things
regiment sort -F ',' -k 1 -k 4 -k 5 -m 10G -f delimited input-file output-dir
```


