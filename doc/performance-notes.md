### Notes on performance

20170323 - with no performance tuning, at its inception (around commit `b6da9b7`):

```
Sorting an 11GB file (on a Macbook Pro):

gnu-sort (defaults): LC_COLLATE=C sort -t '|' -k 3,3 -o ~/Downloads/grohl/sort-sauerkraut  314.75s user 67.72s system 96% cpu 6:36.24 total

gnu-sort (2GB memory allocation): LC_COLLATE=C sort -t '|' -k 3,3 -S 2G -o ~/Downloads/grohl/sort-sauerkraut  346.95s user 34.03s system 97% cpu 6:32.65 total

regiment (2GB memory allocation): ./dist/build/Regiment/regiment sort -c 4 -k 3 -f '|' -m 2147483648 -o    3283.97s user 481.99s system 95% cpu 1:05:54.34 total
```

Results of profiling points clearly to the need to improve `updateMinCursor`:

```
COST CENTRE        MODULE                 %time %alloc

updateMinCursor    Regiment.Vanguard.Base  69.3   80.1
runVanguard        Regiment.Vanguard.Base   7.5    9.8
compare            Regiment.Data            6.6    0.0
flushVector        Regiment.Parse           3.3    1.4
compare            Regiment.Data            2.8    0.0
readKeyedPayloadIO Regiment.Vanguard.IO     1.8    1.2
writeCursor        Regiment.Parse           1.2    1.4
selectSortKeys     Regiment.Parse           1.0    1.1
```


