### Format of temp files 

Sorted temp files will be written in a binary format following the structure described below.

```
+-----------------------------------------------+-------------------------------------------------+
|                                               |                                                 |
|                                               |                                                 |
|                 Block Size (Int32)            |              Block Count (Int 32)               |
|                                               |                                                 |
|                                               |                                                 |
+-----+------------+-----+----------+-----------+-------------------------------+-----+-----------+
|     |            |     |          |                                           |     |           |
|     |            |     |          |                                           |     |           |
| Size|  SortKey 1 | Size| SortKey 2|       .......                  SortKey n  |Size | Payload   |
| SK_1|            | SK_2|          |                                           |     |           |
|     |            |     |          |                                           |Pay  |           |
|     |            |     |          |                                           |Load |           |
|     |            |     |          |                                           |     |           |
+-----+------------+-----+----------+-------------------------------------------+-----+-----------+
```
