---
title: UNG2AVS
tags: ung2avs
---

# UNG2AVS


--------

This routine is used to convert files in UNGenerate FORMAT to AVS format. UNGenerate files are produced by ARCINFO.

The UNG format is described below, see AVS format at [**READ/AVS**](read_avs.md).


## SYNTAX

<pre>
ung2avs/avs_file_out/ung_file_in

ung2av/avs_file_out/ung_file_in/[constant]/ & [xy, xz, yx, zx, yz, zy]
</pre>


`avs_file_out` is the name of the AVS file to write

`ung_file_in` is the name of the UNG file to read


### Options

`constant` (default 0.) is the value to assign to z or the second axis indicated by axis options. 


**xy xz yx zx yz zy** (default **xy**) detirmines axis values to write.
 



## EXAMPLES

```
ung2avs/avs_file_out/ung_file_in
```
converts ung_file_in to avs_file_out with a constant default z value of 0.0

```
ung2avs/avs_file_out/ung_file_in/7.0
```
converts ung_file_in to avs_file_out with a constant default z value of 7.0 in xy order

```
ung2avs/avs_file_out/ung_file_in/15.0/xz
```
converts ung_file_in to avs_file_out with a constant y value of 15.0 in xz order


## UNG file format

The UNG format contains a file of xy values, in groups of sequentially numbered sets, of connected line segments, each ending
  with END. The format for connected lines that form polygons or that do not form polygons is very similar. To make sets of connected
  lines that are polygons the first xy pair for each set of lines must be on the same line as the set number. Closed polygons will be
  formed by connecting the last point of a point set to the first point of the  point set. The set number is on a line by itself for
  non-polygons. Spacing on a line must not contain tabs.


### FORMAT for connected lines that do not form polygons:

<pre>
1
x y
x y
x y
...
END
n
x y
x y
x y
END
END
</pre>
     

### FORMAT for polygons:

<pre>
1 x y
x y
x y
...
END
n x y
x y
x y
END
END
</pre>
     
     
### File SAMPLE of connected, non-polygon, lines:

<pre>
  <b>UNG File</b>         <b>AVS File</b>
---------------------------------------------
1                   10   7 0 0 0
10.00   15.00       1     10.0000 15.0000  0.0000
10.50   15.50       2     10.5000 15.5000  0.0000
11.00   16.00       3     11.0000 16.0000  0.0000
END                 4     20.0000 25.0000  0.0000
2                   5     20.5000 25.5000  0.0000
20.00   25.00       6     21.0000 26.0000  0.0000
20.50   25.50       7     22.0000 27.0000  0.0000
21.00   26.00       8     30.0000 35.0000  0.0000
22.00   27.00       9     30.5000 35.5000  0.0000
END                 10    31.0000 36.0000  0.0000
3                   1  1 line    1   2
30.00   35.00       2  1 line    2   3
30.50   35.50       3  2 line    4   5
31.00   36.00       4  2 line    5   6
END                 5  2 line    6   7
END                 6  3 line    8   9
                    7  3 line    9   10
---------------------------------------------
</pre>
  
### File SAMPLE of polygons:

<pre>
  <b>UNG File</b>         <b>AVS File</b>
---------------------------------------------
1 11.00   16.00           10  10  0 0 0
  10.00   15.00           1   10.0000 15.0000  0.0000
  10.50   15.50           2   10.5000 15.5000  0.0000
  11.00   16.00           3   11.0000 16.0000  0.0000
END                       4   20.0000 25.0000  0.0000
2 22.00   27.00           5   20.5000 25.5000  0.0000
  20.00   25.00           6   21.0000 26.0000  0.0000
  20.50   25.50           7   22.0000 27.0000  0.0000
  21.00   26.00           8   30.0000 35.0000  0.0000
  22.00   27.00           9   30.5000 35.5000  0.0000
END                      10   31.0000 36.0000  0.0000
3 31.00   36.00          1   1 line    1    2
  30.00   35.00          2   1 line    2    3
  30.50   35.50          3   1 line    3    1
  31.00   36.00          4   2 line    4    5
END                      5   2 line    5    6
END                      6   2 line    6    7
                         7   2 line    7    4
                         8   3 line    8    9
                         9   3 line    9   10
                         10  3 line   10    8
---------------------------------------------
</pre>

