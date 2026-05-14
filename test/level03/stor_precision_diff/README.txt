Issue with new mac compilers result in precision differences with very small numbers
This causes failures in the test comparisons between mac runs and reference

FIX TEST: change test to avoid square, make vertical length shorter than horiz.
FIX TEST: or set user value for epsilon zero volume


For "degenerate" cases with 4 vertices on circumscribed circle, you can put the diagonal edge of the rectangle on either diagonal and round off may be the deciding factor. Different connection of degenerate case is still correct/Delaunay.

NOTE files written without zero coefs are the same:
output_mac/out_hex_3x4.uge output_linux/out_hex_3x4.uge


Files with all coefs including zero coefs differ:

diff output_mac/out_hex_3x4.stor output_linux/out_hex_3x4.stor
2,3c2,3
<  Thu Dec 21 11:21:38 20233-D Linear Diffusion Model (matbld3d_astor)
<          7        24       143         1         6
---
>  Tue Jan  2 09:50:13 20243-D Linear Diffusion Model (matbld3d_astor)
>          8        24       145         1         7
13c13
<        124       129       133       139       143
---
>        124       130       134       141       145
34,37c34,37
<         18        20        21        24        10
<         19        22        23        11        12
<         20        22        23        24        12
<         21        23        24
---
>         18        20        21        23        24
>         10        19        22        23        11
>         12        20        21        22        23
>         24        12        21        23        24
49c49
<          4         7         7         6         6
---
>          4         7         8         6         6
58,61c58,62
<          2         3         7         2         6
<          2         7         4         5         7
<          1         4         7         4         6
<          2         4         7         0         0
---
>          2         3         7         7         2
>          6         2         7         4         5
>          8         1         7         4         7
>          4         6         2         4         7
>          0         0         0         0         0
66d66
<          0         0         0
71c71
<        128       132       138       143
---
>        128       133       140       145
73c73
<  -4.166666666667E-02 -1.515940605973E-17
---
>  -4.166666666667E-02 -4.618806972100E-17 -1.671098990551E-17



