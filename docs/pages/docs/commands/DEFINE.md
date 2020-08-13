---
title: DEFINE 
tags: define variable 
---


# DEFINE

------------------------------

Allows a value to be associated with a character string, such that
  the character string can be used in input decks in place of the 
  number. Up to 100 declarations can be defined. 

The keyword **remove** in the third argument will remove a
  defined variable from the stack of defined variables.

Each instance of the **`define`** command will overwrite the previous assignment.

  
## SYNTAX

<pre>
<b>define</b> / name / value_real

<b>define</b> / name / value_integer

<b>define</b> / name / value_character

<b>define</b> / name / <b>remove</b>
</pre>


## EXAMPLES

```
define / nx / 3
define / ny / 4 
define / nz / 5 
define / bottom / 0.1
define / top / 4.0 
define / left / 9.8 
define / type / reflect

surface/s1/reflect/box/0.0,left,bottom/1.0,right,top 
rz/xyz/nx/nz/0.0,left,bottom/1.0,right,top/1,1,1
```

```
define / top / 1.0 
define / top / 5.0 
surface/s1/reflect/box/0.0,left,bottom/1.0,right,top 
rz/xyz/nx/nz/0.0,left,bottom/1.0,right,top/1,1,1

define / nx / remove
define / ny / remove
define / nz / remove
```
