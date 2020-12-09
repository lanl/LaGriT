---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#EXECUTABLE) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](example.md)

------------------------------------------------------------------------

**INCLUDE FILES FOR MSTKLA**

------------------------------------------------------------------------

**
#include "mstkla.h"**            with the addition of
**-I/home/rao/meshing/mstkla/1.0/include** in the link line

**
#include "gmtkla.h"**            with the addition of
**-I/home/rao/meshing/mstkla/1.0/include** in the link line

**
#include "utilities.h"**           with the addition of
**-I/home/rao/utilities/1.0/include** in the link line

**
#include "macros.h"**            with the addition of
**-I/home/rao/meshing/mstkla/1.0/include** in the link line

------------------------------------------------------------------------

  **Linking with MSTKLA ** 

------------------------------------------------------------------------

**MSTKLA** (and the embedded **GMTKLA**) are in the following libs:

**/home/rao/develop/meshing/mstkla/1.0/lib/
$ARCHOS**/

where

**
$ARCHOS** is a variable that combines the architecture and operating
system info.

You can have it set in your **Makefile** by appropriately calling the
script

**/home/rao/bin/getarch** (See the **Makefile** in
**/home/rao/develop/meshing/test**)

The libraries are **libmstk.a** (debug version) and **libmstk-O.a**
(optimized version)

------------------------------------------------------------------------

**MSTKLA** also uses 2 other modules called '**clagrit**' and
'**utilities**'. The libs for clagrit  are in

**/home/rao/develop/meshing/clagrit/1.0/lib/
$ARCHOS**/

The libs are l**ibclagrit.a** (debug) and **libclagrit-O.a**
(optimized).

 

The libs for utilities are in

**/home/rao/develop/utilities/1.0/lib/
$ARCHOS**/

and the libs are **libutilities.a** (debug) and **libutilities-O.a**
(optimized)

------------------------------------------------------------------------

Also, the standard l**agrit** and **util** libs have to be included.

They are located in

**/pvcs.config/t3d/bin**

The libs are **liblagrit\_
$ARCH.a**, **libutil\_
$ARCH.a** (debug) and
**liblagrit\_
$ARCH\_opt.a**, **libutil\_
$ARCH\_opt.a** (optimized).
Here, **
$ARCH** is as follows:

    Solaris:     sol

    HP:           hp

    DEC:        dec

    SGI:          sgi

    SGI/64:     sgi\_64

    IBM:         ibm

------------------------------------------------------------------------

The order of libraries that works while linking (until more complicated
library interdependencies creep in) is

 

**-L
$(MSTKLA\_LIBDIR) -lmstkla -L
$(CLAGRIT\_LIBDIR) -lclagrit 

**

**-L
$(MYUTIL\_LIBDIR)  -lutilities 

**

**-L 
$(LAGRIT\_LIBDIR) -llagrit\_
$(ARCH)  -L 
$(CLAGRIT\_LIBDIR)
-lclagrit 

**

**-L
$(UTIL\_LIBDIR) -lutil 

**

**-lm**

 

 <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#EXECUTABLE) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](utilities.md)
