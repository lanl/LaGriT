---
title: RMSURF
tags: rmsurf
---


# RMSURF

---------------


Removes points that lie in, on or in and on the specified surface. 
The surface must be defined for the current mesh object, see [**`SURFACE`**](SURFACE.md).


## SYNTAX

<pre>
<b>rmsurf</b> / surface_name / operation
</pre>


`surface_name` is the name of the previously define surface.


`operation` can be one of the following:

 
-   **lt**  only points inside the surface are removed

-   **eq**  only points on the surface are removed

-   **le**  all points inside or on the surface are removed



