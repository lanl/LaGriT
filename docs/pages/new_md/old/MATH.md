---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: MATH
---

MATH
----

 The **math** routine operates attributes of a mesh object. It performs
 arithmetic operations or mathematical functions on the source mesh
 object or objects, and places the results in the sink mesh object. The
 source and sink mesh objects can be the same, and there can be either
 one or two source objects, depending on the function selected.
 All attributes must have the same type, rank, and length.

 The last parameter, value, may or may not be used according to the
 operation details listed below.

 For the standard arithmetic operations, value can be either a constant
 or an attribute. The operation is performed as sink\_attr =
 (src\_attr) operator (value). These operations work for all types of
 attributes.

 For the mathematical functions other than **floor** and **ceiling**,
 the value is omitted, the function is performed on the src\_attr and
 stored in the sink\_attr. These functions are not implemented for
 attributes whose values are integers.

FORMAT:

 **math** / operation / cmo\_sink/attr\_sink / range
 /cmo\_src/attr\_src / 
[ value 
]



operation: The keyword choices for the first parameter indicate the
arithmetic, functional, or operational types of work to perform.

 **plus, add, minus, sub, subtract, times, divide, multiply** or
 **mult** are arithmetic operations and take the form of

 sink\_attr = (src\_attr) operator (value)

 where value can be either a numerical constant or a mesh object
 attribute.

 **sin, cos, tan, ln** (natural log), and **log10** are mathematical
 functions. The value parameter is omitted, and the function is
 performed on the src\_attr and stored in the sink\_attr. These
 functions are not implemented for attributes whose values are
 integers.

 **floor** and **ceiling** are mathematical functions where value
 parameter is used as the lower or upper limit, the value(s) of
 src\_attr are checked against this value, and the results are stored
 in the sink\_attr. These functions work for all types of attributes.

 **power** function uses both value parameters. The first value or
 src\_attr is raised to the power of the second value or attribute. You
 cannot use two constants. At least one of the sources must be an
 attribute. The result is stored in the sink\_attr.

 **exp** and **exp10** functions raise the constant e or the constant
 10 to the power specified by src\_attr and stores the result in the
 sink\_attr.

 **integrate** operation is used to integrate a field value over
 elements. Given a scalar function f(x,y,z), compute the integral I =
 Int f dV over each element and put the result into the element array
 (i.e. multiply the volume of the element by the value of the field for
 that element).

 The syntax is: **math/integrate**/
 cmo\_sink/attr\_sink\_face/range/attr\_src\_field

 It is assumed that the sink and source attributes are in the same mesh
 object and the second cmo name is ignored for this operation. The
 field value for an element is either the element value (if the source
 attribute has length nelements) or is the average of the values at the
 vertices of the element (if the source attribute has length nnodes).
 The sink attribute will be created as a vector type nelements in
 length if it does not exist. If the sink attribute exists and is type
 REAL, then the sum of the element integrals is computed and saved in
 the sink attribute.  If range is used, it must refer to a set of
 elements. idebug levels change at 1 5 and 9 with idebug greater than 9
 producing the most output and 1 the least.

 **sum** operation adds all node or element values in attr\_src, within
 the selected range and writes the result to attr\_sink. The sink
 attribute must be of type REAL or INT (length=1, rank=1) and will be
 created if it does not exist.



cmo\_sink, attr\_sink: are the sink cmo and sink attribute for the math
results to be written to. These parameters are required for all math
operations.



range: is the selection set of elements or nodes for the math operation
and is in the form:

 /ifirst,ilast,istride / numbers indicating attribute set

 **/pset,get**, pset\_name / for attributes with length = 'nnodes'

 **/eltset,get**, eltset\_name / for attributes with length =
 'nelements'



value: is required by some math operations and can be of type constant
or can be a cmo attribute. The following are possible forms:

 /cmo\_src2/attr\_src2/ where cmo\_src2 may be the same name as the
 source cmo, or the name of a second source cmo.

 /attr\_src2/ assumes attribute is a part of cmo\_src

 /constant/ is a numerical value



EXAMPLES:

**math****/multiply**/sink\_mo/sink\_attribute/50,60,3/src\_mo/src\_attribute/1.0

**math/add**/mo/attribute/50,60,3/mo/attribute/100.0

**math/sub**/sink\_mo/sink\_attribute/50,60,3/src\_mo1/src\_attribute1/
src\_mo2/src\_attribute2/

**math****/ln**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/

**math/floor**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/2.0/

**math/power**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/2.0/

**math/power**/sink\_mo/sink\_attribute/1,0,0/2.0/src\_mo/src\_attribute/

**math/power**/sink\_mo/sink\_attribute/1,0,0/base\_mo/base\_attribute/
power\_mo/power\_attr

**math/exp**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/

**math/exp10**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/

**math/integrate**/ cmotri /Vf /1,0,0/ cmotri/ Fn

**math/sum**/ cmotri / Vfsum /1,0,0/ cmotri / Vf

**math/sum**/ cmotri / area\_sum /1,0,0/ cmotri / darea

