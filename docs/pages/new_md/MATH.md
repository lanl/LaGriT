---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: MATH
---

MATH
----

 The **math** routine operates on attributes of a mesh object(s). It
 performs arithmetic operations or evaluates mathematical functions on
 the source mesh object or objects, and places the results in the sink
 mesh object. The source and sink mesh objects can be the same, and
 there can be either one or two source objects, depending on the
 operation selected.

 All attributes must have the same type, rank, and length.

 The last parameter, value, may or may not be used according to the
 operation details listed below.

 For the standard arithmetic operations, value can be either a constant
 or an attribute. These arithmetic operations work for all types of
 attributes.

 For the mathematical functions other than **floor** and **ceiling**, 
 value is omitted, and the function is performed on the src\_attr and
 stored in the sink\_attr. Mathematical functions other than **floor**
 and **ceiling** are not implemented for attributes whose values are
 integers.

**FORMAT:**

 **math** / operation / cmo\_sink/attr\_sink / range
 /cmo\_src/attr\_src / [ value ]



operation: The first parameter is one of the following keywords that
indicates the type of work to perform.

 **plus, add, minus, sub, subtract, times,  multiply, mult, divide,
 min, max** are arithmetic operators; the result is stored in
 sink\_attr:

 sink\_attr = (src\_attr) operator (value), where value can be either a
 numerical constant or a second mesh object attribute.[

 min, max ]{style="font-weight: bold;"are not to be confused with the
 minimum or maximum value of an attribute; rather the the result is a
 comparison of pairs of source values.


 **sin, cos, tan, ln** (natural log), and **log10** are mathematical
 functions. The value parameter is omitted, and the function is
 performed on the src\_attr and stored in the sink\_attr. These
 functions are not implemented for integer attributes.

 **floor** and **ceiling** are mathematical functions where value is
 used as the lower or upper limit, the value(s) of src\_attr are
 checked against value, and the results are stored in the sink\_attr.
 These functions work for all types of attributes.

 **power** function uses both value parameters. The first value or
 src\_attr is raised to the power of the second value or attribute. You
 cannot use two constants. At least one of the sources must be an
 attribute. The result is stored in the sink\_attr.

 **exp** and **exp10** functions raise the constant e or the constant
 10 to the power specified by src\_attr and stores the result in the
 sink\_attr.

 &lt;&gt;**integrate** function computes the product of 'field\_value
 'times 'element volume' at each element and either saves these
 products or sums the products and saves the integrated result.

 The syntax is:
 [math/integrate/]{style="font-weight: bold;"&lt;/&gt;[cmo\_sink/attr\_sink/range/attr\_src\_field]{style="font-family: monospace;"&lt;&gt;[]{style="font-weight: bold;"

 &lt;/&gt;&lt;&gt;The 'field\_value' for an element is either the value
 of attr\_src (if the attr\_src is an element attribute and has length
 'nelements') or is the average of the values at the vertices of the
 element (if the attr\_src is a node attribute has length 'nnodes').

 If sink\_attr does not exist or if it exists and has length
 'nelements' and type 'VDOUBLE' the products (&lt;/&gt;&lt;&gt;
 &lt;/&gt;&lt;&gt;'field\_value' times 'element\_volume') are stored in
 sink\_attr.

 If sink\_attr exists and has length 'scalar' and type 'REAL', then the
 products are summed up and the resulting sum if stored in sink\_attr.
 (If the user requires just the integrated sum this alternative avoids
 having to use the pair of commands '[integrate,
 sum]{style="font-weight: bold;"' and also avoids creating the
 'nelement' long sink attribute)

 It is assumed that the sink and source attributes are in the same mesh
 object and the second cmo name is ignored.

 If range is used, it must refer to a set of elements. &lt;/&gt;

 **sum** adds all node or element values in attr\_src, within the
 selected range and writes the result to attr\_sink. The sink attribute
 must be of type 'REAL' or 'INT' (length='scalar') and will be created
 if it does not exist.



cmo\_sink, attr\_sink: are the sink cmo and sink attribute for the math
results to be written to. These parameters are required for all math
operations.



range: is the selection set of elements or nodes for the math operation
and may be in one of these 3 forms:

 /ifirst,ilast,istride / numbers indicating attribute set  (1,0,0 means
 all elements or nodes)

 **/pset,get**, pset\_name / for attributes with length = 'nnodes' 
 (all nodes in the named point set)

 **/eltset,get**, eltset\_name / for attributes with length =
 'nelements' (all elements in the named element set)




value: is required by some math operations and can be of type constant
or can be a cmo attribute. The following are possible forms:

 /cmo\_src2/attr\_src2/ where cmo\_src2 may be the same name as the
 source cmo, or the name of a second source cmo.

 /attr\_src2/ assumes attribute is a part of cmo\_src

 /constant/ is a numerical value



**EXAMPLES:**

**math** **/multiply**/sink\_mo/sink\_attribute/50,60,3/src\_mo/src\_attribute/1.0

**math/add**/mo/attribute/50,60,3/mo/attribute/100.0

**math/sub**/sink\_mo/sink\_attribute/50,60,3/src\_mo1/src\_attribute1/src\_mo2/src\_attribute2/

**math/min**/sink\_mo/sink\_attribute/1,0,0/src\_mo1/src\_attribute1/src\_mo2/src\_attribute2/

**math** **/ln**/sink\_mo/sink\_attribute/1,0,0/src\_mo/src\_attribute/

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

 



