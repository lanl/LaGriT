 Example 1: connect points in a 2d geometry

 The objective is to connect a set of existing nodes (2 dimensional) into a triangle mesh that satisfies the Delaunay criterion using the  **connect** command.
 The output consists of two gmv files - one showing the point  distribution and the other showing the resulting triangle mesh.

 Example:

 [lagrit\_input\_2dconnect](../lagrit_input_2dconnect.txt)

```
# read the input points
read / avs / input.inp / cmopts

# create 2D mesh object and copy points
cmo/create/cmotri/ / /triplane
copypts/cmotri/ cmopts
cmo/delete/cmopts

# remove duplicate points if they exist
cmo/select/cmotri
filter/1,0,0
rmpoint/compress

# connect the points and set itp array
connect
resetpts/itp

# write AVS format file of the tri plane
dump / avs / output_2d_conn.inp / cmotri

# report mesh information
cmo/status
cmo/printatt//-all-/minmax
quality
```
 
Input points:

<img width="350" src="https://lanl.github.io/LaGriT/assets/images/2d_connect1.gif" 


Triangulation after connect:

<img width="350" src="https://lanl.github.io/LaGriT/assets/images/2d_connect2.gif"



