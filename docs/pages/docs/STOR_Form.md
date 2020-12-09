# FEHMN Stor File Format

The sparse matrix data file is both an input and an output file the FEHM application uses for storing or reading geometric coefficients associated with a finite element mesh. The sparse matrix data file is read during FEHM initialization if being used for input. The generated name is of the form filename.stor and can be written in ascii or binary (unformatted) mode. 

The stor file is written with "blocks" of information data in the following format.

1. ASCII Header consisting of 2 lines giving code version information, date, and time.

2. Single line with matrix integer parameters; number_coefs, nodes, memory_size, num_area_coefs, max_num_connections.

3. List of Voronoi Volumes associated with each node. 

4. List indicating the number of entries for each row of the matrix. If a node in a mesh has N edge connections, this number is N+1.

5. Connectivity list. Row i is a list of all nodes j1, j2, j3... connected to node i. The list contains the matrix diagonal entry, i.

6. Pointers or index numbers into the last block containing values for area coefficients.

7. List of Geometric area coefficients. In general this is the area of each face of the Voronoi polygon associate with node i, face j


## ASCII Header Lines

```
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
 Thu Jul 25 08:41:03 20193-D Linear Diffusion Model (matbld3d_astor)
```
Title Line: The first line consists of a character string of 72 or less characters. Characters 14-17 should be labled as one of r4i4 r4i8 r8i4 r8i8. This signifies the number of bytes used for reals 'r' and integers 'i'. For instance, r8i4 means 8 byte reals, 4 byte integer. The second word also contains information stating whether the file is binary or ASCII (text). If it is ASCII, then characters 10-16 should be 'asci.' If it is binary, then characters 10-16 should be 'ieee'.

Date Line: The second line contains a character string of 72 or less characters. This line contains a time stamp of when the file was written and the code option used for writing this file.  


## Matrix Parameters
 
```
      3        12        65         1         5
```
The Parameter line has integer values: NUM_WRITTEN_COEFS, NEQ, NCOEF+NEQ+1, NUM_AREA_COEF, and optional value NCON_MAX.

NUM_WRITTEN_COEFS: is the number coefficient values written in the .stor file. If compression is not used, then this is number is equal to the number of connections in the grid. If compression is used, each unique value will be written once and pointers will reference the appropriate value.

NEQ: signifies the number of equations and is equivalent to the number of nodes in a triangular or tetrahedral grid. 

NCOEF+NEQ+1: is a value used to allocate memory in FEHM for the integer pointers. 

NUM_AREA_COEF: Can be (1,3, or 4) number of area coefficients. The area/distance coefficients are the area of a Voronoi face divided by the Delaunay edge length. This coefficient is either written as a scalar (1), a vector area (3), or both vector and scalar areas are written (4). 

NCON_MAX: is an optional parameter indicating the maximum number of connections to a single node in the entire mesh. This is used for memory allocation. If omitted, FEHM will default to a value that might cause problems in highly unstructured meshes. 


## Voronoi Volumes ##

```
1.250000000000E-01  2.500000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
2.500000000000E-01  2.500000000000E-01  2.500000000000E-01  1.250000000000E-01  1.250000000000E-01
1.250000000000E-01  1.250000000000E-01
```
This block has NEQ (number of nodes) floating point values which are the Voronoi Volumes associated with each node.

Each mesh node is a voronoi center for the volume. So nodes on the boundary will have a fractional volume, for instance a 90 degree corner node will have half or quarter volumes compared to the nodes internal to the mesh. A bottom node will have half the volume of nodes above and internal given the same spacing.


## Row Counts for Matrix ##
```
   13        17        22        26        30
   34        39        44        49        53
   57        61        65
```
The purpose of this block is to indicate the number of specified entries each row of the matrix contains. (All unspecified entries are assumed to be 0). 

The representation is circuitous. There are a total of NEQ+1 entries. The first entry is the value NEQ+1. The second entry is the value NEQ+1+the number of connections the in the first row. The third entry is value of the second entry + the number of connections in the second row. In general, the ith entry (for i1) is the i-1th entry + the number of specified entries in the row i-1. In this way, the difference between the i+1st and the ith value indicates how many entries row i has in the matrix. 

In the above example the matrix has 13 entries as indicated in the first number (12 nodes + 1). After the first, there are 12 numbers, each representing the number of values in each row of the matrix.


## Row Entries ##
```
1 2 3 
2 3 
```
This block is used to state which entries of the matrix are explicitly represented. Since we know the number of entries per row, (given above), we can do this by stating which columns of each row are represented. 

In above example we have a 3x3 matrix, use these numbers in the following manner:

The first row has specified entries in the first, second, and third columns (i.e, entries (1,1), (1,2) and (1,3) are specified ; the second row has an entry specified in the second column entry (i.e., (2,2)); the third row has an entry specified in the third column (i.e. (3,3)). 


## Indices into Coefficient List ##

This block contains NCOEF integer pointers to entries to the Coefficient Block (described below.) The actual values of a matrix entry can be repeated many times, particularly in a matrix arising from the discretization of a structured mesh. Because floating point values require more storage than integers, we have set up the following data structure to allow compression: 

View the floating point values as two dimensional array of dimensions NUM_WRITTEN_COEFS by NUM_AREA_COEF. 

To represent a matrix entry, give the index of value contained in the floating point values block here. If a 0 is given, then the writer of the .stor file has explicitly stated he wants the value to be 0.0.

Following the NCOEF integer pointers are: 1) NEQ+1 0's for padding and 2) a set of NEQ+1 integer pointers stating which pointers correspond to diagonal elements. 


## Geometric Area Coefficient Values ##
```
-5.000000000000E-01 -2.500000000000E-01  0.000000000000E+00
```

This block contains NUM_WRITTEN_COEFS x NUM_AREA_COEF floating point values. 

NUM_AREA_COEF = 1 The scalar area for each connection a(i=1,k). This is the default.

NUM_AREA_COEF = 3 The vector area for each connection in the order a_x(i=1,k), a_y(i=1,k), a_z(i=1,k). 

NUM_AREA_COEF = 4 The vector areas first followed by the scalar area. Note that a(i) = sqrt(a_x^2 + a_y^2 + a_z^2) 

Note the negative values because flow is from high to low. The flow (energy/temperature, fluid/Darcy) is governed by the diffusion equation  
q = C x grad(P)

The geometric part of the equation is area/distance. The area comes in for figuring out the mass flow given the material properties, gradient, etc. The distance is use in computing grad(P). The negative comes in because the grad(P) vector points from low to high P, but flow is from high to low.
 

## LaGriT Output ##

In addition to the .stor file, LaGriT writes a summary to the screen and file output. This includes information useful to the modeler such as the min and max voronoi volumes (should be greater or equal to zero), total voronoi volume, and possible negative coupling coefficients which can occur on non-convex boundaries.

```
# use default format for ASCII file with compression

dump/stor/ tet / cmotet

*** Construct and Compress Sparse Matrix:3D ***                                 
   *** Compress Area Coefficient Values ***                                     
 
AMatbld3d_stor: Matrix compress_eps:  0.1000000E-07                             
AMatbld3d_stor: Local epsilon:  0.1000000E-14                                   

SparseMatrix initialize epsilon to 1.000000e-08
SparseMatrix using Epsilon 1.000000e-08
AMatbld3d_stor: *****Zero Negative Coefficients ******                          
AMatbld3d_stor: Number of 'zero' (< compress_eps) coefs  0               
AMatbld3d_stor: npoints =       12  ncoefs =         52                         
AMatbld3d_stor: Number of unique coefs =         3                              
AMatbld3d_stor: Maximum num. connections to a node =  5                 
AMatbld3d_stor: Volume min =   1.2500000E-01                                    
AMatbld3d_stor: Volume max =   2.5000000E-01                                    
AMatbld3d_stor: Total Volume:   2.0000000E+00                                   
AMatbld3d_stor: abs(Aij/xij) min =   0.0000000E+00                              
AMatbld3d_stor: abs(Aij/xij) max =   5.0000000E-01                              
AMatbld3d_stor: (Aij/xij) max =   0.0000000E+00                                 
AMatbld3d_stor: (Aij/xij) min =  -5.0000000E-01                                 
AMatbld3d_stor Matrix coefficient values stored as scalar area/distance         
AMatbld3d_stor Matrix compression used for graph and coefficient values         
ascii STOR file written with name tet.stor                                      
 
*** SPARSE COEFFICIENT MATRIX _astor SUCCESSFUL ***                             
 3D Matrix Coefficient file written with name tet.stor    
```


## Example STOR file compress all, graph and coef (astor) ##

This is the default stor format and will compress values into an ASCII file. (Binary files can only be read on machines they have been written on). This provides the smallest possible file size based on the number of unique coefficient values in the mesh. This example file is summarized in the above output report.

```
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
 Thu Jul 25 14:23:32 20193-D Linear Diffusion Model (matbld3d_astor)
         3        12        65         1         5
  1.250000000000E-01  2.500000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
  2.500000000000E-01  2.500000000000E-01  2.500000000000E-01  1.250000000000E-01  1.250000000000E-01
  1.250000000000E-01  1.250000000000E-01
        13        17        22        26        30
        34        39        44        49        53
        57        61        65
         1         2         3         4         1
         2         7         8        11         1
         3         5         8         1         4
         5         7         3         4         5
         6         5         6         7         8
        12         2         4         6         7
         9         2         3         6         8
        10         7         9        11        12
         8        10        11        12         2
         9        10        11         6         9
        10        12
         3         2         2         2         2
         3         1         1         2         2
         3         2         2         2         3
         2         2         2         2         3
         2         2         3         1         1
         2         1         2         1         3
         2         1         2         1         3
         2         2         3         2         2
         2         3         2         2         2
         2         2         3         2         2
         2         3         0         0         0
         0         0         0         0         0
         0         0         0         0         0
        14        19        24        28        33
        36        43        48        51        55
        61        65
 -5.000000000000E-01 -2.500000000000E-01  0.000000000000E+00
```

## Example STOR file with no compression (nstor) ##

This is used if you want a file with all values in the matrix written, no compression.

```
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
 08/14 10:23:54 20093-D Linear Diffusion Model (matbld3d_nstor)
      46         8        55         1         8
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
       9        14        22        27        32
      37        42        50        55         1
       2         3         5         7         1
       2         3         4         5         6
       7         8         1         2         3
       4         7         2         3         4
       7         8         1         2         5
       6         7         2         5         6
       7         8         1         2         3
       4         5         6         7         8
       2         4         6         7         8
       1         2         3         4         5
       6         7         8         9        10
      11        12        13        14        15
      16        17        18        19        20
      21        22        23        24        25
      26        27        28        29        30
      31        32        33        34        35
      36        37        38        39        40
      41        42        43        44        
      46         0         0         0         0
       0         0         0         0         0
      10        16        25        30        35
      40        49        55
 0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01  0.000000000000E+00
-2.500000000000E-01  0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00
-2.500000000000E-01  0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00
 0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01
 0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01  0.000000000000E+00
 0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01
 0.000000000000E+00  0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00  0.000000000000E+00
-2.500000000000E-01  0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00  0.000000000000E+00
-2.500000000000E-01  0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01
 0.000000000000E+00
```

## Example STOR file with coef compression but no graph compression (cstor) ##

```  
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
  08/13 15:40:06 20093-D Linear Diffusion Model (matbld3d_cstor)
        2         8        55         1         8
 1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
 1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
        9        14        22        27        32
       37        42        50        55         1
        2         3         5         7         1
        2         3         4         5         6
        7         8         1         2         3
        4         7         2         3         4
        7         8         1         2         5
        6         7         2         5         6
        7         8         1         2         3
        4         5         6         7         8
        2         4         6         7         8
        1         2         2         2         1
        2         1         1         2         1
        2         1         1         2         1
        1         2         2         2         2
        1         1         2         2         1
        1         2         2         2         2
        1         1         2         1         1
        2         1         2         1         1
        2         1         2         2         2
        1         0         0         0         0
        0         0         0         0         0
       10        16        25        30        35
       40        49        55
      0.000000000000E+00 -2.500000000000E-01
```

## Example STOR file with coef compression off and graph compression on (gstor) ##

```
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
  08/13 15:40:06 20093-D Linear Diffusion Model (matbld3d_gstor)
      20         8        41         1         4
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
       9        13        17        21        25
      29        33        37        41
       1         2         3         5         1
       2         4         6         1         3
       4         7         2         3         4
       8         1         5         6         7
       2         5         6         8         3
       5         7         8         4         6
       7         8
       1         2         3         4         2
       5         6         7         3         8
       9        10         6         9        11
      12         4        13        14        15
       7        14        16        17        10
      15        18        19        12        17
      19        20         0         0         0
       0         0         0         0         0
       0
      10        15        19        24        27
      32        36        41
 0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01 -2.500000000000E-01  0.000000000000E+00
-2.500000000000E-01 -2.500000000000E-01  0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01
 0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00 -2.500000000000E-01 -2.500000000000E-01
 0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00 -2.500000000000E-01  0.000000000000E+00
```

## Example STOR file compress all, graph and coef (astor) ##

This is the default stor format and provides the smallest possible file size based on the number of unique coeficient values in the mesh.

```
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
  08/13 15:40:06 20093-D Linear Diffusion Model (matbld3d_astor)
    2         8        41         1         4
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
1.250000000000E-01  1.250000000000E-01  1.250000000000E-01
    9        13        17        21        25
   29        33        37        41
    1         2         3         5         1
    2         4         6         1         3
    4         7         2         3         4
    8         1         5         6         7
    2         5         6         8         3
    5         7         8         4         6
    7         8
    2         1         1         1         1
    2         1         1         1         2
    1         1         1         1         2
    1         1         2         1         1
    1         1         2         1         1
    1         2         1         1         1
    1         2         0         0         0
    0         0         0         0         0
    0
   10        15        19        24        27
   32        36        41
-2.500000000000E-01  0.000000000000E+00
```
