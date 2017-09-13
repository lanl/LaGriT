**FEHMN Stor File Format**

The sparse matrix data file is both an input and an output file the FEHM application uses for storing or reading geometric coefficients associated with a finite element mesh. The sparse matrix data file is read during FEHM initialization if being used for input. The generated name is of the form filename.stor and can be written in ascii or binary (unformatted) mode. 

The stor file is written with "blocks" of information data in the following format.

1. ASCII Header consisting of 2 lines giving code version information, date, and time.

2. One line of integer numbers defining sparse matrix parameters, including the number of written coeficients and the number of nodes.

3. The volume associated with each node. In general this is the volume of the Voronoi polygon associated with the node.

4. List indicating the number of entries for each row of the matrix. If a node in a mesh has N edge connections, this number is N+1.

5. The connectivity list. Row i is a list of all nodes j1, j2, j3... connected to node i. The list contains the matrix diagonal entry, i.

6. Pointers or index numbers into the last block containing coefficient values.

7. Geometric area coefficients. In general this is the area of each face of the Voronoi polygon associate with node i, face j


1. ASCII Header
Title Line: The first line consists of a character string of 72 or less characters. 

Date Line: The second line contains a character string of 72 or less characters. This line contains a time stamp of when the file was written. It also contains information stating whether the file is binary or ASCII (text). If it is ASCII, then characters 10-16 should be 'asci.' If it is binary, then characters 10-16 should be 'ieee'. 

Characters 14-17 should be labled as one of: r4i4 r4i8 r8i4 r8i8 

This signifies the number of bytes used for reals 'r' and integers 'i'. E.g., r8i4 means 8 byte reals, 4 byte integer

2. Matrix Parameters
The Parameter line contains 4 or 5 integer values: NUM_WRITTEN_COEFS, NEQ, NCOEF+NEQ+1, NUM_AREA_COEF, and [NCON_MAX}. [] denotes the optional parameter: 

NUM_WRITTEN_COEFS: is the number of floating point entries written in the .stor file. If compression is not used, then this is equal to the number of connections in the grid. This value is contained in the variable NCOEF, which is used to allocate integer pointers. 

NEQ: signifies the number of equations. This is equivalent to the number of nodes in a triangular or tetrahedral grid. 

NCOEF+NEQ+1: is a value used to allocate memory in FEHM for the integer pointers. 

NUM_AREA_COEF: Can be (1,3, or 4) number of area coefficients. The area/distance coefficients are the area of a Voronoi face divided by the Delaunay edge length. This coefficient is either written as a scalar (1), a vector area (3), or both vector and scalar areas are written (4). 

NCON_MAX: is an optional parameter indicating the maximum number of connections to a single node in the entire mesh. This is used for memory allocation. If omitted, FEHM will default to a value that might cause problems in highly unstructured meshes. 

[A note on delimiters: In the ASCII version of the .stor format, the first three lines are parsed for end of line characters. That is, the Title Line, Date Line, and Matrix Parameters Line must occur in the first three lines. Afterwards, data is delimited by whitespace and an occur in free form. Further, no attempt to find or report errors in .stor files is made in FEHM.] 

3. Voronoi Volumes
This block has NEQ floating point values which are the Voronoi Volumes associated with each node 1 through NEQ. 

4. Count for Each Row
The purpose of this block is to indicate the number of specified entries each row of the matrix contains. (All unspecified entries are assumed to be 0). 

The representation is circuitous. There are a total of NEQ+1 entries. The first entry is the value NEQ+1. The second entry is the value NEQ+1+the number of connections the in the first row. The third entry is value of the second entry + the number of connections in the second row. In general, the ith entry (for i1) is the i-1th entry + the number of specified entries in the row i-1. In this way, the difference between the i+1st and the ith value indicates how many entries row i has in the matrix. 

For example, suppose we had a 3x3 matrix with 5 entries we wished to explicitly represent. The .stor file at representing the matrix would contain 4 entries in this block (i.e., NEQ+1=4). Suppose the first row of this matrix has 3 entries, the second row contains 1 entry, and the third row contains one entry. There would be 4 integer values given to represent this information:
5. Row Entries
The purpose of the next block is to state which entries of the matrix are explicitly represented. Since we know the number of entries per row, (given above), we can do this by stating which columns of each row are represented. For example, suppose in our 3x3 matrix example, we saw the following entries in this block: 

1 2 3 
2 3 

This would indicate: The first row has specified entries in the first, second, and third columns (i.e, entries (1,1), (1,2) and (1,3) are specified ; the second row has an entry specified in the second column entry (i.e., (2,2)); the third row has an entry specified in the third column (i.e. (3,3)). 

There are a total of NCOEF such values. 

6. Indices into Coefficient List
This block contains NCOEF integer pointers to entries to the Floating Point Values Block (described below.) The actual values of a matrix entry can be repeated many times, particularly in a matrix arising from the discretization of a structured mesh. Because floating point values require more storage than integers, we have set up the following data structure to allow compression: 

View the floating point values as two dimensional array of dimensions NUM_WRITTEN_COEFS by NUM_AREA_COEF. 

To represent a matrix entry, give the index of value contained in the floating point values block here. If a 0 is given, then the writer of the .stor file has explicitly stated he wants the value to be 0.0. 

Following the NCOEF integer pointers are: 1) NEQ+1 0's for padding and 2) a set of NEQ+1 integer pointers stating which pointers correspond to diagonal elements. 

7. Geometric Area Coefficient Values
This block contains NUM_WRITTEN_COEFS*NUM_AREA_COEF floating point values. 

NUM_AREA_COEF = 1 The scalar area for each connection a(i=1,k). 

NUM_AREA_COEF = 3 The vector area for each connection in the order a_x(i=1,k), a_y(i=1,k), a_z(i=1,k). 

NUM_AREA_COEF = 4 The vector areas first followed by the scalar area. Note that a(i) = sqrt(a_x**2 + a_y**2 + a_z**2) 

**ASCII Stor file examples for 2x2x2 tetrahedral mesh**.

STOR File example nstor: No compression.

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
            41        42        43        44        45
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

STOR File example cstor: coef compression on, graph compression off.
    
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



STOR File example gstor: coef compression off, graph compression on.
     
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


STOR File example astor: Compress all, graph compression on and coef compression on.
 
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
