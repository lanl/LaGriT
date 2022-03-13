Version 3.X LaGriT libraries and executables
with linux 64 bit compiled with gfortran 4.5 or greater -------


==============================================================
Version 3.106 and greater LaGriT uses these external libraries:

   Exodus II 6.09 
   HDF5 version 1.8.6
   netcdf-4.1.3
   zlib 1.2.5

Contact: Greg Sjaardema, Sandia National Laboratories, gdsjaar@sandia.gov
http://sourceforge.net/projects/exodusii/files/

  $DIR/exodusii/6.09/gcc-4.8.2-serial/lib
  libexodus.so  libexoIIv2for.so 
  (static libs should be libexodus.a  libexoIIv2for.a) 
  copy include files to src (or add -I to the link command)
    exodusII.h (for C calls)  
    exodusII.inc (for Fortran calls) 
    exodusII_int.h  exodusII_int.inc

NOTE: exodusII.inc is incomplete (Greg will fix in his version)
Add these lines if it does not exist:
       integer EX_ELEM_SET
       parameter (EX_ELEM_SET  = 10)    

BUILD Ubuntu shared EXECUTABLE with Exodus:

#### ubuntu with modules ###  USING BASH instead of TCSH ######
# GNU Fortran (Ubuntu 4.8.2-19ubuntu1) 4.8.2
# using ExodusII 6.09 shared libraries

# load modules
module load exodusii/6.09/gcc-4.8.2-serial
module list
rm *.o
rm *.mod
make COPT=-g lib

### link code and libraries to build executable
# leave off -static option for the .so shared libraries:
gfortran -g -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit lagrit_main.o lagrit_fdate.o  lagrit_ulin64_g_gf4.8.a /n/swdev/LAGRIT/VERS_3.103_015_MAR/lg_util/src/util_ulin64_g_gf4.8.a -L/n/swdev/packages/Ubuntu-14.04-x86_64/exodusii/6.09/gcc-4.8.2-serial/lib -lexodus -lexoIIv2for -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

# BUILD DONE

==============================================================
If do not have and do not need the ExodusII and netCDF libraries.

BUILD Ubuntu shared EXECUTABLE with No Exodus:

LaGriT V3.106 supports ExodusII file format output (which sits on top of netCDF).
This means that if you want to compile your own code you have to build the ExodusII and netCDF libraries yourself. We do not distribute those packages. A quick and dirty option to compile without these libraries would to just take the code that calls those libraries and replace dumpexodusII.f it with:

     subroutine dumpexodusII(ifile)
     return
     end

==============================================================
STEPS to COMPILE LAGRIT V3
# OSTYPE = Linux gfortran 4.5 for 64 bit 
# build -g debug version for use with debuggers such as gdb


STEP 1 
Compile lg_util library (LaGriT memory management routines)

In directory lg_util/src

CAUTION: check mm2000.h that BYTES_PER_INT = 8
if not then Remove and re-make mm2000.h from mm2000_header.F
    rm -f mm2000.h
    gcc -g -E -m64 -Dlinx64 mm2000_header.F -o mm2000.h
    cp -p mm2000.h objects_lin64_g_gfort4.5/

Note that 64 bit compile is now default.

make MOPT=64 COPT=-g install
or 
make COPT=-g 



STEP 2
Compile LaGriT library and build executable example for Linux 64 bit
The script 'buildbin.scr' can be used to compile and link libraries
This is similar to the 'go' script we use which has explicit compile commands
 
cp lagrit_lin64.h lagrit.h
cp machine_m64.h machine.h 

make COPT=-g lib


STEP 3
Compile the drivers, build the executable and link to needed libraries

gfortran  -g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_main.o lagrit_main.f
gfortran  -g  -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -c -o lagrit_fdate.o lagrit_fdate.f

Link to local exodus libraries:

gfortran -g -fcray-pointer -fdefault-integer-8 -m64 -Dlinx64 -static -fno-sign-zero -o lagrit_lin64_o_gfort4.5 lagrit_main.o lagrit_fdate.o ./lagrit_lin64_g_gfort4.5.a /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/lg_util/lib/util_lin64_g_gfort4.5.a -L/n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/exodus/lin64/lib -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

or 
load exodus modules and build shared version:

module load exodusii/6.09/gcc-4.8.2-serial

gfortran -g -Dlinx64 -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit lagrit_main.o lagrit_fdate.o  lagrit_ulin64_g_gf4.8.a /n/swdev/LAGRIT/VERS_3.103_015_MAR/lg_util/src/util_ulin64_g_gf4.8.a -L/n/swdev/packages/Ubuntu-14.04-x86_64/exodusii/6.09/gcc-4.8.2-serial/lib -lexodus -lexoIIv2for -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++



==============================================================

Running executable lagrit_lin64_o_gfort4.5

% lagrit_lin64_o_gfort4.5
 
*               * * * * * * * * * * * * * * * * * * * * * * * *                 
*               *                                             *                 
*               *    Program:  LaGriT V3.001   Linux m64      *                 
*               *    date_compile: 2011/08/01  gfort 4.5      *                 
*               *    Run Time: 2011/Aug 31  10:14:12          *                 
*               *    Manual:   http://lagrit.lanl.gov         *                 
*               *                                             *                 
*               * * * * * * * * * * * * * * * * * * * * * * * *                 
 
                               -----oOo-----                                    
LaGriT Copyright: This program was prepared by Los Alamos National Security, LLC
at Los Alamos National Laboratory (LANL) under contract No. DE-AC52-06NA25396   
with the U.S. Department of Energy (DOE). All rights in the program are reserved
by the DOE and Los Alamos National Security, LLC. Permission is granted to the  
public to copy and use this software without charge, provided that this Notice  
and any statement of authorship are reproduced on all copies. Neither the       
U.S. Government nor LANS makes any warranty, express or implied, or assumes     
any liability or responsibility for the use of this software.                   
                               -----oOo-----                                    
 
 
Output log file: outx3dgen                                                      
Command log file: logx3dgen                                                     
 
 Enter a command
memory print

--------

MEMORY SIZES : 
 Sizeof char    (type 3) =  1 bytes      Sizeof long        =   8 bytes
 Sizeof real*8  (type 2) =  8 bytes      Sizeof pointer     =   8 bytes
 Sizeof integer (type 1) =  4 bytes      Sizeof INT_PTRSIZE =   8 bytes


INDEX         LENGTH    TYPE     ADDRESS     NAME                           PARTITION
   2                10   1       102267104 global_type                      global_lg 
   3                10   1       102267296 global_index                     global_lg
   1                10   3       102269056 global_name                      global_lg
   4                20   1       102329536 global_integer                   global_lg
   5                20   2       102329808 global_real                      global_lg
   6                20   3       102330080 global_character                 global_lg
   7                10   3       102330832 geom_names                       geom_lg 
   8                80   1       102331264 geom_info                        geom_lg
   9                 8   3       102332016 defcmo_attparam_names            define_cmo_lg 
  10                10   3       102332384 cmo_names                        define_cmo_lg
  11                10   1       102332816 cmo_natts                        define_cmo_lg
  12               455   3       102333008 cmo_attlist                      default_cmo_lg
  13                65   3       102347680 cmo_attparam_cdefault            default_cmo_lg
  14                65   1       102349872 cmo_attparam_idefault            default_cmo_lg
  15                65   2       102350496 cmo_attparam_rdefault            default_cmo_lg
  16              2560   1       102351120 cmd_stack                        initlagrit   
  17              1280   1       102371712 definition                       initlagrit  

Total BYTES =                51456   Total MEGABYTES =   51.456E-03
--------

==============================================

