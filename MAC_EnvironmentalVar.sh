#
#	Set Up Mac Specific Environmental Variables
#
#=====================================================

fortran_var=$(find /opt/local/bin/gfortran-mp-*)
cc_var=$(find /opt/local/bin/gcc-mp-*)
cpp_var=$(find /opt/local/bin/g++-mp-*)

echo "Environmental Variables set"

FC="$fortran_var"
find ${FC}
export FC="$fortran_var"

CC="$cc_var"
find ${CC}
export CC="$cc_var"

CXX="$cpp_var"
find ${CXX}
export CXX="$cpp_var"
