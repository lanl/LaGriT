Issue with new mac compilers result in precision differences with very small numbers
This causes failures in the test comparisons between mac runs and reference

Not sure this is an issue for users, will check for ways to avoid this difference.

==> out_linux_3x4.stor <==
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
 Thu Dec 21 11:15:21 20233-D Linear Diffusion Model (matbld3d_astor)
         8        24       145         1         7
...
 -7.500000000000E-01 -3.750000000000E-01 -3.333333333333E-01 -1.666666666667E-01 -8.333333333333E-02
 -4.166666666667E-02 -4.618806972100E-17 -1.671098990551E-17

==> out_mac_3x4.stor <==
fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients
 Thu Dec 21 11:21:38 20233-D Linear Diffusion Model (matbld3d_astor)
         7        24       143         1         6
...
 -7.500000000000E-01 -3.750000000000E-01 -3.333333333333E-01 -1.666666666667E-01 -8.333333333333E-02
 -4.166666666667E-02 -1.515940605973E-17

