**Using various quality measures to characterize tets as type: sliver, wedge, cap or needle elements.**

**Characteristics**

- **Sliver**: Characterize by small minimum dihedral angle and large maximum dihedral angle but not a large solid angle.
- **Cap**: Characterize by large maximum solid angle.
- **Needle**: Characterize by small min/max edge length ratio but not small minimum dihedral angle.
- **Wedge**: Characterize by small minimum dihedral angle and small min/max edge length ratio.

Characteristic    | Equilateral | Sliver | Cap | Needle | Wedge 
--- | ---- | --- | ---- | --- | ---
Picture | <img height="200" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/quality_tet_equilateral.png"> | <img height="200" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/quality_tet_sliver.png"> | <img height="200" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/quality_tet_cap.png"> | <img height="200" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/quality_tet_needle.png"> | <img height="200" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/quality_tet_wedge.png"> |
Coordinates | 0 0 0 <br> 0 1 1 <br> 1 0 1 <br> 1 1 0 | 1 0 0.1 <br> -1 0 0.1 <br> 0 1 -0.1 <br> 0 -1 -0.1 | 1 0 0 <br> 1 1 0 <br> 0 0 0 <br> 0.75 0.25 0.1 | .1 -.1 0 <br> .1 .1 0 <br> -.1 0 0 <br> 0 0 1 | 1 0 0 <br> -1 0 0 <br> 0 1 0.1 <br> 0 1 -0.1 
Aspect Ratio | 1.0 | 0.2927 | 0.07448 | 0.3429 | 0.2617
Min Dihedral Angle | 7.05288E+01 | 1.59424E+01 | 1.57932E+01 | 5.33585E+01 | 1.14212E+01
Max Dihedral Angle | 7.05288E+01 | 1.57380E+02 | 1.49550E+02 | 8.74394E+01 | 90.0
Max Solid Angle | 3.15863E+01 | 9.26487E+00 | 2.60111E+02 | 5.56182E+01 | 8.19307E+01
Min/Max Edge Length Ratio  | 1.0 | 7.14143E-01 | 2.59808E-01 | 1.98030E-01 | 0.1

**Example: LaGriT input file to identify sliver, cap, needle and wedge type elements.*

    *--* ex_id_tet_types
    *--* Header Begin
    *--* LAGriT Example Identify Sliver, Cap, Needle, Wedge
    * Carl Gable
    * gable -at- lanl -dot- gov
    *
    *
    *--* Create a random point distribution
    *--* Connect them into a tet mesh
    *--*
    *--*----------------------------------------------------
    *--* Header End
    *--* ex_id_tet_types
    *--*
    cmo / create / cmo1 / / / tet
    surface / outer / reflect / box / 0,0,0 / 1,1,1
    region / r1 /  le outer  /
    mregion / m1 /  le outer  /
    createpts / random / xyz / 0.1 / 0,0,0 / 1,1,1
    setpts
    connect
    dump / gmv / output_tets.gmv
    *
    * Compute the various tet quality measures
    *
    * Minimum Dihedral Angle, (degree)
    cmo / addatt / cmo1 / ang_mind / ang_mind
    * Minimum Dihedral Angle, (radian)
    cmo / addatt / cmo1 / ang_minr / ang_minr
    * Maximum Dihedral Angle, (degree)
    cmo / addatt / cmo1 / ang_maxd / ang_maxd
    * Maximum Dihedral Angle, (radian)
    cmo / addatt / cmo1 / ang_maxr / ang_maxr
    * Minimum Solid Angle, (degree)
    cmo / addatt / cmo1 / ang_mind_solid / s_mind
    * Minimum Solid Angle, (radian)
    cmo / addatt / cmo1 / ang_minr_solid / s_minr
    * Maximum Solid Angle, (degree)
    cmo / addatt / cmo1 / ang_maxd_solid / s_maxd
    * Maximum Solid Angle, (radian)
    cmo / addatt / cmo1 / ang_maxr_solid / s_maxr
    * Aspect Ratio
    quality / aspect / y
    * ( minimum edge lenght ) / ( maximum edge length )
    quality / edge_ratio / y
    * Edge lenght minimum
    quality / edge_min / y
    * Edge length maximum
    quality / edge_max / y
    *
    * Identify Slivers
    *
    * Define adjustable parameters to determine cut-off values.
    *
    define / MIN_DIHEDRAL  /  10.0
    define / MAX_DIHEDRAL  / 170.0
    define / MAX_SOLID_ANG_BIG / 170.0
    define / MAX_SOLID_ANG_SMALL / 10.0
    define / EDGE_RATIO    /   0.1
    *
    eltset / e_dih_small / ang_mind / le / MIN_DIHEDRAL
    eltset / e_dih_big   / ang_maxd / ge / MAX_DIHEDRAL
    eltset / e_solid_big / s_maxd   / le / MAX_SOLID_ANG_SMALL
    eltset / e_tmp       / inter / e_dih_small e_dih_big
    eltset / e_sliver    / inter / &
             e_tmp e_solid_big
    *
    * Identify Cap elements
    *
    eltset / e_cap / s_maxd / ge / MAX_SOLID_ANG_BIG
    *
    * Identify Needle elements
    *
    eltset / e_edge_ratio / eratio   / le / EDGE_RATIO
    eltset / e_dih_small  / ang_mind / le / MIN_DIHEDRAL
    eltset / e_needle / not / e_edge_ratio e_dih_small
    *
    * Identify Wedge elements
    *
    eltset / e_wedge / inter / e_edge_ratio e_dih_small
    *
    * Set up some attributes to tag the elements.
    *
    cmo/addatt/cmo1/if_sliv/vint/scalar/nelements/-def-/-def-/-def-/1
    cmo/addatt/cmo1/if_cap/vint/scalar/nelements/-def-/-def-/-def-/1
    cmo/addatt/cmo1/if_ned/vint/scalar/nelements/-def-/-def-/-def-/1
    cmo/addatt/cmo1/if_wed/vint/scalar/nelements/-def-/-def-/-def-/1
    cmo/setatt/cmo1/if_sliv/eltset get e_sliver/ 2
    cmo/setatt/cmo1/if_cap /eltset get e_cap   / 2
    cmo/setatt/cmo1/if_ned /eltset get e_needle/ 2
    cmo/setatt/cmo1/if_wed /eltset get e_wedge / 2


    dump / gmv / tet_types.gmv / cmo1

    * begin compare here
    cmo / status
    cmo / printatt /  / -all- / minmax
    quality
    finish
