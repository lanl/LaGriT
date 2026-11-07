C     poi_octree_opts.h -- the h lookup options of createpts /
C     poisson_disk, set by poi_parse_options (poi_driver_octree.f):
C
C        h_lookup        hex_grid | octree      if_poi_octree 0 | 1
C                        (-1 until chosen: poi_3d_choose_lookup)
C        octree_k_factor real > 0               poi_oct_k
C        octree_batch    integer > 0            poi_oct_batch
C        octree_dump     file name              poi_oct_dump,
C                                               if_poi_oct_dump
C
C     if_poi_oct_key is 1 when any of them was given (2d_polygon
C     refuses them).
C
      integer if_poi_octree,if_poi_oct_key,poi_oct_batch,
     &        if_poi_oct_dump
      real*8  poi_oct_k
      character*32 poi_oct_dump
      common /poi_oct_i/ if_poi_octree,if_poi_oct_key,poi_oct_batch,
     &        if_poi_oct_dump
      common /poi_oct_r/ poi_oct_k
      common /poi_oct_c/ poi_oct_dump
C
C     defaults. A batch of octree vertices in mo_poi_h_field costs up
C     to about 250 bytes each while the user's .mlgi runs (x, y, z, the
C     standard attributes, h_field_att, the .mlgi's own arrays and
C     temporaries), so 8 million vertices is about 2 GB. Below that a
C     whole refinement pass goes to the .mlgi at once.
      real*8  POI_OCT_K_DEFAULT
      parameter (POI_OCT_K_DEFAULT = 0.75d0)
      integer POI_OCT_BATCH_DEFAULT
      parameter (POI_OCT_BATCH_DEFAULT = 8000000)
