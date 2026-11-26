C
C     poi_hfield.h: createpts / poisson_disk with an hfield (keyword
C     hfield, NAME) instead of a control file.
C
C     ipoi_hfield     1 when the command gave hfield, NAME (the name
C                     is carried where the control file name goes)
C     hpoi_hfloor_min the command's h_spacing_scalar: the floor of the
C                     hfield is raised to it (a WARNING says so)
C
      integer ipoi_hfield
      real*8 hpoi_hfloor_min
      common /poi_hfield_c/ hpoi_hfloor_min,ipoi_hfield
      save /poi_hfield_c/
