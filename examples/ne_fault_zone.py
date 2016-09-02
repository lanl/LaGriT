from pylagrit import PyLaGriT
import numpy
lg = PyLaGriT()

# Read in mesh
motet = lg.read('tet_matclr.inp')

# Jim Bridger fault coordinates in feet from John Ziao
cs = [[498000.,381946.,0.],
      [497197.,381946.,0.],
      [494019.,384890.,0.],
      [490326.,386959.,0.],
      [487822.,388599.,0.],
      [486337.,390755.,0.],
      [486337.,392000.,0.]]

# Convert to meters
cs = numpy.array(cs)/3.28

# Create surfaces of fault
ss = []
for p1,p2 in zip(cs[:-1],cs[1:]):
    p3 = p1.copy()
    p3[2] = -4000.
    ss.append(lg.surface_plane(p1,p2,p3))

# Create region by boolean operations of fault surfaces
boolstr = ''
for i,s in enumerate(ss):
    if not i == 0: boolstr += ' and '
    boolstr += 'le '+s.name

r = lg.region_bool(boolstr)

# Create pset from region
p = motet.pset_region(r)

# Change imt value for pset
p.setatt('imt',21)

motet.dump_zone_imt('tet_nefault',21)
