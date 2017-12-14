using PyCall
@pyimport pylagrit

l = pylagrit.PyLaGriT(lagrit_exe="lagrit")

# Create mesh object 'm'
m = l[:create](elem_type="triplane")
m[:read]("data.inp")

# Set imt to 1 for all points
m[:setatt]("imt",1)
m[:minmax_xyz]()

# Connect mesh
m[:connect]()

# Lift the triangulation by assigning the Z coordinate to the data value (value)
m[:copyatt]("value","zic")
m[:minmax_xyz]()

# Dump surface to vis file
m[:dump]("tmp_tri_surf.inp")

# Extrude mesh to 0
mprism = m[:extrude](0.1,offset_type="min",return_type="volume", direction=[0.,0.,-1.])

# Diagnostic
l[:sendline]("quality / volume")

# Add e_volume attribute of type "volume"
mprism[:addatt]("e_volume", keyword="volume")

# Calculate volume
mprism[:math]("sum","volume_sum",attsrc="e_volume")

# Dump surface to vis file
mprism[:dump]("tmp_prism.inp")

# Just to check the math do this again and assign all the 'values' to 1.0 and see if we get the expected result.
mprism[:delete]()
m[:setatt]("value", 1.0)
m[:copyatt]("value","zic")
mprism = m[:extrude](0.1,offset_type="min",return_type="volume", direction=[0.,0.,-1.])
l[:sendline]("quality / volume")
mprism[:addatt]("e_volume", keyword="volume")
mprism[:math]("sum","volume_sum",attsrc="e_volume")

