################################################################################
# This example shows how to merge multiple mesh objects together.              #
################################################################################

#To use pylagrit, import the module.
import pylagrit
import numpy

#Instantiate the lagrit object.
lg = pylagrit.PyLaGriT()

# Create list with mesh object as first element
dxyz = numpy.array([0.25]*3)
mins = numpy.array([0.]*3)
maxs = numpy.array([1.]*3)
ms = [lg.createpts_dxyz(dxyz,mins,maxs,'tet',connect=True)]

# Create three new mesh objects, each one directly above the other
for i in range(3):
    ms.append(ms[-1].copy())
    ms[-1].trans(ms[-1].mins,ms[-1].mins+numpy.array([0.,0.,1.]))

# Merge list of mesh objects and clean up
mo_merge = lg.merge(ms)
for mo in ms: mo.delete()
mo_merge.rmpoint_compress(filter_bool=True,resetpts_itp=True)

mo_merge.paraview(filename='mo_merge.inp')


