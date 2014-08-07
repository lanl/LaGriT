################################################################################
# This example shows how to merge multiple mesh objects together.              #
################################################################################

#To use pylagrit, import the module.
import pylagrit

#Instantiate the lagrit object.
lg = pylagrit.PyLaGriT()

#Create mesh objects.
mo1 = lg.create()
mo2 = lg.create()
mo3 = lg.create()
mo4 = lg.create()

#Merge mesh objects into object mo_merged.
mo_merged1 = lg.merge(mo1, mo2)
mo_merged2 = lg.merge(mo1, mo2, mo3)
mo_merged3 = lg.merge(mo1, mo2, mo3, mo4)



