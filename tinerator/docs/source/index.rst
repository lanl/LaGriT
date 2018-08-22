.. image:: logo.png
   :scale: 50 %
   :alt: tinerator
   :align: center

Getting Started
===============

Introduction
------------

TINerator is a tool for the fast creation of extruded meshes from DEM and GIS data. Developed at Los Alamos National Laboratory to aid in hydrogeological simulations, TINerator .

TINerator allows a user to define a bounding box of latitude/longitude coordinates, a shapefile, or a local DEM, and generate a triplane or extruded mesh. 

The mesh will have the topology of the DEM, along with user-defined material IDs and depths for stacked layers. Further, TINerator performs watershed delination on the defined DEM and refines the mesh's elements around the feature to a user-defined length scale.

TINerator comes with a host of 2D and 3D visualization functions, allowing the user to view the status of the mesh at every step in the workflow. In addition, there are geometrical tools for removing triangles outside of a polygon, generating quality analytics on the mesh, adding cell- and node-based attributes to a mesh, and much more.

Simple Example
--------------

The following is a simple script which builds a layered, stacked mesh from a local DEM:

.. code:: python

    from tinerator import *

    # First, load the DEM from a physical file.
    my_dem = loadDEM("data/dem.asc")

    # Threshold the feature at 4500
    my_dem.watershedDeliniation(threshold=4500.)

    # Then, calculate the distance field.
    my_dem.calculateDistanceField(accumulation_threshold=4500.)

    # Let's visualize the DEM and distance field.
    plot_raster(my_dem.dem)
    plot_raster(my_dem.distance_field,hillshade_image=False)

    # Define the layers and corresponding material ids
    layers = (0.1,0.3,0.6,8.0,21.0)
    matids = (1,2,3,4,5)

    # Generate a perimeter around the DEM, spaced at 10 meters.
    my_dem.generateBoundary(10.)

    # Generate and save a stacked TIN
    my_dem.generateStackedTIN("test_extruded_mesh.inp",layers,matids=matids)

Installation
------------

To build TINerator, simply run:

.. code:: bash

   python setup.py install

Additional packages are required, if not already installed. These include PyLaGriT and RichDEM. RichDEM can be built by calling:

.. code:: bash

   pip install richdem

For instructions on building PyLaGriT, follow the directions on the `PyLaGriT 
<https://lanl.github.io/LaGriT/pylagrit/original/gettingstarted.html#installation>`_.

It is recommended that one uses :code:`conda` or :code:`virtual-env` when building, particularily if you have a system build of GDAL. This circumvents versioning errors. 

Source Documentation
--------------------

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   DEM Class and Methods <autodoc_dem.rst>
   Additional Functions <functions.rst>


