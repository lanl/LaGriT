Getting Started
===============

Installation
------------

The Python module PyLaGriT allows LaGriT to be accessed interactively and in batch mode from Python. 
Examples of PyLaGriT usage are in the PyLaGriT/examples folder. 
To install PyLaGriT on your system, change to the PyLaGriT directory and run `python setup.py install`. 
Alternatively, the PyLaGriT folder can be added to your `PYTHONPATH` environment variable. 
Due to the use of the pexpect Python module, PyLaGriT does not work on Windows. 
It has been tested on Mac and Linux operating systems. 

Since PyLaGriT is simply a frontend for LaGriT, you will need a lagrit executable to use PyLaGriT.
To use the paraview and gmv methods, you will need ParaView (http://www.paraview.org) and GMV (http://www.generalmeshviewer.com) installed, respectively.

To avoid specifying the LaGriT, ParaView, and GMV executable locations, copy the *pylagritrc_example* file:

.. literalinclude:: ../../pylagritrc_example

in the PyLaGriT folder to your home directory or working directory changing its name to either *pylagritrc* or *.pylagritrc* and uncomment and change the path locations.

Simple example
--------------

The following is an example of creating a simple cube shaped mesh.

.. code-block:: python

    # Import PyLaGriT class from pylagrit module
    from pylagrit import PyLaGriT
    
    # Create PyLaGriT object
    # This assumes that pylagritrc is being used so that lagrit_exe option does not need to be specified
    l = PyLaGriT()

    # Create x,y,z arrays for location of points
    x = range(1,5)
    y = range(1,5)
    z = range(1,5)

    # Create mesh object using x,y,z arrays
    m = l.gridder(x,y,z)

    # Connect points
    m.connect()

    # Visualize connected mesh using ParaView
    # This assumes that pylagritrc is being used so that exe option does not need to be specified
    m.paraview()

This will open up ParaView with the mesh loaded:

.. image:: cube.png
