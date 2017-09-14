Getting Started
===============

Installation
------------

The Python module PyLaGriT allows LaGriT to be accessed interactively and in batch mode from Python. 
To install PyLaGriT on your system, change to the PyLaGriT directory and run ``python setup.py install``. 
Alternatively, the PyLaGriT folder can be added to your **PYTHONPATH** environment variable. 
Due to the use of the pexpect Python module, PyLaGriT does not work on Windows. 
It has been tested on Mac and Linux operating systems. 

Since PyLaGriT is simply a frontend for LaGriT, you will need a lagrit executable to use PyLaGriT.
To use the paraview and gmv methods, you will need ParaView (http://www.paraview.org) and GMV (http://www.generalmeshviewer.com) installed, respectively.

To avoid specifying the LaGriT, ParaView, and GMV executable locations, copy the **pylagritrc_example** file:

.. literalinclude:: ../../pylagritrc_example

in the PyLaGriT folder to your home directory or working directory changing its name to either **pylagritrc** or **.pylagritrc** and uncomment and change the path locations.

Simple example
--------------

The following is a PyLaGriT script for creating a simple cube shaped mesh using the :func:`gridder <pylagrit.PyLaGriT.gridder>` and `connect <pylagrit.PyLaGriT.connect>` methods:

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

The script can be pasted line by line into a Python or IPython terminal, or saved in a file (e.g., ``script.py``) and run on the command line as ``python script.py``.

This will open up ParaView with the mesh loaded:

.. image:: cube.png

Next steps
----------

Additional example PyLaGriT scripts are provided in the PyLaGriT/examples folder and in the :ref:`class_docs`. 

Combining LaGriT and Python functionality
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An example of using PyLaGriT to facilitate looping over LaGriT functionality is described in the :func:`merge method <pylagrit.PyLaGriT.merge>` documentation, where a mesh is successively copied, translated and merged into a larger mesh.
Other examples of usage are provided for other methods and more will be added in the future.

User defined error checking
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since PyLaGriT stores the LaGriT terminal output from each command in the PyLaGriT object attribute ``before``, user-defined error checking can be easily implemented.
The example provided with the :func:`refine method <pylagrit.EltSet.refine>` demonstrates this by checking for the string `The mesh is complete but could not include all points.` in the LaGriT output using ``lg.before``. 
In this example, an error message is printed to the screen and the script is aborted.
