from distutils.core import setup
setup(
    name = "tinerator",
    packages = ["tinerator"],
    version = "0.1.0",
    description = "DEM -> Refined TIN Generation",
    author = "Daniel Livingston",
    author_email = "livingston@lanl.gov",
    url = "http://www.github.com/lanl/tinerator",
    keywords = ["gis", "dem", "tin"],
    classifiers = [
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Development Status :: 4 - Beta",
        "Environment :: Other Environment",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU Library or Lesser General Public License (LGPL)",
        "Operating System :: OS Independent",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
    long_description = """\
Digital Elevation Map to Refined and Extruded TIN
-------------------------------------

This library:
- Downloads a DEM from lat/long coordinates or from a shapefile
- Performs watershed deliniation to determine areas of refinement
- Triangles DEM with refinement along watershed features
- Extrudes DEM to configurable layers

This version requires Python 3 or later; a Python 2 version is available separately.
"""
)