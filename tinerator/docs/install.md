# Installation

While TINerator is a relatively light module, it depends on several
compiled libraries and packages. Consequently, it is recommended that you set up
a [Conda environment](https://conda.io/en/latest/) or [virtual-env](https://virtualenv.pypa.io/en/latest/) for managing TINerator:

```bash
$ conda create --name tinerator python=3.5
$ source activate tinerator
```

Note that you will have to re-run the `source activate` command on each new shell instance.

## Python Requirements

TINerator requires Python 3.4+ and the following Python modules:

| Module     | URL                                                        |
|------------|------------------------------------------------------------|
| RichDEM    | https://richdem.readthedocs.io                             |
| Matplotlib | https://matplotlib.org                                     |
| Numpy      | http://www.numpy.org                                       |
| SciPy      | https://www.scipy.org                                      |
| Rasterio   | https://rasterio.readthedocs.io                            |
| Fiona      | https://fiona.readthedocs.io                               |
| Elevation  | http://elevation.bopen.eu/en/stable/                       |
| PyLaGriT   | https://lanl.github.io/LaGriT/pylagrit/original/index.html |

With the exception of PyLaGriT, all modules will be automatically installed 
when running the `python setup.py install` command in TINerator.

!!! warning
    On macOS, the default clang C/C++ compilers seem to fail on compilation of
    `richdem`. To circumvent this, `export CC; export CXX` to GNU GCC compilers.

## Building LaGriT + Exodus

LaGriT is a mesh generation software suite built by Los Alamos National Laboratories, and it (wrapped by the Python library PyLaGriT) is the 'engine' driving TINerator. 

A LaGriT binary needs to be present on your system before proceeding. You can [directly download](https://github.com/lanl/LaGriT/releases) a pre-built binary, or to build one yourself, run:

```bash
git clone https://github.com/lanl/LaGriT.git && cd LaGriT
make exodus
make static && make test
```

You will find the LaGriT executable in `$(LAGRIT_ROOT_DIR)/src/lagrit`. For more information, please see the [LaGriT installation documentation](https://github.com/lanl/LaGriT/blob/master/documentation/INSTALL.md).

!!! Warning
    LaGriT does not currently support Windows compilation. Windows support is coming soon.

## Installing PyLaGriT

PyLaGriT is a subfolder within the LaGriT repo. 
Navigate to `$(LAGRIT_ROOT_DIR)/PyLaGriT` and run

```bash
python setup.py install
```

Next, create the file `~/.pylagritrc` with the following text:

    lagrit_exe : 'path/to/lagrit/executable'

where `path/to/lagrit/executable` is the path to the recently downloaded/installed LaGriT binary.

For more information, visit the [PyLaGriT installation page](https://lanl.github.io/LaGriT/pylagrit/original/gettingstarted.html#installation).

##  Installing TINerator

TINerator is currently a submodule within a branch on the LaGriT repository.

Navigate to `$(LAGRIT_ROOT_DIR)` and run

```bash
git checkout tinerator
cd tinerator
python setup.py install
```

TINerator should now be installed and ready for use. 

If you experience any difficulty building TINerator, please open an issue on the [LaGriT Issues page](https://github.com/lanl/LaGriT/issues).

## Next Steps

Check out [Examples](tutorials/index.md) to get started, or [read the API](api/index.md) for function and method documentation.