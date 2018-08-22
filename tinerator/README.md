![logo](docs/source/logo.png)

[![][gitlab-img]][gitlab-url] [![][codecov-img]][codecov-url]

[gitlab-img]: https://gitlab.com/daniellivingston/tinerator/badges/master/pipeline.svg
[gitlab-url]: https://gitlab.com/TensorFactorization/NTFk.jl/pipelines

[codecov-img]: https://gitlab.com/daniellivingston/tinerator/badges/master/coverage.svg
[codecov-url]: https://gitlab.com/daniellivingston/tinerator

## Installation ##

After closing the repository, navigate to the root directory and run

    python setup.py install
    
It is recommended that you do this in `conda` or `virtual-env`, particularily due to the strong version depedencies on gdal.

On macOS, the default clang C/C++ compilers seem to fail on compilation of `richdem`. To circumvent this, `export CC; export CXX` to GNU GCC compilers.

## Release Notes ##

* 0.1.1 (08/12/18) - Minor bug fixes and beginning of documentation.
* 0.1.0 (08/10/18) - Initial release. 


## Milestones ##

* 0.2.0 (Projected: by 08/16/18)
   - Improved refinement / triangulation through LaGriT's engine
   - Multi-layer extrusion capabilities
   - Exodus output support
   - GitLab CI integration, code coverage
   - unit test suite
   - Improved documentation
   - examples folder added

* 0.3.0 (Projected: by 08/24/18)
   - Simple GUI powered by wxPython, compiled to multi-platform executables using `Pyinstaller` or `py2exe`.