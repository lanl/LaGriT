#=====================================================#
#  TINERATOR Dockerfile
#  lanl.github.io/LaGriT
# 
#  Copyright (c) 2019. Triad National Security, LLC.
#  All rights reserved.
#
#  Distributed under the terms of
#  the Modified BSD License.
#=====================================================#

# BASIC USAGE:
# (1) To build: docker build -t tinerator .
# (2) To run: docker run -p 8888:8888 -ti --name tin3 tinerator:latest

ARG BASE_CONTAINER=jupyter/minimal-notebook
FROM $BASE_CONTAINER

LABEL maintainer="Daniel Livingston <livingston@lanl.gov>"

USER root

RUN apt-get update && \
    apt-get install -y --no-install-recommends gfortran libz-dev m4 bison cmake

USER $NB_UID

# Build dependencies
RUN conda install --quiet --yes \
    'conda-forge::blas=*=openblas' \
    'ipywidgets=7.4*' \
    'matplotlib=3.0*' \
    'vtk' \
    'vtki' \
    'rasterio' \
    'fiona' \
    'geopandas' \
    'scikit-fmm' && \
    pip install richdem && \
    conda remove --quiet --yes --force qt pyqt && \
    conda clean -tipsy && \
    # Activate ipywidgets extension in the environment that runs the notebook server
    jupyter nbextension enable --py widgetsnbextension --sys-prefix && \
    # Also activate ipywidgets extension for JupyterLab
    # Check this URL for most recent compatibilities
    # https://github.com/jupyter-widgets/ipywidgets/tree/master/packages/jupyterlab-manager
    jupyter labextension install @jupyter-widgets/jupyterlab-manager@^0.38.1 && \
    jupyter labextension install jupyterlab_bokeh@0.6.3 && \
    npm cache clean --force && \
    rm -rf $CONDA_DIR/share/jupyter/lab/staging && \
    rm -rf /home/$NB_USER/.cache/yarn && \
    rm -rf /home/$NB_USER/.node-gyp && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER

# Build LaGriT and PyLaGriT
RUN git clone --depth 1 http://github.com/lanl/LaGriT.git LaGriT && \
    cd LaGriT && \
    make exodus && make static && \
    echo "lagrit_exe : \"`pwd`/src/lagrit\"" >> ~/.pylagritrc && \
    cd PyLaGriT && \
    python setup.py install && \
    cd ../..

# Build TINerator
RUN git clone http://github.com/lanl/LaGriT.git tinerator && \
    cd tinerator && git checkout tinerator && \
    python setup.py install


# Import matplotlib the first time to build the font cache.
ENV XDG_CACHE_HOME /home/$NB_USER/.cache/
RUN MPLBACKEND=Agg python -c "import matplotlib.pyplot" && \
    fix-permissions /home/$NB_USER

USER $NB_UID
