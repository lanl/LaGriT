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
SHELL ["/bin/bash", "-c"] 

ENV APP_PATH=/home/jovyan/
WORKDIR $APP_PATH
RUN mkdir bin

LABEL maintainer="Daniel Livingston <livingston@lanl.gov>"

USER root

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    gfortran libz-dev m4 bison cmake \
    libgl1-mesa-dev xvfb

USER $NB_UID

# Build dependencies
RUN conda install --quiet --yes \
    'conda-forge::blas=*=openblas' \
    'ipywidgets=7.4*' \
    'matplotlib=3.0*' \
    'vtk' \
    'rasterio' \
    'fiona' \
    'geopandas' \
    'scikit-image' \
    'panel' \
    'scikit-fmm' && \
    pip install richdem && \
    ##conda remove --quiet --yes --force qt pyqt && \
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

# Install JupyterLab
RUN pip install jupyterlab

# Install dev version of PyVista
RUN git clone https://github.com/pyvista/pyvista.git && \
    cd pyvista && \
    python setup.py install

WORKDIR $APP_PATH
RUN rm -Rf pyvista

# Build LaGriT and PyLaGriT
RUN git clone --depth 1 https://github.com/lanl/LaGriT.git LaGriT && \
    cd LaGriT && \
    make exodus && make static && \
    echo "lagrit_exe : \"$APP_PATH/bin/lagrit\"" >> ~/.pylagritrc && \
    cd PyLaGriT && \
    python setup.py install && \
    cp ../src/lagrit $APP_PATH/bin/lagrit

WORKDIR $APP_PATH
RUN rm -Rf LaGriT

# Build TINerator
RUN git clone https://github.com/lanl/LaGriT.git tinerator && \
    cd tinerator && git checkout tinerator && \
    python setup.py install

WORKDIR $APP_PATH
RUN cp -r tinerator/examples/. examples/
RUN cp tinerator/README.md . && cp tinerator/LICENSE.md .
RUN rm -Rf tinerator

# Import matplotlib the first time to build the font cache.
ENV XDG_CACHE_HOME /home/$NB_USER/.cache/
RUN MPLBACKEND=Agg python -c "import matplotlib.pyplot" && \
    fix-permissions /home/$NB_USER

USER $NB_UID
