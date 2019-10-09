import vtk
import pyvista
import numpy as np

from copy import deepcopy
from matplotlib import pyplot as plt
from matplotlib.colors import LogNorm
from mpl_toolkits.axes_grid1 import make_axes_locatable

import tinerator.config as cfg
import tinerator.dump as dump
import tinerator.utilities as util
import tinerator.watershed_deliniation as delin
import tinerator.facesets as fs_lib


# Custom color map
try:
    import pycpt
    topocmap = pycpt.load.cmap_from_cptcity_url('wkp/schwarzwald/wiki-schwarzwald-cont.cpt')
except:
    topocmap = 'Spectral_r'


# Jupyter notebook interactive widgets
try:
    from ipywidgets import interact,interactive,fixed,interact_manual
    from ipywidgets import FloatSlider
    import ipywidgets as widgets
except NameError:
    cfg.IN_NOTEBOOK = False


# -- helper functions ------------- #

def __apply_grid_to_axis(axis):
    axis.set_facecolor('#EAEAF1')
    axis.grid('on',zorder=0,color='white')

def _hillshade(array, azimuth, angle_altitude):

    # Source: http://geoexamples.blogspot.com.br/2014/03/shaded-relief-images-using-gdal-python.html

    x, y = np.gradient(array)
    slope = np.pi/2. - np.arctan(np.sqrt(x*x + y*y))
    aspect = np.arctan2(-x, y)
    azimuthrad = azimuth*np.pi / 180.
    altituderad = angle_altitude*np.pi / 180.

    shaded = np.sin(altituderad) * np.sin(slope) \
             + np.cos(altituderad) * np.cos(slope) \
             * np.cos(azimuthrad - aspect)

    return 255*(shaded + 1)/2

def _plot_raster(dem_array,
                title=None,
                xlabel=None,
                ylabel=None,
                extent=[],
                hillshade_image=True,
                show_plot=True,
                plot_out=None):
    '''
    Beautifully plots a DEM.

    Function shamelessly stolen from GeologyAndPython.com
    <http://geologyandpython.com/dem-processing.html>
    '''

    if not extent:
        extent = (0,np.shape(dem_array)[1],0,np.shape(dem_array)[0])

    vmin,vmax = np.nanmin(dem_array),np.nanmax(dem_array)

    fig = plt.figure(figsize=(12, 8))
    ax = fig.add_subplot(111)

    __apply_grid_to_axis(ax)

    if hillshade_image:
        ax.matshow(_hillshade(dem_array, 30, 30),
            extent=extent, cmap='Greys', alpha=.5,
            zorder=10, vmin=vmin, vmax=vmax)

    try:
        cax = ax.contourf(dem_array,
                          np.arange(vmin,vmax,10),
                          extent=extent, 
                          cmap=topocmap,
                          vmin=vmin,
                          vmax=vmax,
                          origin='image',
                          zorder=9)
    except ValueError:
        cax = ax.imshow(dem_array,
                        zorder=9,
                        extent=extent,
                        vmin=vmin,
                        vmax=vmax,
                        cmap=topocmap)

    cbar = fig.colorbar(cax, ax=ax)
    cbar.set_label('Elevation (m)',rotation=270)

    if title is not None:
        plt.title(title)
    if xlabel is not None:
        plt.xlabel(xlabel)
    if ylabel is not None:
        plt.ylabel(ylabel)
    if show_plot:
        plt.show()
    if plot_out is not None:
        fig.savefig(plot_out)

# -- 2D visualization ------------- #

def plot_dem(dem_object,hillshade:bool=False,plot_out:str=None):
    '''
    Plots the DEM corresponding to a tinerator.DEM object.

    # Arguments
    dem_object (tinerator.DEM): A DEM object to plot
    hillshade (bool): use hillshading on plot
    plot_out (str): filepath to save plot
    '''

    cfg.log.info('Plotting DEM')

    dem = deepcopy(dem_object.dem)

    if dem_object.mask is not None:
        try:
            dem[dem_object.mask] = np.nan
        except ValueError:
            dem = dem.astype(float)
            dem[dem_object.mask] = np.nan

    extent = dem_object.extent
    _plot_raster(dem,
                 plot_out=plot_out,
                 title="DEM",
                 extent=extent,
                 xlabel="latitude",
                 ylabel="longitude",
                 hillshade_image=hillshade)


def preview_accumulation(dem_object,method:str='D8'):
    '''
    Preview the accumulation matrix on a DEM.
    Where this is useful is to preview the accumulation level
    at which you wish to capture a feature.

    # Arguments
    dem_object (tinerator.DEM): A DEM class instance
    method (str): Flow calculation method
    at_threshold

    # Returns
    The generated accumulation matrix
    '''


    # Perform delin. and plot
    def __show_feature_at_threshold(dem_object,threshold):
        dem_object.watershed_delineation(threshold,method=method)
        plot_feature(dem_object)

    dem_copy = deepcopy(dem_object.dem)

    accum = delin.watershedDelineation(dem_copy,method=method)

    extent = dem_object.extent

    f = plt.figure(figsize=(12,8))
    ax = f.add_subplot(111)

    f.suptitle('Accumulation Matrix\nMethod: %s' % (method))
    divider = make_axes_locatable(ax)
    cax = divider.append_axes('right', size='5%', pad=0.05)

    im = ax.imshow(accum,
                   norm=LogNorm(vmin=0.01,vmax=np.max(accum)),
                   extent=extent)

    cbar = f.colorbar(im,cax=cax,orientation='vertical')
    cbar.set_label('Accumulation',rotation=270)
    plt.show()

    return accum

def plot_feature(dem_object):
    '''
    Displays the feature captured by performing watershed delination

    # Arguments
    dem_object (tinerator.DEM): A DEM class instance
    '''

    if dem_object.feature is None:
        raise ValueError('DEM has no feature - watershed delination must be run')

    extent = dem_object.extent

    f = plt.figure(figsize=(12,8))
    ax = f.add_subplot(111)

    f.suptitle('Watershed Delineation (Feature Capture)')
    divider = make_axes_locatable(ax)
    cax = divider.append_axes('right', size='5%', pad=0.05)
    
    im = ax.imshow(dem_object.accumulation_matrix,
                   norm=LogNorm(vmin=0.01,vmax=np.max(dem_object.accumulation_matrix)),
                   extent=extent)

    ax.scatter(dem_object.feature[:,0],
               dem_object.feature[:,1],
               c=np.array([[0.,0.,0.]]),
               s=0.2)

    ax.set_xlim(extent[0],extent[1])
    ax.set_ylim(extent[2],extent[3])

    cbar = f.colorbar(im,cax=cax,orientation='vertical')
    cbar.set_label('Accumulation',rotation=270)

    plt.show()


def plot_boundary(dem_object):
    '''
    Plots a tinerator.DEM boundary. Must have been created
    through the `build_refined_triplane` or 
    `build_uniform_triplane` methods.

    # Arguments
    dem_object (tinerator.DEM): A DEM instance
    '''

    if dem_object.boundary is None:
        raise ValueError('DEM object must have a boundary initialized')

    dem = deepcopy(dem_object.dem)

    if dem_object.mask is not None:
        try:
            dem[dem_object.mask] = np.nan
        except ValueError:
            dem = dem.astype(float)
            dem[dem_object.mask] = np.nan

    fig,ax = plt.subplots(figsize=(12, 8))

    __apply_grid_to_axis(ax)

    extent = dem_object.extent
    im = ax.imshow(dem,zorder=5,extent=extent,cmap=plt.cm.cubehelix)
    cbar = fig.colorbar(im)
    cbar.set_label('Elevation (m)',rotation=270)
    plt.title('DEM with generated boundary')
    ax.scatter(dem_object.boundary[:,0],
               dem_object.boundary[:,1],
               zorder=9,
               s=1.,
               c='red')
    plt.show()


def preview_boundary(dem_object,
                     distance:float,
                     rectangular:bool=False,
                     interactive=False):
    '''
    Preview the mesh boundary nodes that will be generated by
    `edge_length` (in `DEM.build_uniform_triplane`) or `max_edge_length`
    (`DEM.build_refined_triplane`).

    # Arguments
    distance (float): Euclidean distance between adjacent boundary nodes
    rectangular (bool): if True, generates a boundary around the extent of the DEM
    interactive (bool): if True and function is called within a
    Jupyter notebook, then function params can be controlled with sliders
    '''

    # Put into a function for easy Jupyter integration
    def __gen_and_plot(dem_object,distance,rectangular):
        dem_object._generate_boundary(distance,rectangular=rectangular)
        plot_boundary(dem_object)

    frozen_boundary = dem_object.boundary

    if interactive and cfg.IN_NOTEBOOK:
        delta_x = abs(dem_object.extent[0] - dem_object.extent[1]) / 5.0

        if distance > delta_x:
            distance = delta_x

        interact_manual(__gen_and_plot,
                        dem_object=fixed(dem_object),
                        distance=widgets.FloatSlider(min=1,
                                                     max=delta_x,
                                                     step=5,
                                                     value=distance),
                        rectangular=fixed(rectangular));
    else:
        if interactive:
            cfg.log.warn('Cannot init Jupyter notebook functionality')

        __gen_and_plot(dem_object,distance,rectangular)

    tmp_boundary = deepcopy(dem_object.boundary)
    dem_object.boundary = frozen_boundary

def plot_facesets(dem_object,fs_list):
    '''
    Displays a topological preview of how facesets will look after
    Exodus generation.

    # Arguments
    dem_object (tinerator.DEM): A DEM class instance
    fs_list (list<tinerator.Faceset>): One or more initialized Faceset objects
    '''
    import warnings
    np.warnings.filterwarnings('ignore')
    
    cmap = 'tab10'
    
    def plot_row(fs_object,row,extent=None):
        empty = np.zeros(np.shape(dem))
        empty.fill(np.nan)
        
        if fs_object._has_type == '__NAIVE':

            if fs_object._metadata['top']:
                row[0].imshow(dem,cmap=cmap,extent=extent)
            else:
                row[0].imshow(empty,extent=extent)
            
            if fs_object._metadata['sides']:
                row[1].scatter(dem_object.boundary[:,0],dem_object.boundary[:,1])
                row[1].set_aspect(dem_object.ratio)
                row[1].set_xlim(extent[:2])
                row[1].set_ylim(extent[2:])
            else:
                row[1].imshow(empty,extent=extent)
                
            if fs_object._metadata['bottom']:
                row[2].imshow(dem,cmap=cmap,extent=extent)
            else:
                row[2].imshow(empty)

        elif fs_object._has_type == '__FROM_ELEVATION':

            row[1].imshow(empty,extent=extent)
            row[2].imshow(empty,extent=extent)
            
            discrete_dem = np.zeros(dem_object.dem.shape)
            discrete_dem.fill(np.nan)
            heights = [0] + fs_object._data
            heights.sort()
            
            for i in range(len(heights)):
                discrete_dem[dem_object.dem > heights[i]] = i * 10
            
            row[0].imshow(discrete_dem,cmap=cmap,extent=extent)
            
        elif fs_object._has_type == '__SIDESETS':

            row[0].imshow(empty,extent=extent)
            row[2].imshow(empty,extent=extent)
            
            data = fs_lib.__facesets_from_coordinates(
                                {'all': fs_object._data},
                                dem_object.boundary)['all']
            
            bnd = dem_object.boundary
            
            for i in np.unique(data):

                if i == 1 and fs_object._metadata['layers'][0] == 0:
                    continue

                mask = data == i
                row[1].scatter(bnd[:,0][mask],bnd[:,1][mask])

            row[1].set_aspect(dem_object.ratio)
            row[1].set_xlim(extent[:2])
            row[1].set_ylim(extent[2:])
                
        else:
            raise ValueError('Malformed faceset object')
                
    if not isinstance(fs_list,list):
        fs_list = [fs_list]
    
    if dem_object.boundary is None:
        raise ValueError('Please generate a DEM boundary before '\
                         'running this function')
        
    dem = deepcopy(dem_object.dem)
    dem = np.ones(dem.shape)
    dem[dem_object.dem == dem_object.no_data_value] = np.nan
    extent = dem_object.extent
    
    rows,cols = len(fs_list),3

    f, axes = plt.subplots(rows,cols,figsize=(12,8),sharex=True,sharey=True)
    
    top_axes_row = axes[0] if rows > 1 else axes

    top_axes_row[0].set_title('Top')
    top_axes_row[1].set_title('Sides')
    top_axes_row[2].set_title('Bottom')
    
    if rows > 1:
        for row in range(rows):
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                plot_row(fs_list[row],axes[row],extent=extent)
    else:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            plot_row(fs_list[0],top_axes_row,extent=extent)

    plt.show()
    

# -- 3D visualization ------------- #


def plot_triplane(dem_object):
    '''
    Plots the triangular surface mesh.

    !!! note
        Note - due to underlying issues with the renderer, 
        this must be called in `__main__` with the syntax:

    ```python
    tin.plot_triplane(dem).plot()
    ```

    # Arguments
    dem_object (tinerator.DEM): A DEM class instance
    '''

    print('WARNING: This function may not render correctly.\n'\
          'Please use tinerator.plot_triplane until a '\
          'patch is issued.')

    temp_fileid = '__render_exo.exo'
    dump.to_exodus(dem_object,temp_fileid,mesh='surface')
    exii_mesh = pyvista.read(temp_fileid)
    return exii_mesh

    #exii_mesh['Element Blocks'].plot()
    #util.cleanup([temp_fileid])


def plot_full_mesh(dem_object,facesets:list=None):
    '''
    Plots the triangular surface mesh.
    
    !!! note
        Note - due to underlying render issues, 
        this must be called in `__main__` with the syntax:

    ```python
    tin.plot_full_mesh(dem).plot()
    ```

    # Arguments
    dem_object (tinerator.DEM): A DEM class instance
    facesets (list<Facesets>): facesets to render
    '''

    print('WARNING: This function may not render correctly.\n'\
          'Please use tinerator.plot_full_mesh until a '\
          'patch is issued.')

    temp_fileid = '__render_exo.exo'
    dump.to_exodus(dem_object,temp_fileid,mesh='prism',facesets=facesets)

    # If facesets are provided, override element block rendering
    if facesets is None:
        render_blocks = 'Element Blocks'
    else:
        render_blocks = 'Side Sets'

    exii_mesh = pyvista.read(temp_fileid)
    return exii_mesh

    #exii_mesh[render_blocks].plot()
    #util.cleanup([temp_fileid])




