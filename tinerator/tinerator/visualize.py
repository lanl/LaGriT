import numpy as np
import matplotlib.pyplot as plt

try:
    import pycpt
    topocmap = pycpt.load.cmap_from_cptcity_url('wkp/schwarzwald/wiki-schwarzwald-cont.cpt')
except:
    topocmap = 'Spectral_r'

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

def plotDEM(dem_array,title=None,xlabel=None,ylabel=None,extent=[],hillshade_image=True,show_plot=True,plot_out=None):
    '''
    Beautifully plots a DEM.

    Function shamelessly stolen from GeologyAndPython.com
    <http://geologyandpython.com/dem-processing.html>

    :param dem_array: 2D DEM matrix
    :type dem_array: np.ndarray
    :param title: plot title
    :type title: string
    :param xlabel: x-axis label
    :type xlabel: string
    :param ylabel: y-axis label
    :type ylabel: string
    :param extent: The extend of the matrix in the form: (xmin,xmax,ymin,ymax) 
    :type extent: tuple or list
    :param hillshade_image: use hillshading on the flag to produce a 3D effect
    :type hillshade_image: bool
    :param show_plot: flag to show fig.show() the plot 
    :type show_plot: bool
    :param plot_out: file path to save image
    :type plot_out: string
    '''

    if not extent:
        extent = (0,np.shape(dem_array)[1],0,np.shape(dem_array)[0])

    vmin, vmax = np.nanmin(dem_array), np.nanmax(dem_array)

    fig = plt.figure(figsize=(12, 8))
    ax = fig.add_subplot(111)
    if hillshade_image:
        ax.matshow(_hillshade(dem_array, 30, 30), extent=extent, cmap='Greys', alpha=.5, zorder=10, vmin=vmin, vmax=vmax)
    cax = ax.imshow(dem_array, cmap=topocmap, extent=extent, vmin=vmin, vmax=vmax, origin='image')
    fig.colorbar(cax, ax=ax)

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


