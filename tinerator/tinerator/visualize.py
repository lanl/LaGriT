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
    #cax = ax.imshow(dem_array, cmap=topocmap, extent=extent, vmin=vmin, vmax=vmax, origin='image')
    cax = ax.contourf(dem_array, np.arange(vmin, vmax, 10),extent=extent, 
                  cmap=topocmap, vmin=vmin, vmax=vmax, origin='image')
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

def _debugScatterPlot(points):
    '''
    Scatter plot of points with node number annotation.

    '''

    plt.scatter(points[:,0],points[:,1],c='blue')
    for (i,pt) in enumerate(points):
        plt.text(pt[0],pt[1],str(i),color='black')
    plt.show()


def selectFacesetsFromBoundary(dem):
    '''
    Starts an interactive GUI wherein the user selects different regions,
    which then correspond to different facesets.

    :param dem: tinerator.DEM instance
    :type dem: tinerator.DEM
    '''

    from matplotlib.widgets import Button
    from scipy.spatial import distance
    from copy import deepcopy

    if dem.boundary is None:
        raise ValueError('dem.boundary must be generated or assigned')

    extent = (dem.xll_corner,dem.ncols*dem.cell_size+dem.xll_corner,
              dem.yll_corner,dem.nrows*dem.cell_size+dem.yll_corner)

    colors = ['red','blue','green','orange','black','purple','yellow','pink','grey']
    color_idx = 0

    final_points = []    # The complete list of points
    current_points = [0] # Currently selected points

    ax_title = 'Select points in a \nclockwise direction'
    ax_status_title = 'Defined facesets'

    def forceAspect(ax,ax2,aspect=1):
        '''
        Forces ax to conform to the aspect ratio of ax2.
        '''

        im = ax2.get_images()
        extent =  im[0].get_extent()
        ax.set_aspect(abs((extent[1]-extent[0])/(extent[3]-extent[2]))/aspect)

    def reset_pts(event):
        '''
        Resets the workspace so points can be selected again.
        '''
        print('Reset picked points')
        nonlocal final_points
        nonlocal current_points
        nonlocal line
        nonlocal extent
        nonlocal color_idx

        current_points = []
        final_points = []
        color_idx = 0
        ax_status.clear()
        ax_status.imshow(dem_raster,zorder=1,extent=extent,cmap=plt.cm.cubehelix)
        ax.clear()
        mask[:] = True
        ax.plot(dem.boundary[:,0][mask],dem.boundary[:,1][mask], 'o', picker=5)
        ax.set_title(ax_title)
        ax_status.set_title(ax_status_title)
        forceAspect(ax,ax_status)
        plt.draw()

    def save_pts(event):
        '''
        Stores selected points to the full list.
        Points must be selected in a clockwise manner.
        '''
        print('Saved picked points')
        nonlocal final_points
        nonlocal current_points
        nonlocal color_idx

        print(current_points)

        if not current_points:
            print('No points are selected')
            return

        _start = current_points[0]
        _end = current_points[-1]

        _seq = [_start,_end] if _start != _end else [_start]

        # If (we have looped past element 0 in the boundary array...)
        if _start > _end:

            # We need to handle this case in a special way as a pure slice won't work
            _seq = [ [_start,np.shape(dem.boundary)[0]] , [0,_end] ]

            ax_status.scatter(dem.boundary[_seq[0][0]:_seq[0][1],0],dem.boundary[_seq[0][0]:_seq[0][1],1],c=np.repeat(colors[color_idx],(_seq[0][1] - _seq[0][0])))
            ax_status.scatter(dem.boundary[_seq[1][0]:_seq[1][1],0],dem.boundary[_seq[1][0]:_seq[1][1],1],c=np.repeat(colors[color_idx],(_seq[1][1] - _seq[1][0])))

            mask[_seq[0][0]:_seq[0][1]] = False
            mask[_seq[1][0]:_seq[1][1]] = False

        else:

            ax_status.scatter(dem.boundary[_start:_end,0],dem.boundary[_start:_end,1],c=np.repeat(colors[color_idx],_end-_start))
            mask[_start:_end] = False
        
        final_points.append(_seq)

        color_idx += 1
        if color_idx >= len(colors): color_idx = 0

        ax.clear()
        ax.plot(dem.boundary[:,0][mask],dem.boundary[:,1][mask], 'o', picker=5)
        ax.set_title(ax_title)
        ax_status.set_title(ax_status_title)
        forceAspect(ax,ax_status)
        plt.draw()
        current_points = []

    def onpick(event):
        '''
        Called when a node is pressed. Stores the selected node 
        in current_points.
        '''
        
        nonlocal current_points

        # Capture the (x,y) value of mouse press
        thisline = event.artist
        ind = event.ind
        xdata = thisline.get_xdata()
        ydata = thisline.get_ydata()
        picked_point = np.array([tuple(zip(xdata[ind], ydata[ind]))[0]])

        # Determine the boundary index closest to this (x,y)
        ind = distance.cdist(picked_point, dem.boundary[:,:2]).argmin()

        current_points.append(ind)
        print('Current line segment:',current_points,'\n')


    def indicesToValues(points,size):
        '''
        Converts the generated list<list> of indices to a vector
        of 'attributes' that will be applied to the boundary.
        '''
        a = np.full((size,),1,dtype=int)
        for (i,p) in enumerate(points):
            if isinstance(p[0],list):
                a[p[0][0]:p[0][1]] = (i+1)
                a[p[1][0]:p[1][1]] = (i+1)
            else:
                a[p[0]:p[1]] = (i+1)
        return a

    # Generate the left and right figures
    fig, axarr = plt.subplots(1, 2, figsize=(16,8))
    ax = axarr[0]
    ax_status = axarr[1]
    ax.set_title(ax_title)
    ax_status.set_title(ax_status_title)

    mask = np.ones((np.shape(dem.boundary)[0],),dtype=bool)
    line, = ax.plot(dem.boundary[:,0],dem.boundary[:,1], 'o', picker=5)
    scatter = ax_status.scatter([],[])

    # Prepare the DEM for rendering
    dem_raster = deepcopy(dem.dem)
    dem_raster[dem_raster == dem.no_data_value] = np.nan
    ax.text(dem.boundary[0,0],dem.boundary[0,1],'0')

    ax_status.imshow(dem_raster,zorder=1,extent=extent,cmap=plt.cm.cubehelix)
    forceAspect(ax,ax_status)

    # Begin event-driven program
    fig.canvas.mpl_connect('pick_event', onpick)

    # This effectively defines the location and size of the buttons
    axprev = plt.axes([0.7, 0.05, 0.1, 0.075])
    axnext = plt.axes([0.81, 0.05, 0.1, 0.075])

    bprev = Button(axprev, 'Reset')
    bpnext = Button(axnext, 'Save')

    bprev.on_clicked(reset_pts)
    bpnext.on_clicked(save_pts)

    plt.show()

    print('final_points = ',final_points,'\n')

    return indicesToValues(final_points,np.shape(dem.boundary)[0])

