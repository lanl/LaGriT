from pylagrit.pexpect import spawn
from subprocess import call, PIPE
import os,sys
import glob
import re
from collections import  OrderedDict
import numpy
import warnings
from itertools import product
try:
    import xml.etree.cElementTree as ET
except ImportError:
    import xml.etree.ElementTree as ET
from xml.dom import minidom

# Universal-safe function for ensuring string integrity
def _decode_binary(b):
    if isinstance(b,bytes):
        return b.decode('ascii')
    else:
        return b

catch_errors = True

class LaGriT_Warning(Warning):
    pass

class PyLaGriT(spawn):
    ''' 
    Python lagrit class
    
    :param lagrit_exe: Path to LaGriT executable
    :type lagrit_exe: str
    :param verbose: If True, LaGriT terminal output will be displayed
    :type verbose: bool
    :param batch: If True, PyLaGriT will be run in batch mode, collecting LaGriT commands until the run_batch method is called.
    :type batch: bool
    :param batchfile: Name of batch file to use if batch is True
    :type batchfile: str
    :param gmv_exe: Path to GMV executable
    :type gmv_exe: str
    :param paraview_exe: Path to ParaView executable
    :type paraview_exe: str
    :param timeout: Number of seconds to wait for response from LaGriT
    '''

    def __init__(self, lagrit_exe=None, verbose=True, batch=False, batchfile='pylagrit.lgi', gmv_exe=None, paraview_exe=None, timeout=300, *args, **kwargs):
        self.verbose = verbose
        self.mo = {}
        self.surface = {}
        self.region = {}
        self.batch = batch
        self._check_rc()
        if lagrit_exe is not None: self.lagrit_exe = lagrit_exe
        if gmv_exe is not None: self.gmv_exe = gmv_exe
        if paraview_exe is not None: self.paraview_exe = paraview_exe        
        if self.batch:
            try: self.fh = open(batchfile, 'w')
            except IOError as e: 
                print("Unable to open "+batchfile+": {1}".format(e.strerror))
                print("Batch mode disabled")
                self.batch = False
            else:
                self.batchfile = batchfile
                self.fh.write('# PyLaGriT generated LaGriT script\n')
        else:
            super(PyLaGriT, self).__init__(self.lagrit_exe,timeout=timeout,*args, **kwargs) 
            self.expect()
            if verbose: print(_decode_binary(self.before))
    def run_batch(self):
        self.fh.write('finish\n')
        self.fh.close()
        if self.verbose:
            call(self.lagrit_exe+' < '+self.batchfile, shell=True)
        else:
            fout=open('pylagrit.stdout','w')
            call(self.lagrit_exe+' < '+self.batchfile, shell=True, stdout=fout)
            fout.close()
    def expect(self, expectstr='Enter a command',timeout=8640000.):
        if self.batch:
            print("expect disabled during batch mode")
        else:
            super(PyLaGriT, self).expect(expectstr,timeout=timeout) 
    def sendline(self, cmd, verbose=True, expectstr='Enter a command'):
        if self.batch:
            self.fh.write(cmd+'\n')
        else:
            super(PyLaGriT, self).sendline(cmd)
            self.expect(expectstr=expectstr)
            if verbose and self.verbose: print(_decode_binary(self.before))

            if catch_errors:
                for _line in _decode_binary(self.before).split('\n'):
                    if 'ERROR' in _line:
                        raise Exception(_line)
                    elif 'WARNING' in _line:
                        warnings.warn(_line,category=LaGriT_Warning)

    def interact(self, escape_character='^'):
        if self.batch:
            print("Interactive mode unavailable during batch mode")
        else:
            print("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            print("Entering interactive mode")
            print("To return to python terminal, type a '"+escape_character+"' character")
            print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
            print(self.after)
            super(PyLaGriT, self).interact(escape_character=escape_character) 
    def cmo_status(self, cmo=None, brief=False, verbose=True):
        cmd = 'cmo/status'
        if cmo: cmd += '/'+cmo 
        if brief: cmd += '/brief'
        self.sendline(cmd, verbose=verbose)
    def read(self,filename,filetype=None,name=None,binary=False):
        '''
        Read in mesh

        :param filename: Name of mesh file to read in
        :type filename: str
        :param filetype: Type of file, automatically detected if not specified
        :type filetype: str
        :param name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str
        :param binary: Indicates that file is binary if True, ascii if False
        :type binary: bool
        :returns: MO

        Example 1:
            >>> #To use pylagrit, import the module.
            >>> import pylagrit
            >>> #Create your pylagrit session.
            >>> lg = pylagrit.PyLaGriT()
            >>> #Create a mesh object and dump it to a gmv file 'test.gmv'.
            >>> mo = lg.create(name='test')
            >>> mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5,))
            >>> mo.dump('test.gmv')
            >>> mo.dump('test.avs')
            >>> mo.dump('test.lg')
            >>> mo1 = lg.read('test.gmv')
            >>> mo2 = lg.read('test.avs')
            >>> mo3 = lg.read('test.lg',name='test')

        Example 2 - Reading in LaGriT binary file, autodetect mesh object name
            >>> #To use pylagrit, import the module.
            >>> import pylagrit
            >>> import numpy
            >>> #Instantiate the lagrit object.
            >>> lg = pylagrit.PyLaGriT()
            >>> # Create list with mesh object as first element
            >>> dxyz = numpy.array([0.25]*3)
            >>> mins = numpy.array([0.]*3)
            >>> maxs = numpy.array([1.]*3)
            >>> ms = [lg.createpts_dxyz(dxyz,mins,maxs,'tet',connect=True,name='testmo')]
            >>> # Create three new mesh objects, each one directly above the other
            >>> for i in range(3):
            >>>     ms.append(ms[-1].copy())
            >>>     ms[-1].trans(ms[-1].mins,ms[-1].mins+numpy.array([0.,0.,1.]))
            >>> lg.dump('lagrit_binary.lg')
            >>> lg.close()
            >>> lg = pylagrit.PyLaGriT()
            >>> ms_read = lg.read('lagrit_binary.lg')
            >>> print 'Name of mesh object read in should be testmo, is: ', ms_read.name
        '''

        # If filetype is lagrit, name is irrelevant
        if filetype == 'lagrit' or filename.split('.')[-1] in ['lg','lagrit','LaGriT']: islg=True
        else: islg=False
        cmd = ['read',filename]
        if filetype is not None: cmd.append(filetype)
        if islg:
            cmd.append('dum')
        else:
            if name is None:
                name = make_name('mo',self.mo.keys())
            cmd.append(name)
        if binary: cmd.append('binary')
        self.sendline('/'.join(cmd))
        # If format lagrit, cmo read in will not be set to name
        if islg and not self.batch:
            self.sendline('cmo/status/brief', verbose=False)
            # dump lagrit doesn't seem to ever dump multiple mos now???
            mos = []
            for line in _decode_binary(self.before).splitlines():
                if 'Mesh Object name:' in line: 
                    nm = line.split(':')[1].strip()
                    self.mo[nm] = MO(nm,self)
                    mos.append(self.mo[nm])
            if len(mos) == 1:
                if name is not None and name != mos[0].name: 
                    self.sendline('cmo/copy/'+name+'/'+mos[0].name)
                    self.sendline('cmo/release/'+mos[0].name)
                return mos[0]
            elif len(mos) > 1:
                if name is not None:
                    print("Multiple mesh objects exist, 'name' option will be ignored")
                return mos
        else:
            self.mo[name] = MO(name,self)
            return self.mo[name]
    def read_fehm(self,filename,avs_filename='temp.inp',elem_type=None):
        with open(filename,'r') as fh:
            ln = fh.readline()
            nn = int(fh.readline().strip())
            while not 'elem' in ln:
                ln = fh.readline()
            vs = fh.readline().strip().split()
        elem_int = int(vs[0])
        ne = int(vs[1])
        crds = numpy.genfromtxt(filename,skip_header=2,max_rows=nn)
        conns = numpy.genfromtxt(filename,skip_header=2+nn+3,max_rows=ne)
        with open(avs_filename,'w') as fh:
            fh.write('    %d    %d    0    0    0\n'%(nn,ne))
            numpy.savetxt(fh,crds,fmt='%d %f %f %f')
            if elem_type is None:
                if elem_int == 8: elem_type = 'hex'
                elif elem_int == 3: elem_type = 'tri'
                elif elem_int == 4:
                    if numpy.all(numpy.diff(crds[:,1])) == 0 or numpy.all(numpy.diff(crds[:,2])) == 0 or numpy.all(numpy.diff(crds[:,3])) == 0:
                        elem_type = 'quad'
                    else:
                        elem_type = 'tet'
            for conn in conns:
                fh.write('%d 1 %s'%(conn[0],elem_type))
                for i in range(elem_int):
                    fh.write(' %d'%conn[i+1])
                fh.write('\n')
        return self.read(avs_filename)
    
    def read_sheetij(self,name,filename,NXY,minXY,DXY,connect=True,file_type='ascii',flip='none',skip_lines=0,data_type='float'):
        '''
        Creates a quad mesh from an elevation file. Note the input file is read as Z(i,j) into the cmo attribute 'zic'
        
        :param name: name of mesh object
        :type name: string
        :param filename: Elevation filename
        :type filename: string
        :param NXY: [nx, ny] - [columns in x-direction, rows in y-direction]
        :type NXY: list
        :param minXY: [minX, minY] - location of lower left corner
        :type minXY: list
        :param DXY: [Dx, Dy] - cell size in x and y directions
        :type DXY: list
        :param connect: True will create a quad grid, otherwise keeps data as points
        :type connect: bool
        :param file_type: May be either ascii or binary
        :type file_type: string
        :param flip: May be 'x', 'y' to reflect across those axes, or 'none' to keep static
        :type flip: string
        :param skip_lines: skip n number of header lines
        :type skip_lines: integer
        :param data_type: read in elevation data as either float or double
        :type data_type: string
        :returns: MO
        
        Example 1 - Building a surface mesh from Modflow elevation file:
            >>> #To use pylagrit, import the module.
            >>> from pylagrit import PyLaGriT
            >>> import numpy as np
            >>> 
            >>> # Instantiate PyLaGriT
            >>> l = PyLaGriT()
            >>> 
            >>> # Elevation files are typically headerless unwrapped vectors
            >>> # Define parameters to pack these elements into a matrix
            >>> ncols = 276
            >>> nrows = 313
            >>> DXY = [100,100]
            >>> 
            >>> elev_surface = l.read_sheetij('surfacemesh', 'example.mod', [ncols, nrows], [0, 0], DXY, flip='y')
            >>> elev_surface.paraview()
        
        '''
        
        NXY = [str(v) for v in NXY]
        minXY = [str(v) for v in minXY]
        DXY = [str(v) for v in DXY]
        
        connect_str = 'connect' if connect==True else 'points'
        skip_str = 'skip %d' % skip_lines
        
        data_type = data_type.lower()
        file_type = file_type.lower()
        
        if data_type not in ['float', 'double']:
            raise ValueError("data_type must be float or double")
        
        if file_type not in ['ascii', 'binary']:
            raise ValueError("file_type must be ascii or binary")
        
        flip_str = flip.lower()
        
        if flip_str in ['x', 'y', 'xy', 'none']:
            if flip_str == 'x':    flip_str = 'xflip'
            if flip_str == 'y':    flip_str = 'yflip'
            if flip_str == 'xy':   flip_str = 'xflip,yflip'
            if flip_str == 'none': flip_str = ''
        else:
            raise ValueError("Argument flip must be: 'x', 'y', 'xy', or 'none'")
        
        # Create new mesh object with given name
        self.sendline('cmo/create/{}'.format(name))
        self.sendline('cmo/select/{}'.format(name))
        
        # Read in elevation file and append to mesh
        cmd = ['read','sheetij',filename,','.join(NXY),','.join(minXY),','.join(DXY),skip_str,flip_str,connect_str,file_type,data_type]
        self.sendline('/'.join(cmd))
        
        self.mo[name] = MO(name,self)
        return self.mo[name]
        
    def read_modflow(self, materials_file, nrows, ncols, name=None, DXY = [100,100], height=7.75, filename=None):
        '''
        Reads in a Modflow elevation file (and, optionally, an HDF5/txt file containing node materials) and generates and returns hexagonal mesh.
        
        :param filename: Filename of Modflow elevation data file
        :type filename: str
        :param nrows: Number of rows in elevation file
        :type nrows: int
        :param ncols: Number of columns in elevation file
        :type ncols: int
        :param name: Name of returned mesh (optional)
        :type name: str
        :param DXY: Spacing in x/y directions
        :type DXY: list (number)
        :param height: The 'thickness' in the Z-direction of the returned hex mesh
        :type height: float
        :param materials_file: A text or HDF5 binary file containing materials properties for an elevation mesh
        :type materials_file: str
        :param materials_keys: A list containing the keys to the materials array, ordered sequentially. If set, it is assumed materials_file is an HDF5 file.
        :type materials_keys: list (str)
        :returns: MO
        '''
    
    
        if name is None:
            name = make_name('mo',self.mo.keys())
    
        x = numpy.arange(0,ncols+1,1)
        y = numpy.arange(0,nrows+1,1)
        z = numpy.arange(0,2*height,height) # x2 because of half-open interval: [start, stop)
    
        # Generate hexmesh
        # Alternately, just extrude elev_surface
        hexmesh = self.gridder(x,y,z,elem_type='hex',connect=True,name=name)
    
        # Capture hexmesh points as pset
        hexset = hexmesh.pset_geom((0,0,0),(max(x), max(y), max(z)), ctr=(0,0,0), stride=(0,0,0), geom='xyz', name='hexset')

        # Scale hexmesh to match length of surface (optimize later)
        hexset.scale('relative','xyz',[DXY[0],DXY[1],1],[0,0,0])
    
        # Translate such that 50% of mesh is above z=0 and 50% is under
        hexset.trans((0,0,0),(0,0,-height/2))
    
        # Capture points < 0
        hex_bottom = hexmesh.pset_attribute('zic', 0, comparison='lt', stride=(0,0,0), name='pbot')
    
        # Set hex mesh z-coord to 0
        hexmesh.setatt('zic', 0.)
        
        try:
            imt_data = numpy.loadtxt(materials_file)
        except:
            print("ERROR: materials file {} not found!".format(materials_file))
            return
        
        # Write out to hidden materials file
        tmp_file = "._tmp_materials.txt"
        tmp_materials = open(tmp_file,"w")
        
        imt_dims = numpy.shape(imt_data)
        nrows = imt_dims[0]
        ncols = imt_dims[1]

        imt_types = numpy.unique(imt_data).tolist()

        # Ensure that imt values are greater than 0
        imt_min = min(imt_types)
        correction = 0
        
        #if imt_min < 0:
        #    imt_types = [int(i + 1 + abs(imt_min)) for i in imt_types]
        #    correction = 1 + abs(imt_min)
        #elif imt_min == 0:
        #    imt_types = [int(i + 1) for i in imt_types]
        #    correction = 1

        # Unpack matrix into vector and write
        for i in range(0, nrows):
            for j in range(0, ncols):
                imt_value = int(imt_data[(nrows-1)-i][j]) + correction
                tmp_materials.write("{}\n".format(imt_value))

        # Close write file
        tmp_materials.close()
    
        # Project materials onto surface
        mtrl_surface = self.read_sheetij('mo_mat', tmp_file, [ncols, nrows], [0,0], DXY)
        
        # Create psets based on imt values, assign global imt from psets
        #for i in range(0,len(imt_types)):
        #    mtrl_surface.pset_attribute('zic', imt_types[i], comparison='eq', stride=(0,0,0), name='p{}'.format(i))
        #    mtrl_surface.setatt('imt', imt_types[i], stride=['pset','get','p{}'.format(i)])

        #mtrl_surface.setatt('zic', 0.)
    
        hexmesh.addatt('mod_bnds',vtype='VINT',rank='scalar',length='nelements')
        hexmesh.copyatt('zic',attname_sink='mod_bnds',mo_src=mtrl_surface)
        self.sendline('cmo/printatt/{}/mod_bnds/minmax'.format(hexmesh.name))
        self.sendline('cmo/printatt/{}/zic/minmax'.format(mtrl_surface.name))
    
        hexmesh.addatt('pts_topbot')
        hexmesh.setatt('pts_topbot',1.)
        hexmesh.setatt('pts_topbot',2.,stride=['pset','get',hex_bottom.name])
        
        #hexmesh.addatt('newimt')
        #hexmesh.interpolate('continuous','newimt',mtrl_surface,'imt')
        #hexmesh.copyatt('newimt','imt') # Probably unnecessary
        #hexmesh.delatt('newimt')
    
        if filename != None:
            # Load modflow elevation map into surface
            elev_surface = self.read_sheetij('motmp', filename, [ncols, nrows], [0, 0], DXY, flip='y')
    
            # Copy elevation to new attribute and set all surface point height to 0
            elev_surface.addatt('z_elev')
            elev_surface.copyatt('zic','z_elev',elev_surface)
            elev_surface.setatt('zic', 0.)

            # Interpolate elevation onto z_new, copy z_new to Z, translate the bottom half of pts to fill out mesh
            hexmesh.addatt('z_new')
            hexmesh.interpolate('continuous','z_new',elev_surface,'z_elev')
            hexmesh.copyatt('z_new','zic')
            hexmesh.math('add','zic',value=-height,stride=['pset','get',hex_bottom.name],attsrc='z_new')
            hexmesh.delatt('z_new')
        else:
            hexmesh.math('add','zic',value=height,stride=['pset','get',hex_bottom.name],attsrc='zic')

        self.mo[name] = MO(name,self)
        return self.mo[name]
    
    def boundary_components(self, style='node',material_id_number=None,reset=None):
        '''
        Calculates the number of connected components of a mesh for diagnostic purposes.

        :param style: May be element or node
        :type style: string
        :param material_id_number: Only examines nodes with imt = mat. id number
        :type material_id_number: int
        :param reset: May be either True, False, or None
        :type reset: bool
        '''
        
        cmd = ['boundary_components',style]
        
        if material_id_number: cmd.append(str(material_id_number))
        if reset is not None:
            if reset == True:
                cmd.append('reset')
            elif reset == False:
                cmd.append('noreset')
        
        self.sendline('/'.join(cmd))
    
    def addmesh(self, mo1, mo2, style='add', name=None, *args):
        if isinstance(mo1,MO): mo1name = mo1.name
        elif isinstance(mo1,str): mo1name = mo1
        else:
            print("ERROR: MO object or name of mesh object as a string expected for mo1")
            return
        if isinstance(mo2,MO): mo2name = mo2.name
        elif isinstance(mo2,str): mo2name = mo2
        else:
            print("ERROR: MO object or name of mesh object as a string expected for mo2")
            return
        if name is None:
            name = make_name('mo',self.mo.keys())
        cmd = '/'.join(['addmesh',style,name,mo1name,mo2name])
        for a in args: 
            if isinstance(a, str):
                cmd = '/'.join([cmd,a])
            elif isinstance(a,list):
                cmd = '/'.join([cmd,' '.join([str(v) for v in a])])
        self.sendline(cmd)
        self.mo[name] = MO(name,self)
        return self.mo[name]
    def addmesh_add(self, mo1, mo2, name=None, refine_factor=None, refine_style='edge'):
        if refine_factor is None: refine_factor = ' '
        return self.addmesh( mo1, mo2, 'add', name, refine_factor, refine_style )
    def addmesh_amr(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='amr', name=name )
    def addmesh_append(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='append', name=name )
    def addmesh_delete(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='delete', name=name )
    def addmesh_glue(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='glue', name=name )
    def addmesh_intersect(self, pset, mo1, mo2, name=None):
        if isinstance(pset,PSet): psetname = pset.name
        elif isinstance(pset,str): psetname = pset
        else:
            print("ERROR: PSet object or name of PSet object as a string expected for pset")
            return
        if isinstance(mo1,MO): mo1name = mo1.name
        elif isinstance(mo1,str): mo1name = mo1
        else:
            print("ERROR: MO object or name of mesh object as a string expected for mo1")
            return
        if isinstance(mo2,MO): mo2name = mo2.name
        elif isinstance(mo2,str): mo2name = mo2
        else:
            print("ERROR: MO object or name of mesh object as a string expected for mo2")
            return
        if name is None:
            name = make_name('mo',self.mo.keys())
        cmd = '/'.join(['addmesh','intersect',name,psetname,mo1name,mo2name])        
        self.sendline(cmd)
        self.pset[name] = PSet(name,self)
        return self.pset[name]
    def addmesh_merge(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='merge', name=name )
    def addmesh_pyramid(self, mo1, mo2, name=None):
        return self.addmesh( mo1, mo2, style='pyramid', name=name )
    def addmesh_excavate(self, mo1, mo2, name=None, bfs=False, connect=False):
        if bfs: bfsstr = 'bfs'
        else: bfsstr = ' '
        if connect: connectstr = 'connect'
        else: connectstr = ' '
        return self.addmesh( mo1, mo2, 'excavate', name, bfsstr, connectstr )
    def surface_box(self,mins,maxs,name=None,ibtype='reflect'):
        if name is None:
            name = make_name('s',self.surface.keys())
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        cmd = '/'.join(['surface',name,ibtype,'box',','.join(mins),','.join(maxs)])
        self.sendline(cmd)
        self.surface[name] = Surface(name,self)
        return self.surface[name]
    def surface_cylinder(self,coord1,coord2,radius,name=None,ibtype='reflect'):
        if name is None:
            name = make_name('s',self.surface.keys())
        coord1 = [str(v) for v in coord1]
        coord2 = [str(v) for v in coord2]
        cmd = '/'.join(['surface',name,ibtype,'cylinder',','.join(coord1),','.join(coord2),str(radius)])
        self.sendline(cmd)
        self.surface[name] = Surface(name,self)
        return self.surface[name]
    def surface_plane(self,coord1,coord2,coord3,name=None,ibtype='reflect'):
        if name is None:
            name = make_name('s',self.surface.keys())
        coord1 = [str(v) for v in coord1]
        coord2 = [str(v) for v in coord2]
        coord3 = [str(v) for v in coord3]
        cmd = '/'.join(['surface',name,ibtype,'plane',' &\n'+','.join(coord1),' &\n'+','.join(coord2),' &\n'+','.join(coord3)])
        self.sendline(cmd)
        self.surface[name] = Surface(name,self)
        return self.surface[name]
    def region_bool(self,bool,name=None): 
        '''
        Create region using boolean string

        :param bool: String of boolean operations
        :type bool: str
        :param name: Internal lagrit name for mesh object
        :type name: string
        :returns: Region

        Example:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> lg = PyLaGriT()
            >>> # Read in mesh
            >>> motet = lg.read('tet_matclr.inp')
            >>> # fault coordinates in feet
            >>> cs = [[498000.,381946.,0.],
            >>>       [497197.,381946.,0.],
            >>>       [494019.,384890.,0.],
            >>>       [490326.,386959.,0.],
            >>>       [487822.,388599.,0.],
            >>>       [486337.,390755.,0.],
            >>>       [486337.,392000.,0.]]
            >>> # Convert to meters
            >>> cs = numpy.array(cs)/3.28
            >>> # Create surfaces of fault
            >>> ss = []
            >>> for p1,p2 in zip(cs[:-1],cs[1:]):
            >>>     p3 = p1.copy()
            >>>     p3[2] = -4000.
            >>>     ss.append(lg.surface_plane(p1,p2,p3))
            >>> # Create region by boolean operations of fault surfaces
            >>> boolstr = ''
            >>> for i,s in enumerate(ss):
            >>>     if not i == 0: boolstr += ' and '
            >>>     boolstr += 'le '+s.name
            >>> r = lg.region_bool(boolstr)
            >>> # Create pset from region
            >>> p = motet.pset_region(r)
            >>> # Change imt value for pset
            >>> p.setatt('imt',21)
            >>> motet.dump_zone_imt('tet_nefault',21)

        '''
        if name is None:
            name = make_name('r',self.region.keys())
        cmd = '/'.join(['region',name,bool])
        self.sendline(cmd)
        self.region[name] = Region(name,self)
        return self.region[name]
    def _check_rc(self):
        # check if pyfehmrc file exists
        rc_wd1 = os.getcwd()+os.sep+'.pylagritrc'
        rc_wd2 = os.getcwd()+os.sep+'pylagritrc'
        rc_home1 = os.path.expanduser('~')+os.sep+'.pylagritrc'
        rc_home2 = os.path.expanduser('~')+os.sep+'pylagritrc'
        if os.path.isfile(rc_wd1): fp = open(rc_wd1)
        elif os.path.isfile(rc_wd2): fp = open(rc_wd2)
        elif os.path.isfile(rc_home1): fp = open(rc_home1)
        elif os.path.isfile(rc_home2): fp = open(rc_home2)
        else: return
        lns = fp.readlines()
        for ln in lns:
            ln = ln.split('#')[0]       # strip off the comment
            if ln.startswith('#'): continue
            elif ln.strip() == '': continue
            elif ':' in ln:
                v = ln.split(':')
                if v[0].strip() == 'lagrit_exe':
                    self.lagrit_exe = v[1].strip()
                elif v[0].strip() == 'gmv_exe':
                    self.gmv_exe = v[1].strip()
                elif v[0].strip() == 'paraview_exe':
                    self.paraview_exe = v[1].strip()
                else:
                    print('WARNING: unrecognized .pylagritrc line \''+ln.strip()+'\'')
            else:
                print('WARNING: unrecognized .pylagritrc line \''+ln.strip()+'\'')
    def extract_surfmesh(self,name=None,cmo_in=None,stride=[1,0,0],reorder=True,resetpts_itp=True,external=False,append=None):
        if name is None:
            name = make_name('mo',self.mo.keys())
        if cmo_in is not None:
            if not isinstance( cmo_in, MO):
                print("ERROR: MO object or name of mesh object as a string expected for cmo_in")
                return
        if resetpts_itp: cmo_in.resetpts_itp()
        if reorder:
            cmo_in.sendline('createpts/median')
            self.sendline('/'.join(['sort',cmo_in.name,'index/ascending/ikey/itetclr zmed ymed xmed']))
            self.sendline('/'.join(['reorder',cmo_in.name,'ikey']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'xmed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'ymed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'zmed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'ikey']))
        stride = [str(v) for v in stride]
        cmd = ['extract/surfmesh',','.join(stride),name,cmo_in.name]
        if external: cmd.append('external')
        if append: cmd.append(append)
        self.sendline( '/'.join(cmd))
        self.mo[name] = MO(name,self)
        return self.mo[name] 
              
    def read_script(self, fname):
        '''
        Read a LaGriT Script
        
        Given a script name, executes the script in LaGriT. 
        
        :param fname: The name or path to the lagrit script.
        :type fname: str
        '''     
           
        f = open(fname)
        commands = f.readlines()   
        for c in commands:
            #Remove newlines and spaces
            c = ''.join(c.split())
            if len(c) != 0 and 'finish' not in c:
                self.sendline(c)

    def read_att(self,fname,attributes,mesh=None,operation='add'):
        '''
        Reads data from a file into an attribute.
        '''

        if mesh is None:
            mesh = self.create()

        if not isinstance(attributes,(list,tuple)):
            attributes = [attributes]

        if isinstance(operation,(list,tuple)):
            operation = ','.join(list(map(str,operation)))

        cmd = '/'.join(['cmo','readatt',mesh.name,','.join(attributes),operation,fname])
        self.sendline(cmd)

        return mesh

    def define(self,**kwargs):
        '''
        Pass in a variable number of arguments to be defined in 
        LaGriT's internal global scope.

        Note that it is generally considered bad practice in PyLaGriT
        to rely on LaGriT's variable system for parameters; however,
        there are use-cases where it is necessary: i.e., macro scripts.

        Usage:

            lg.define(MO_PTS=mo_pts.name,OUTFILE='mesh.inp',PERTURB32=1.3244)

            >> define / MO_PTS / mo1
            >> define / OUTFILE / mesh.inp
            >> define / PERTURB32 / 1.3244

        '''

        for key,value in kwargs.items():
            self.sendline('define / {0} / {1}'.format(key,value))
                            
    def convert(self, pattern, new_ft):
        '''
        Convert File(s)
        
        For each file of the pattern, creates a new file in the new_ft format. 
        The new files will be inside the directory that the LaGriT object was
        instantiated. The name of each file will be the same as the original 
        file with the extension changed to new_ft.
        
        Supports conversion from avs, and gmv files.
        Supports conversion to avs, exo, and gmv files.
         
        :param pattern: Path, name or unix style file pattern of files to be 
                        converted.
        :type  pattern: str
        
        :param new_ft: New format to convert files.
        :type  new_ft: str

        Example:
            >>> #To use pylagrit, import the module.
            >>> import pylagrit
            >>>
            >>> #Create your pylagrit session.
            >>> lg = pylagrit.PyLaGriT()
            >>>
            >>> #Create a mesh object and dump it to a gmv file 'test.gmv'.
            >>> mo = lg.create(name='test')
            >>> mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5,))
            >>> mo.dump('gmv', 'test.gmv')
            >>>
            >>> #Convert test.gmv to exoduce and contour files.
            >>> lg.convert('test.gmv', 'exo')
            >>> lg.convert('test.gmv', 'avs')
        '''
        
        #Make sure I support the new filetype.
        if new_ft not in ['avs', 'gmv', 'exo']:
            raise ValueError('Conversion to %s not supported.'%new_ft)
        
        #Make sure there are file patterns of this type.
        fnames = glob.glob(pattern)
        if len(fnames) ==  0:
            raise OSError('No files found matching that name or pattern.')
        
        for rpath in fnames:
            #Set everything up for lagrit.
            path = os.path.abspath(rpath)
            fname = path[path.rfind('/')+1:path.rfind('.')]
            old_ft = path[path.rfind('.')+1:]
            
            #Check that I support the old filetype.
            if old_ft not in ['avs', 'gmv']:
                raise ValueError('Conversion from %s not supported.'%old_ft)
  
            try:
                os.symlink(path, 'old_format')   
            except OSError as err:
                raise err('Unable to create a symbolic link.') 
                  
            #Run the commands in lagrit.
            self.sendline('read/%s/old_format/temp_cmo'%old_ft)   
            self.sendline('dump/%s/%s.%s/temp_cmo'%(new_ft, fname, new_ft))

            #Clean up created data.
            self.sendline('cmo/release/temp_cmo')
            os.unlink('old_format')
            
    def merge(self, mesh_objs, elem_type="tet",name=None):
        '''
        Merge Mesh Objects
        
        Merges two or more mesh objects together and returns the combined mesh
        object.
        
        :param mesh_objs: An argument list of mesh objects.
        :type  mesh_objs: MO list
        
        Returns: MO.

        Example:
            >>> #To use pylagrit, import the module.
            >>> import pylagrit
            >>> import numpy
            >>> #Instantiate the lagrit object.
            >>> lg = pylagrit.PyLaGriT()
            >>> # Create list with mesh object as first element
            >>> dxyz = numpy.array([0.25]*3)
            >>> mins = numpy.array([0.]*3)
            >>> maxs = numpy.array([1.]*3)
            >>> ms = [lg.createpts_dxyz(dxyz,mins,maxs,'tet',connect=True)]
            >>> # Create three new mesh objects, each one directly above the other
            >>> for i in range(3):
            >>>     ms.append(ms[-1].copy())
            >>>     ms[-1].trans(ms[-1].mins,ms[-1].mins+numpy.array([0.,0.,1.]))
            >>> # Merge list of mesh objects and clean up
            >>> mo_merge = lg.merge(ms)
            >>> for mo in ms: mo.delete()
            >>> mo_merge.rmpoint_compress(filter_bool=True,resetpts_itp=True)
            >>> mo_merge.paraview(filename='mo_merge.inp')
        ''' 
        if name is None:
            name = make_name('mo',self.mo.keys())
        self.mo[name] = MO(name,self)
        if len(mesh_objs) > 1:
            #mo_merge = self.create(elem_type=elem_type)
            for mo in mesh_objs:
                cmd = '/'.join(['addmesh','merge',name,name,mo.name])
                self.sendline(cmd)
                #mo_merge = self.addmesh_merge(mo_merge,mo)
            #return mo_merge
            #return reduce(self.addmesh_merge, mesh_objs)
        else:
            raise ValueError('Must provide at least two objects to merge.')
        return self.mo[name]
    def create(self, elem_type='tet', name=None, npoints=0, nelements=0):
        '''
        Create a Mesh Object
        
        Creates a mesh object in lagrit and an MO in the LaGriT object. Returns 
        the mesh object. 
        
        :kwarg name: Name to be given to the mesh object.
        :type  name: str
        
        :kwarg mesh: The type of mesh object to create.
        :type  mesh: str
        
        :kwarg npoints: The number of points.
        :type  npoints: int
        
        :kwarg nelements: The number of elements.
        :type  nelements: int
        
        Returns: MO
        '''
                        
        if type(name) is type(None):
            name = make_name('mo', self.mo.keys())     
        self.sendline('cmo/create/%s/%i/%i/%s'%(name, npoints, nelements, elem_type))
        self.mo[name] = MO(name, self)
        return self.mo[name]
        
    def create_tet(self, name=None, npoints=0, nelements=0):
        '''Create a tetrahedron mesh object.'''
        return self.create(elem_type='tet', **minus_self(locals()))
        
    def create_hex(self, name=None, npoints=0, nelements=0):
        '''Create a hexagon mesh object.'''
        return self.create(elem_type='hex', **minus_self(locals()))
        
    def create_pri(self, name=None, npoints=0, nelements=0):
        '''Create a prism mesh object.'''
        return self.create(elem_type='pri', **minus_self(locals()))  
            
    def create_pyr(self, name=None, npoints=0, nelements=0):
        '''Create a pyramid mesh object.'''
        return self.create(elem_type='pyr', **minus_self(locals()))
        
    def create_tri(self, name=None, npoints=0, nelements=0):
        '''Create a triangle mesh object.'''
        return self.create(elem_type='tri', **minus_self(locals()))
        
    def create_qua(self, name=None, npoints=0, nelements=0):
        '''Create a quadrilateral mesh object.'''
        return self.create(elem_type='qua', **minus_self(locals())) 
             
    def create_hyb(self, name=None, npoints=0, nelements=0):
        '''Create a hybrid mesh object.'''
        return self.create(elem_type='hyb', **minus_self(locals()))
        
    def create_line(self, npoints=0, mins=[], maxs=[], rz_switch=(1,1,1), name=None):
        '''Create a line mesh object.'''
        mo_new = self.create(elem_type='lin', name=name, npoints=npoints)
        if len(mins) == 3 and len(maxs) == 3:
            mo_new.createpts_line(npoints,mins,maxs,rz_switch)
        return mo_new
        
    def create_triplane(self, name=None, npoints=0, nelements=0):
        '''Create a triplane mesh object.'''
        return self.create(elem_type='triplane', **minus_self(locals()))
        
    def copy(self, mo, name=None):
        '''
        Copy Mesh Object
        
        Copies a mesh object, mo, and returns the MO object.
        '''
        
        #Check if name was specified, if not just generate one.
        if type(name) is type(None):
            name = make_name('mo', self.mo.keys())
        
        #Create the MO in lagrit and the PyLaGriT object.
        self.sendline('cmo/copy/%s/%s'%(name, str(mo)))
        self.mo[name] = MO(name, self)
        
        return self.mo[name]
    def dump(self, filename, mos=[], filetype='binary'):
        '''
        Dump lagrit binary file
        :arg filename: name of lagrit binary file to create
        :type filename: string
        :arg mos: List of mesh objects to include, default is all
        :type mos: list(MO)
        :arg filetype: Filetype to dump, 'binary' or 'ascii'
        :type mos: string
        '''
        cmd = ['dump','lagrit',filename]
        if len(mos) == 0: cmd.append('-all-')
        else:
            cmd += ','.join([mo.name for mo in mos])
        if filetype == 'ascii': cmd.append('ascii')
        self.sendline('/'.join(cmd))
    def tri_mo_from_polyline(self,coords,order='clockwise',filename='polyline.inp',name=None):
        '''
        Create polygon tri mesh object from points
        Points are expected to be defined clockwise by default

        :param coords: x,y,z coordinates defined in npoints by 3 array, points expected to be ordered clockwise by default
        :type coords: lst(floats) or ndarray(floats)
        :param order: ordering of points, clockwise by default
        :type order: string
        :param filename: Name of avs polyline file to create 
        :type filename: string
        :param name: Internal lagrit name for mesh object
        :type name: string
        :returns: PyLaGriT Mesh Object

        Example:
            >>> from pylagrit import PyLaGriT
            >>> lg = PyLaGriT()
            >>> mo = lg.tri_mo_from_polyline([[0.,0.],[0.,1.],[1.,1.],[1.,0.]])
        '''
        coords = numpy.array(coords)
        mstr = str(coords.shape[0])+' '+str(coords.shape[0])+' 0 0 0\n'
        for i,p in enumerate(coords):
            mstr += ' '.join([str(i+1),str(p[0]),str(p[1]),str(0.)])
            mstr += '\n'
        es1 = numpy.arange(coords.shape[0])+1
        es2 = numpy.roll(es1,coords.shape[0]-1)
        for e1,e2 in zip(es1,es2):
            mstr += ' '.join([str(e1),'1 line ',str(e1),str(e2)])
            mstr += '\n'
        with open(filename,'w') as fh: fh.write(mstr)
        #Check if name was specified, if not just generate one.
        if type(name) is type(None):
            name = make_name('mo', self.mo.keys())
        motmp = self.read(filename)
        motri = motmp.copypts(elem_type='tri')
        motmp.delete()
        self.mo[name] = motri
        return self.mo[name]
    def createpts(self, crd, npts, mins, maxs, elem_type,rz_switch=(1,1,1), rz_value=(1,1,1), connect=False, name=None):
        '''
        Create and Connect Points
        
        :arg crd: Coordinate type of either 'xyz' (cartesian coordinates), 
                    'rtz' (cylindrical coordinates), or 
                    'rtp' (spherical coordinates).
        :type  crd: str
        :arg  npts: The number of points to create in line
        :type npts: tuple(int)
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(int, int, int)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(int, int, int)
        :kwarg elem_type: The type of mesh object to create
        :type  elem_type: str
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        :returns: MO
        
        '''
        if elem_type.startswith(('triplane','qua')):
            assert numpy.where(numpy.array(npts)<=1)[0].shape[0]==1, "%r elem_type requires one (1) in npts" % elem_type
            assert numpy.where((numpy.array(maxs)-numpy.array(mins))==0)[0][0]==1, "%r elem_type requires one zero range (max-min)" % elem_type
        if elem_type.startswith(('tet','pri','pyr','hex')):
            assert numpy.all(numpy.array(npts)>1), "%r elem_type requires all npts greater than 1" % elem_type
            assert numpy.all((numpy.array(maxs)-numpy.array(mins))>0), "%r elem_type requires all ranges (max-min) greater than 0" % elem_type
        mo = self.create(elem_type=elem_type,name=name)
        mo.createpts(crd, npts, mins, maxs, rz_switch=rz_switch, rz_value=rz_value, connect=connect)
        return mo
    def createpts_xyz(self, npts, mins, maxs, elem_type,rz_switch=(1,1,1), rz_value=(1,1,1), connect=True,name=None):
        return self.createpts('xyz',npts,mins,maxs,mesh,rz_switch,rz_value,connect=connect,name=name)
    def createpts_dxyz(self, dxyz, mins, maxs, elem_type, clip='under', hard_bound='min',rz_switch=(1,1,1), rz_value=(1,1,1), connect=True,name=None):
        '''
        Create and Connect Points to create an orthogonal hexahedral mesh. The
        vertex spacing is based on dxyz and the mins and maxs specified. mins
        (default, see hard_bound option) or maxs will be adhered to, while maxs
        (default) or mins will be modified based on the clip option to be
        truncated at the nearest value 'under' (default) or 'over' the range
        maxs-mins. clip and hard_bound options can be mixed by specifying tuples
        (see description below).

        :arg  dxyz: The spacing between points in x, y, and z directions
        :type dxyz: tuple(float,float,float)
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(float,float,float)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(float,float,float)
        :kwarg mesh: The type of mesh object to create, automatically set to 'triplane' if 2d or 'tet' if 3d.
        :type  mesh: str
        :kwarg clip: How to handle bounds if range does not divide by dxyz, either clip 'under' or 'over' range
        :type clip: string or tuple(string,string,string)
        :kwarg hard_bound: Whether to use the "min" or "max" as the hard constraint on dimension
        :type hard_bound: string or tuple(string,string,string)
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        :kwarg connect: Whether or not to connect points
        :type  connect: boolean
        
        Example:
            >>> from pylagrit import PyLaGriT
            >>> l = PyLaGriT()
            >>> 
            >>> # Create 2x2x2 cell mesh
            >>> m = l.create()
            >>> m.createpts_dxyz((0.5,0.5,0.5),(0.,0.,0.),(1.,1.,1.),rz_switch=[1,1,1],connect=True)
            >>> m.paraview()
            >>> #m.gmv()
            >>> 
            >>> # Create 2x2x2 mesh where maxs will be truncated to nearest value under given maxs
            >>> m_under = l.create()
            >>> m_under.createpts_dxyz((0.4,0.4,0.4),(0.,0.,0.),(1.,1.,1.),rz_switch=[1,1,1],connect=True)
            >>> m_under.paraview()
            >>> #m_under.gmv()
            >>> 
            >>> # Create 3x3x3 mesh where maxs will be truncated to nearest value over given maxs
            >>> m_over = l.create()
            >>> m_over.createpts_dxyz((0.4,0.4,0.4),(0.,0.,0.),(1.,1.,1.),clip='over',rz_switch=[1,1,1],connect=True)
            >>> m_over.paraview()
            >>> #m_over.gmv()
            >>> 
            >>> # Create 3x3x3 mesh where x and y maxs will be truncated to nearest value over given maxs
            >>> # and z min will be truncated  to nearest value
            >>> m_mixed = l.create()
            >>> m_mixed.createpts_dxyz((0.4,0.4,0.4),(0.,0.,-1.),(1.,1.,0.),hard_bound=('min','min','max'),clip=('under','under','over'),rz_switch=[1,1,1],connect=True)
            >>> m_mixed.paraview()
            >>> #m_mixed.gmv()
        '''
        mo = self.create(elem_type=elem_type,name=name)
        mo.createpts_dxyz(dxyz, mins, maxs, clip='under', hard_bound='min',rz_switch=(1,1,1), rz_value=(1,1,1), connect=True)
        return mo
    def createpts_rtz(self, npts, mins, maxs, elem_type, rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        return self.createpts('rtz',npts,mins,maxs,elem_type,rz_switch,rz_value,connect=connect)
    def createpts_rtp(self, npts, mins, maxs, elem_type, rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        return self.createpts('rtp',npts,mins,maxs,elem_type, rz_switch,rz_value,connect=connect)
    def createpts_line(self, npts, mins, maxs, elem_type='line', rz_switch=(1,1,1),name=None):
        '''
        Create and Connect Points in a line
        
        :arg  npts: The number of points to create in line
        :type npts: int
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(int, int, int)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(int, int, int)
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        
        '''
        mo = self.create(elem_type,name=name)
        mo.createpts_line( npts, mins, maxs, rz_switch=rz_switch)
        return mo
    def gridder(self,x=None,y=None,z=None,connect=False,elem_type='tet',name=None,filename='gridder.inp'):
        '''
        Generate a logically rectangular orthogonal mesh corresponding to vectors of nodal positions.

        :arg x: x discretization locations
        :type x: array(floats)
        :arg y: y discretization locations
        :type y: array(floats)
        :arg z: z discretization locations
        :type z: array(floats)
        :arg connect: Should the points be connected
        :type connect: bool
        :arg elem_type: Type of element for created mesh object
        :type elem_type: string
        :arg filename: Name of avs file created with nodal coordinates
        :type filename: string
        :returns: MO

        Example:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> lg = PyLaGriT()
            >>> x0 = -numpy.logspace(1,2,15,endpoint=True)
            >>> x1 = numpy.arange(-10,10,1)
            >>> x2 = -x0
            >>> x = numpy.concatenate([x0,x1,x2])
            >>> y = x
            >>> mqua = lg.gridder(x,y,elem_type='quad',connect=True)
            >>> mqua.paraview()
        '''
        dim = 0
        if x is not None: 
            if len(x) > 0: dim += 1 
        if y is not None: 
            if len(y) > 0: dim += 1 
        if z is not None: 
            if len(z) > 0: dim += 1 
        if dim == 0:
            print("ERROR: must define at least one of x, y, z arrays")
            return
        if elem_type in ['line'] and dim != 1:
            print("Error: Only 1 coordinate array (x,y,z) required for elem_type 'line'")
            return
        if elem_type in ['tri','quad'] and dim != 2:
            print("Error: Only 2 coordinate arrays (x,y,z) required for elem_type '"+str(elem_type)+"'")
            return
        if elem_type in ['tet','hex'] and dim != 3:
            print("Error: 3 coordinate arrays (x,y,z) required for elem_type '"+str(elem_type)+"'")
            print("Set elem_type to a 2D format like 'quad' or 'triplane'")
            return
        if x is None or len(x) == 0: x = [0]
        if y is None or len(y) == 0: y = [0]
        if z is None or len(z) == 0: z = [0]
        x = list(numpy.unique(x))
        y = list(numpy.unique(y))
        z = list(numpy.unique(z))
        nodelist = numpy.array(list(product(*[z,y,x])))
        nodelist = numpy.fliplr(nodelist)

        outfile = open(filename,'w')
        outfile.write('   '+str(len(nodelist))+' 0 0 0 0\n')
        for i,nd in enumerate(nodelist):
            outfile.write('%11d' % i +'        ')
            outfile.write('%14.8f' % nd[0]+'        ')
            outfile.write('%14.8f' % nd[1]+'        ')
            outfile.write('%14.8f' % nd[2])
            outfile.write('\n')
        outfile.write('\n')
        outfile.close()
        
        m = self.create(elem_type) if name == None else self.create(elem_type, name=name)
        m.read(filename)
            
        if elem_type in ['quad','hex'] and connect:
            cmd = ['createpts','brick','xyz',' '.join([str(len(x)),str(len(y)),str(len(z))]),'1 0 0','connect'] 
            m.sendline('/'.join(cmd))
        elif connect:
            m.connect()
            
        self.sendline('cmo/printatt/{}/-xyz- minmax'.format(m.name))
        return m
    def points(self,coords,connect=False,elem_type='tet',filename='points.inp'):
        '''
        Generate a mesh object of points defined by x, y, z vectors.

        :arg coords: list of 3-tuples containing (x,y,z) coorinates
        :type x: array(3-tuples), npoints by 3 array
        :arg connect: Should the points be connected
        :type connect: bool
        :arg elem_type: Type of element for created mesh object
        :type elem_type: string
        :arg filename: Name of avs file created with nodal coordinates
        :type filename: string
        :returns: MO

        Example:
            >>> from pylagrit import PyLaGriT
            >>> lg = PyLaGriT()
            >>> coords = [[0,0,0],[1,0,0],[1,1,0],[0,1,1],[0,0,1],[0,1,0],[1,1,1],[1,0,1]]
            >>> m = lg.points(coords,elem_type='tet',connect=True)
            >>> m.paraview()
        '''
        dim = 0
        coords = numpy.array(coords)
        ix = numpy.all(numpy.diff(coords[:,0])==0)
        if not ix: dim += 1
        iy = numpy.all(numpy.diff(coords[:,1])==0)
        if not iy: dim += 1
        iz = numpy.all(numpy.diff(coords[:,2])==0)
        if not iz: dim += 1
        if elem_type in ['line'] and dim != 1:
            print("Error: Coordinates must form line for elem_type 'line'")
            return
        if elem_type in ['tri','quad'] and dim != 2:
            print("Error: Coordinates must form plane for elem_type '"+str(elem_type)+"'")
            return
        if elem_type in ['tet','hex'] and dim != 3:
            print("Error: 3D coordinates required for elem_type '"+str(elem_type)+"'")
            print("Set elem_type to a 2D format like 'quad' or 'triplane'")
            return

        outfile = open(filename,'w')
        outfile.write('   '+str(len(coords))+' 0 0 0 0\n')
        for i,nd in enumerate(coords):
            outfile.write('%11d' % i +'        ')
            outfile.write('%14.8f' % nd[0]+'        ')
            outfile.write('%14.8f' % nd[1]+'        ')
            outfile.write('%14.8f' % nd[2])
            outfile.write('\n')
        outfile.write('\n')
        outfile.close()
        m = self.create(elem_type)
        m.read(filename)
        if elem_type in ['quad','hex'] and connect:
            cmd = ['createpts','brick','xyz',' '.join([str(len(coords)),str(len(coords)),str(len(coords))]),'1 0 0','connect'] 
            m.sendline('/'.join(cmd))
        elif connect:
            m.connect()
        return m
 
class MO(object):
    ''' Mesh object class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
        self.pset = {}
        self.eltset = {}
        self.region = {}
    def __repr__(self):
        return self.name
    def sendline(self,cmd, verbose=True, expectstr='Enter a command'):
        self._parent.sendline('cmo select '+self.name,verbose=verbose)
        self._parent.sendline(cmd,verbose=verbose,expectstr=expectstr)
    @property
    def mins(self):
        return numpy.array([self.xmin,self.ymin,self.zmin])
    @property
    def maxs(self):
        return numpy.array([self.xmax,self.ymax,self.zmax])
    @property
    def xmin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[4].split()[1])
    @property
    def xmax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[4].split()[2])
    @property
    def xlength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[4].split()[4])
    @property
    def ymin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[5].split()[1])
    @property
    def ymax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[5].split()[2])
    @property
    def ylength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[5].split()[4])
    @property
    def zmin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[6].split()[1])
    @property
    def zmax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return float(strarr[6].split()[2])
    @property
    def zlength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[6].split()[4])
    @property
    def nnodes(self):
        self.status(1,verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[7].split()[4])
    @property
    def nelems(self):
        self.status(1,verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[7].split()[-1])
    @property
    def ndim_geo(self):
        self.status(1,verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[8].split()[3])
    @property
    def ndim_topo(self):
        self.status(1,verbose=False)
        strarr = self._parent.before.splitlines()
        return int(strarr[9].split()[3])
    @property
    def elem_type(self):
        self.status(1,verbose=False)
        strarr = self._parent.before.splitlines()
        etype = _decode_binary(strarr[8].split()[7])
        if etype == 'tri':
            if self.ndim_geo == 2: etype = 'triplane'
        return etype
    def status(self,brief=False,verbose=True):
        print(self.name)
        self._parent.cmo_status(self.name,brief=brief,verbose=verbose)
    def select(self):
        self.sendline('cmo/select/'+self.name)
    def read(self,filename,filetype=None):
        # If filetype is lagrit, name is irrelevant
        if filetype is not None:
            cmd = '/'.join(['read',filetype])
        else:
            cmd = 'read'
        if filetype != 'lagrit':
            cmd = '/'.join([cmd,filename,self.name])
        else:
            print("Error: Can't read in lagrit type file into existing mesh object")
            return
        self.sendline(cmd)

    def printatt(self,attname=None,stride=[1,0,0],pset=None,eltset=None,ptype='value'):
        stride = [str(v) for v in stride]
        if attname is None: attname = '-all-'
        if pset is None and eltset is None:
            cmd = '/'.join(['cmo/printatt',self.name,attname,ptype,','.join(stride)])
        else:
            if pset is not None:
                if isinstance(pset,PSet): setname = pset.name
                elif isinstance(pset,str): setname = pset
                else:
                    print("ERROR: PSet object or name of PSet object as a string expected for pset")
                    return
                cmd = '/'.join(['cmo/printatt',self.name,attname,ptype,','.join(['pset','get',setname])])
            if eltset is not None:
                if isinstance(eltset,EltSet): setname = eltset.name
                elif isinstance(eltset,str): setname = eltset
                else:
                    print("ERROR: EltSet object or name of EltSet object as a string expected for eltset")
                    return
                cmd = '/'.join(['cmo/printatt',self.name,attname,ptype,','.join(['eltset','get',setname])])
        self.sendline(cmd)
    def delatt(self,attnames,force=True):
        '''
        Delete a list of attributes

        :arg attnames: Attribute names to delete
        :type attnames: str or lst(str)
        :arg force: If true, delete even if the attribute permanent persistance
        :type force: bool

        '''
        # If single attribute as string, make list
        if isinstance(attnames,str): attnames = [attnames]
        for att in attnames:
            if force:
                cmd = '/'.join(['cmo/DELATT',self.name,att])
            else:
                cmd = '/'.join(['cmo/delatt',self.name,att])
            self.sendline(cmd)
    def copyatt(self,attname_src,attname_sink=None,mo_src=None):
        '''
        Add a list of attributes

        :arg attname_src: Name of attribute to copy
        :type attname_src: str
        :arg attname_sink: Name of sink attribute
        :type attname_sink: str
        :arg mo_src: Name of source mesh object
        :type mo_src: PyLaGriT Mesh Object

        '''
        if attname_sink is None: attname_sink = attname_src
        if mo_src is None: mo_src = self
        cmd = '/'.join(['cmo/copyatt',self.name,mo_src.name,attname_sink,attname_src])
        self.sendline(cmd)
    def addatt(self,attname,keyword=None,vtype='VDOUBLE',rank='scalar',length='nnodes',interpolate='linear',persistence='permanent',ioflag='',value=0.0):
        '''
        Add a list of attributes

        :arg attnames: Attribute name to add
        :type attnames: str
        :arg keyword: Keyword used by lagrit for specific attributes
        :type name: str
        :arg vtype: Type of variable {'VDOUBLE','VINT',...}
        :type name: str

        '''
        if keyword is not None:
            cmd = '/'.join(['cmo/addatt',self.name,keyword,attname])
        else:
            cmd = '/'.join(['cmo/addatt',self.name,attname,vtype,rank,length,interpolate,persistence,ioflag,str(value)])
        self.sendline(cmd)
    def addatt_voronoi_volume(self,name='voronoi_volume'):
        '''
        Add voronoi volume attribute to mesh object

        :arg name: name of attribute in LaGriT
        :type name: str
        '''
        self.addatt(name,keyword='voronoi_volume')
    def minmax(self,attname=None,stride=[1,0,0]):
        self.printatt(attname=attname,stride=stride,ptype='minmax')
    def minmax_xyz(self,stride=[1,0,0],verbose=True):
        cmd = '/'.join(['cmo/printatt',self.name,'-xyz-','minmax'])
        self.sendline(cmd,verbose=verbose)
    def list(self,attname=None,stride=[1,0,0],pset=None):
        self.printatt(attname=attname,stride=stride,pset=pset,ptype='list')
    def setatt(self,attname,value,stride=[1,0,0]):
        stride = [str(v) for v in stride]
        cmd = '/'.join(['cmo/setatt',self.name,attname,','.join(stride),str(value)])
        self.sendline(cmd)

    def information(self):
        '''
        Returns a formatted dictionary with mesh information.

        Information is that found in cmo/status/MO
        '''
        import contextlib

        @contextlib.contextmanager
        def capture():
            import sys
            from io import StringIO
            oldout,olderr = sys.stdout, sys.stderr
            try:
                out=[StringIO(), StringIO()]
                sys.stdout,sys.stderr = out
                yield out
            finally:
                sys.stdout,sys.stderr = oldout, olderr
                out[0] = out[0].getvalue()
                out[1] = out[1].getvalue()

        _temp = self._parent.verbose
        self._parent.verbose = True
        with capture() as out:
            self.sendline('cmo/status/'+self.name,verbose=True)
        self._parent.verbose = _temp

        atts = {}
        in_attributes_section = False

        for line in out[0].replace('\r','').split('\n'):
            lline = line.strip().lower()
            split = line.strip().split()

            if not in_attributes_section:
                if 'number of nodes' in lline:
                    atts['nodes'] = int(split[4])
                if 'number of elements' in lline:
                    atts['elements'] = int(split[-1])
                if 'dimensions geomoetry' in lline:
                    atts['dimensions'] = int(split[3])
                if 'element type' in lline:
                    atts['type'] = split[-1]
                if 'dimensions topology' in lline:
                    atts['dimensions_topology'] = int(split[3])
                if 'name' and 'type' and 'rank' and 'length' in lline:
                    in_attributes_section = True
                    atts['attributes'] = {}

            else:

                try:
                    name,atype,rank,length,inter,persi,io,value = split[1:]
                except ValueError:
                    continue 

                atts['attributes'][name] = {}
                atts['attributes'][name]['type'] = atype
                atts['attributes'][name]['rank'] = rank
                atts['attributes'][name]['length'] = length
                atts['attributes'][name]['inter'] = inter
                atts['attributes'][name]['persi'] = persi
                atts['attributes'][name]['io'] = io

                try:
                    atts['attributes'][name]['value'] = float(value)
                except ValueError:
                    atts['attributes'][name]['value'] = value

        return atts
        
    def pset_geom(
            self, mins, maxs, 
            ctr=(0,0,0), geom='xyz', stride=(1,0,0), name=None
        ):
        '''
        Define PSet by Geometry
        
        Selects points from geomoetry specified by string geom and returns a 
        PSet.
        
        :arg  mins: Coordinate of one of the shape's defining points.
                     xyz (Cartesian):   (x1, y1, z1); 
                     rtz (Cylindrical): (radius1, theta1, z1);
                     rtp (Spherical):   (radius1, theta1, phi1);
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Coordinate of one of the shape's defining points.
                     xyz (Cartesian):   (x2, y2, z2); 
                     rtz (Cylindrical): (radius2, theta2, z2);
                     rtp (Spherical):   (radius2, theta2, phi2);
        :type maxs: tuple(int, int, int)
        
        :kwarg ctr: Coordinate of the relative center.
        :type  ctr: tuple(int, int, int)
        
        :kwarg geom: Type of geometric shape: 'xyz' (spherical), 
                     'rtz' (cylindrical), 'rtp' (spherical)
        :type  geom: str
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        
        if name is None:
            name = make_name('p',self.pset.keys())
            
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        stride = [str(v) for v in stride]
        center = [str(v) for v in ctr]
        
        cmd = '/'.join(['pset', name, 'geom', geom, ','.join(stride),
                        ','.join(mins),','.join(maxs), ','.join(center)])
        self.sendline(cmd)
        self.pset[name] = PSet(name, self)
        
        return self.pset[name]

    def pset_geom_xyz(self, mins, maxs, ctr=(0,0,0), stride=(1,0,0), name=None):
        '''
        Define PSet by Tetrahedral Geometry
        
        Selects points from a Tetrahedral region.
        
        :arg  mins: Coordinate point of 1 of the tetrahedral's corners. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Coordinate point of 1 of the tetrahedral's corners.
        :type maxs: tuple(int, int, int)
        
        :kwarg ctr: Coordinate of the relative center.
        :type  ctr: tuple(int, int, int)
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        return self.pset_geom(geom='xyz', **minus_self(locals()))

    def pset_geom_rtz(self, mins, maxs, ctr=(0,0,0), stride=(1,0,0), name=None):
        '''
        Forms a pset of nodes within the cylinder or cylindrical shell section 
        given by radius1 to radius2, and angles theta1 to theta2 and height z1 to z2.
        Refer to http://lagrit.lanl.gov/docs/conventions.html for an explanation of angles
        
        :arg  mins: Defines radius1, theta1, and z1. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Defines radius2, theta2, and z2.
        :type maxs: tuple(int, int, int)
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        :kwarg ctr: Coordinate of the relative center.
        :type  ctr: tuple(int, int, int)
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        return self.pset_geom(geom='rtz', **minus_self(locals()))
        
    def pset_geom_rtp(self, mins, maxs, ctr=(0,0,0), stride=(1,0,0), name=None):
        '''
        Forms a pset of nodes within the sphere, sperical shell or sperical section 
        given by radius1 to radius2, and angles theta1 to theta2 (0 - 180) and angles 
        phi1 to phi2 (0 - 360).
        Refer to http://lagrit.lanl.gov/docs/conventions.html for an explanation of angles
        
        :arg  mins: Defines radius1, theta1, and phi1. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Defines radius2, theta2, and phi2.
        :type maxs: tuple(int, int, int)
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        :kwarg ctr: Coordinate of the relative center.
        :type  ctr: tuple(int, int, int)
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        return self.pset_geom(geom='rtp', **minus_self(locals()))
        
    def pset_attribute(self, attribute,value,comparison='eq',stride=(1,0,0), name=None):
        '''
        Define PSet by attribute 
        
        :kwarg attribute: Nodes defined by attribute ID.
        :type  attribute: str
        
        :kwarg value: attribute ID value.
        :type  value: integer
        
        :kwarg comparison: attribute comparison, default is eq.
        :type  comparison: can use default without specifiy anything, or list[lt|le|gt|ge|eq|ne] 
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        if name is None:
            name = make_name('p',self.pset.keys())
            
        stride = [str(v) for v in stride]
        
        cmd = '/'.join(['pset', name, 'attribute', attribute, ','.join(stride),
                        str(value),comparison])
        self.sendline(cmd)
        self.pset[name] = PSet(name, self)

        return self.pset[name]

    def compute_distance(self, mo, option='distance_field', attname='dfield'):
        '''
        Compute distance from one mesh object to another
        
        :kwarg mo: Mesh object to compute distance to base mesh from
        :type  mo: LaGriT mesh object 
        
        :kwarg option: The type of distance field calculation. Available choices
         are 'distance_field' and 'signed_distance_field'.
        :type  option: str
        
        :kwarg attname: The name of the attribute to be created in the base mesh.
        :type  attname: str
        
        Returns: New attribute in base mesh object

        Example:
        from pylagrit import PyLaGriT
        #create source mesh
        npts = (1,91,1)
        mins = (3.,0.,0.)
        maxs = (3.,270.,0.)
        src_mo = lg.create()
        src_mo.createpts_rtz(npts,mins,maxs,connect=False)

        #create sink mesh
        snk_mo = lg.create()
        snk_mo.createpts_xyz([30,30,1],[-5.,-5.,-5.],[5.,5.,5.],connect=False)

        #compute distance and store in sink mesh attribute 'dfield'
        snk_mo.compute_distance(src_mo)
        snk_mo.dump('comptest.gmv')
        '''        
        if option not in ['distance_field', 'signed_distance_field']:
            print("ERROR: 'option' must be 'distance_field' or 'signed_distance_field'")
            return

        self.sendline('/'.join(['compute',option,self.name,mo.name,attname]))

    def compute_extrapolate(self, surf_mo, dir='zpos',attname='zic'):
        '''
        Given a 3D mesh and a 2D surface, this command will extrapolate a scalar
         value from that surface onto every point of the mesh.
        
        :kwarg surf_mo: Surface mesh object to extrapolate from
        :type  surf_mo: LaGriT mesh object 
        
        :kwarg dir: The direction values are extrapolated from. Choices are one 
        of: 'zpos', 'zneg', 'ypos', 'yneg', 'xpos', 'xneg'
        :type  dir: str
        
        :kwarg attname: The name of the attribute in the surface mesh to be 
        extrapolated
        :type  attname: str
        
        Returns: New attribute in base mesh object

        Example:
        from pylagrit import PyLaGriT
        #create surface mesh
        p1 = (-1.,-1.,-1.)
        p2 = (301.,-1.,-1.)
        p3 = (301.,301.,-1.)
        p4 = (-1.,301.,-1.)
        pts = [p1,p2,p3,p4]
        nnodes = (30,30,1)
        surf = lg.create_qua()
        surf.quadxy(nnodes,pts)
        
        #make surface mesh interesting
        surf.math('sin','zic',cmosrc=surf,attsrc='xic')
        surf.math('multiply','zic',value=5.0,cmosrc=surf,attsrc='zic')
        surf.perturb(0.,0.,1.)
        surf.math('add','zic',value=60.0,cmosrc=surf,attsrc='zic')
        
        #create base mesh
        hex = lg.create_hex()
        hex.createpts_brick_xyz([30,30,20],[0.,0.,0.],[300.,300.,50.])
        hex.resetpts_itp()
        
        #extrapolate z values from surface mesh to base mesh
        hex.compute_extrapolate(surf)
        hex.dump('extrapolated.gmv')
        '''        

        self.sendline('/'.join(['compute','linear_transform',self.name,surf_mo.name,dir,attname]))

    def pset_region(self, region, stride=(1,0,0), name=None):
        '''
        Define PSet by region
        
        :kwarg region: region to create pset
        :type  value: PyLaGriT Region object
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        if name is None:
            name = make_name('p',self.pset.keys())
            
        stride = [str(v) for v in stride]
        
        cmd = '/'.join(['pset', name, 'region',region.name, ','.join(stride)])
        self.sendline(cmd)
        self.pset[name] = PSet(name, self)

        return self.pset[name]

    def pset_surface(self, surface, stride=(1,0,0), name=None):
        '''
        Define PSet by surface
        
        :kwarg surface: surface to create pset
        :type  value: PyLaGriT Surface object
        
        :kwarg stride: Nodes defined by ifirst, ilast, and istride.
        :type  stride: list[int, int, int]
        
        :kwarg name: The name to be assigned to the PSet created.
        :type  name: str
        
        Returns: PSet object
        '''
        if name is None:
            name = make_name('p',self.pset.keys())
            
        stride = [str(v) for v in stride]
        
        cmd = '/'.join(['pset', name, 'surface',surface.name, ','.join(stride)])
        self.sendline(cmd)
        self.pset[name] = PSet(name, self)

        return self.pset[name]

    #def pset_not(self, ps, name=None):
    #    '''
    #    Return PSet from Logical Not
    #    
    #    Defines and returns a PSet from points that are not inside the PSet, ps.
    #    '''
    #    
    #    #Generated a name if one is not specified.
    #    if name is None:
    #        name = make_name('p',self.pset.keys())
    #    
    #    #Create the new PSET in lagrit and the pylagrit object.
    #    cmd = 'pset/%s/not/%s'%(name, str(ps))
    #    self.sendline(cmd)
    #    self.pset[name] = PSet(name, self)
    #    
    #    return self.pset[name]

    def pset_bool(self, pset_list, boolean='union', name=None):
        '''
        Return PSet from boolean operation on list of psets
        
        Defines and returns a PSet from points that are not inside the PSet, ps.
        '''
        #Generated a name if one is not specified.
        if name is None:
            name = make_name('p',self.pset.keys())
        
        #Create the new PSET in lagrit and the pylagrit object.
        cmd = ['pset',name,boolean]
        if isinstance(pset_list,PSet): cmd.append(pset_list.name)
        elif isinstance(pset_list,list):
            cmd.append(','.join([p.name for p in pset_list]))
        self.sendline('/'.join(cmd))
        self.pset[name] = PSet(name, self)
        return self.pset[name]
    def pset_union(self, pset_list, name=None):
        return self.pset_bool(pset_list,boolean='union',name=name)
    def pset_inter(self, pset_list, name=None):
        return self.pset_bool(pset_list,boolean='inter',name=name)
    def pset_not(self, pset_list, name=None):
        return self.pset_bool(pset_list,boolean='not',name=name)
    def resetpts_itp(self):
        '''
        set node type from connectivity of mesh 
        
        '''
        self.sendline('resetpts/itp')

    def eltset_object(self, mo, name=None):
        '''
        Create element set from the intersecting elements with another mesh object
        '''
        if name is None:
            name = make_name('e',self.eltset.keys())
        attr_name = self.intersect_elements(mo)
        e_attr = self.eltset_attribute(attr_name,0,boolstr='gt')
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def eltset_bool(self, eset_list, boolstr='union', name=None):
        '''
        Create element set from boolean operation of set of element sets

        :arg eset_list: List of elements to perform boolean operation on
        :type eset_list: lst(PyLaGriT element set)
        :arg boolstr: type of boolean operation to perform on element sets, one of [union,inter,not]
        :type boolstr: str
        :arg name: The name to be assigned to the EltSet within LaGriT
        :type name: str
        :returns: PyLaGriT element set object
        '''
        if name is None:
            name = make_name('e',self.eltset.keys())
        cmd = ['eltset',name,boolstr,' '.join([e.name for e in eset_list])]
        self.sendline('/'.join(cmd))
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def eltset_union(self, eset_list, name=None):
        return self.eltset_bool(eset_list,'union',name=name)
    def eltset_inter(self, eset_list, name=None):
        return self.eltset_bool(eset_list,'inter',name=name)
    def eltset_not(self, eset_list, name=None):
        return self.eltset_bool(eset_list,'not',name=name)
    def eltset_region(self,region,name=None):
        if name is None:
            name = make_name('e',self.eltset.keys())
        if isinstance(region,Region): region_name = region.name
        elif isinstance(region,str): region_name = region
        else:
            print('region must be a string or object of class Region')
            return
        cmd = '/'.join(['eltset',name,'region',region_name])
        self.sendline(cmd)
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def eltset_attribute(self,attribute_name,attribute_value,boolstr='eq',name=None):
        if name is None:
            name = make_name('e',self.eltset.keys())
        cmd = '/'.join(['eltset',name,attribute_name,boolstr,str(attribute_value)])
        self.sendline(cmd)
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def rmpoint_pset(self,pset,itype='exclusive',compress=True,resetpts_itp=True):
        if isinstance(pset,PSet): name = pset.name
        elif isinstance(pset,str): name = pset
        else:
            print('p must be a string or object of class PSet')
            return
        cmd = 'rmpoint/pset,get,'+name+'/'+itype
        self.sendline(cmd)
        if compress: self.rmpoint_compress(resetpts_itp=resetpts_itp)
    def rmpoint_eltset(self,eltset,compress=True,resetpts_itp=True):
        if isinstance(eltset,EltSet): name = eltset.name
        elif isinstance(eltset,str): name = eltset
        else:
            print('eltset must be a string or object of class EltSet')
            return
        cmd = 'rmpoint/element/eltset,get,'+name
        self.sendline(cmd)
        if compress: self.rmpoint_compress(resetpts_itp=resetpts_itp)
    def rmpoint_compress(self,filter_bool=False,resetpts_itp=True):
        '''
        remove all marked nodes and correct the itet array 

        :param resetpts_itp: set node type from connectivity of mesh
        :type resetpts_itp: bool

        '''

        if filter_bool: self.sendline('filter/1,0,0')
        self.sendline('rmpoint/compress')
        if resetpts_itp: self.resetpts_itp()
    def reorder_nodes(self, order='ascending',cycle='zic yic xic'):
        self.sendline('resetpts itp')
        self.sendline('/'.join(['sort',self.name,'index',order,'ikey',cycle]))
        self.sendline('reorder / '+self.name+' / ikey')
        self.sendline('cmo / DELATT / '+self.name+' / ikey')
    def trans(self, xold, xnew, stride=(1,0,0)):
        ''' Translate mesh according to old coordinates "xold" to new coordinates "xnew"

        :param xold: old position
        :type xold: tuple(float,float,float)
        :param xnew: new position
        :type xnew: tuple(float,float,float)
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int,int,int)
        '''
        xold = [str(v) for v in xold]
        xnew = [str(v) for v in xnew]
        stride = [str(v) for v in stride]
        cmd = '/'.join(['trans',','.join(stride),','.join(xold),','.join(xnew)])
        self.sendline(cmd)
    def rotateln(self,coord1,coord2,theta,center=[0,0,0],copy=False,stride=(1,0,0)):
        ''' 
        Rotates a point distribution (specified by ifirst,ilast,istride) about a line. 
        The copy option allows the user to make a copy of the original points as well 
        as the rotated points, while copy=False just keeps the rotated points themselves. 
        The line of rotation defined by coord1 and coord2 needs to be defined such that 
        the endpoints extend beyond the point distribution being rotated. theta (in degrees) 
        is the angle of rotation whose positive direction is determined by the right-hand-rule, 
        that is, if the thumb of your right hand points in the direction of the line 
        (1 to 2), then your fingers will curl in the direction of rotation. center is the point 
        where the line can be shifted to before rotation takes place. 
        If the copy option is chosen, the new points will have only coordinate values 
        (xic, yic, zic); no values will be set for any other mesh object attribute for these points.
        Note:  The end points of the  line segment must extend well beyond the point set being rotated.

        Example 1:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> x = numpy.arange(0,10.1,1)
            >>> y = x
            >>> z = [0,1]
            >>> lg = PyLaGriT()
            >>> mqua = lg.gridder(x,y,z,elem_type='hex',connect=True)
            >>> mqua.rotateln([mqua.xmin-0.1,0,0],[mqua.xmax+0.1,0,0],25)
            >>> mqua.dump_exo('rotated.exo')
            >>> mqua.dump_ats_xml('rotated.xml','rotated.exo')
            >>> mqua.paraview()

        Example 2:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> x = numpy.arange(0,10.1,1)
            >>> y = [0,1]
            >>> #z = [0,1]
            >>> lg = PyLaGriT()
            >>> layer = lg.gridder(x=x,y=y,elem_type='quad',connect=True)
            >>> layer.rotateln([0,layer.ymin-0.10,0],[0,layer.ymax+0.1,0],25)
            >>> layer.dump('tmp_lay_top.inp')
            >>> # Layer depths?
            >>> #           1   2   3    4    5    6    7   8    9   10
            >>> layers = [ .1, 1.]
            >>> addnum = [  4, 2]
            >>> #matnum = [2]*len(layers)
            >>> matnum = [2, 1]
            >>> layer_interfaces = numpy.cumsum(layers)
            >>> mtop = layer.copy()
            >>> stack_files = ['tmp_lay_top.inp 1,9']
            >>> #stack_files.append('tmp_lay_peat_bot.inp 1,33')
            >>> i = 1
            >>> for li,m,a in zip(layer_interfaces,matnum,addnum):
            >>>     layer.math('sub',li,'zic',cmosrc=mtop)
            >>>     stack_files.append('tmp_lay'+str(i)+'.inp '+str(int(m))+', '+str(a))
            >>>     layer.dump('tmp_lay'+str(i)+'.inp')
            >>>     i += 1
            >>> layer.math('sub',2,'zic',cmosrc=mtop)
            >>> #layer.setatt('zic',-2.)
            >>> layer.dump('tmp_lay_bot.inp')
            >>> stack_files.append('tmp_lay_bot.inp 2')
            >>> stack_files.reverse()
            >>> # Create stacked layer mesh and fill
            >>> stack = lg.create()
            >>> stack.stack_layers('avs',stack_files,flip_opt=True)
            >>> stack_hex = stack.stack_fill()
            >>> stack_hex.dump_exo('rotated.exo')
            >>> stack_hex.dump_ats_xml('rotated.xml','rotated.exo')
            >>> stack_hex.paraview()
        '''
        stride = [str(v) for v in stride]
        coord1 = [str(v) for v in coord1]
        coord2 = [str(v) for v in coord2]
        center = [str(v) for v in center]
        if copy: copystr = 'copy'
        else: copystr = 'nocopy'
        self.sendline('/'.join(['rotateln',','.join(stride),copystr,','.join(coord1),','.join(coord2),str(theta),','.join(center)]))

    def massage(self,bisection_len,merge_len,toldamage,tolroughness=None,stride=None,
                nosmooth=False,norecon=False,strictmergelength=False,checkaxy=False,
                semiexclusive=False,ignoremats=False,lite=False):
        '''
        MASSAGE creates, annihilates, and moves nodes and swaps connections in a 2D or 3D mesh
        in order to improve element aspect ratios and establish user-desired edge lengths.

        The actions of MASSAGE are controlled by values of these four parameters:

            bisection_length  - edge length that will trigger bisection.
            merge_length - edge length that will trigger merging.
            toldamage - maximum grid deformation of interfaces and external boundaries
                        allowed in a single merge, smooth or reconnection event.
            tolroughness - (for 2D surface grids only)  measure of grid roughness
                           (deviation from average surface normal) that triggers refinement.

        The final, optional keywork argument(s) can be one or more of nosmooth, norecon, lite,
        ignoremats, strictmergelength, checkaxy, semiexclusive, and exclusive.  

        Specifying nosmooth will turn off the 'smooth' step by skipping the call to SGD.
        Specifying norecon will turn off all 'recon' steps.
        If lite is specified, only one iteration of the merging/reconnection/smoothing
        loop is executed, and a reconnection after edge refinement is omitted. 
        This is suitable for applications, such as Gradient Weighted Moving Finite
        Elements, where MASSAGE is called repeatedly.

        The optional argument ignoremats causes MASSAGE to process the multimaterial
        mesh in a single material mode; it ignores the material interfaces. 

        The optional argument strictmergelength forces strict interpretation of
        merge_length so that there is no merging along the edges of flat elements.
        This is important if ignoremats is specified to avoid losing the interfaces.

        If checkaxy is given, then we insure that for 2D meshes, the output mesh
        will have positive xy-projected triangle areas, provided that the input mesh
        had them in the first place. 

        If exclusive is given, then edge refinement operations will only be performed
        on edges whose endpoints are both in the PSET that MASSAGE is working on.
        (As usual, new nodes created by refinement are added to the PSET so that MASSAGE
        can refine edges recursively.)  The default behavior is 'inclusive',
        where only ONE edge endpoint has to belong to the PSET for the edge to be
        eligible for refinement.

        If semiexclusive is given, refinement will only be triggered by edges with
        both endpoints in the PSET, but some edges with less than two endpoints in
        the PSET might be refined as part of a 'Rivara chain' triggered by the refinement
        of an edge with both endpoints in the PSET.  This represents an intermediate
        case between 'inclusive' and exclusive
        '''

        cmd = ['massage',str(bisection_len),str(merge_len),str(toldamage)]

        if tolroughness is not None:
            cmd.append(str(tolroughness)) 
        if stride is not None:
            stride = [str(x) for x in stride]
            cmd.append(','.join(stride))

        # Add optional boolean arguments
        _iter = zip(['nosmooth','norecon','strictmergelength','checkaxy','semiexclusive',
                     'ignoremats','lite'],[nosmooth,norecon,strictmergelength,checkaxy,
                      semiexclusive,ignoremats,lite])
        [cmd.append(c[0]) for c in _iter if c[1]]
        self.sendline('/'.join(cmd))

    def massage2(self,filename,min_scale,bisection_len,merge_len,toldamage,
                tolroughness=None,stride=None,nosmooth=False,norecon=False,
                strictmergelength=False,checkaxy=False,semiexclusive=False,
                ignoremats=False,lite=False):
        '''
        MASSAGE2 iteratively calls MASSAGE to refine adaptively according to a
        gradient field. Thus, the bisection_length option must be a field.

        file_name is a file which contains a set of LaGriT commands that
        calculates the gradient field based on the distance field. In other
        words, the gradient field is a function of the distance field.
        It is necessary to have this file when using this routine, as the field
        must be updated after each refinement iteration.

        Use this function in conjunction with PyLaGriT.define(**kwargs) for 
        best results.

        See MASSAGE for other arguments.
        '''

        cmd = ['massage2',filename,str(min_scale),str(bisection_len),str(merge_len),str(toldamage)]
        if tolroughness is not None:
            cmd.append(str(tolroughness)) 
        if stride is not None:
            stride = [str(x) for x in stride]
            cmd.append(','.join(stride)) 

        # Add optional boolean arguments
        _iter = zip(['nosmooth','norecon','strictmergelength','checkaxy','semiexclusive',
                     'ignoremats','lite'],[nosmooth,norecon,strictmergelength,checkaxy,
                      semiexclusive,ignoremats,lite])
        [cmd.append(c[0]) for c in _iter if c[1]]
        self.sendline('/'.join(cmd))

    def perturb(self,xfactor,yfactor,zfactor,stride=(1,0,0)):
        '''
        This command moves node coordinates in the following manner.

        Three pairs of random numbers between 0 and 1 are generated.
        These pairs refer to the x, y and z coordinates of the nodes respectively.
        The first random number of each pair is multiplied by the factor given in
        the command. The second random number is used to determine
        if the calculated offset is to be added or subtracted from the coordinate.
        '''

        cmd = ['perturb',','.join([str(x) for x in stride]),str(xfactor),str(yfactor),str(zfactor)]
        self.sendline('/'.join(cmd))
    

    def upscale(self, method, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        The upscale command is used to interpolate attribute values from nodes of a fine source mesh to node
        attributes of a coarse sink mesh. The subroutine finds nodes of the fine source mesh within the Voronoi
        cell of every node in the coarser sink mesh. Nodes on cell boundaries are assigned to two or more sink
        nodes. Then the attributes of all the source nodes within a source node's cell are upscaled into a
        single value based on the chosen method. Mesh elements and connectivity are ignored and only node
        values are used to upscale values on to the sink mesh nodes. 

        :param method: Type of upscaling: sum, min, max, and averages ariave, harave, geoave
        :type method: str
        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src, defaults to name of attsink
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        stride = [str(v) for v in stride]
        if attsrc is None: attsrc = attsink
        cmd = ['upscale',method,self.name,attsink,','.join(stride),cmosrc.name,attsrc]
        opts = []
        if boundary_choice is not None: opts.append(boundary_choice)
        if keepatt: opts.append('keepatt')
        if set_id: opts.append('set_id')
        if len(opts) > 0: cmd.append(' '.join(opts))
        self.sendline('/'.join(cmd))

    def upscale_ariave(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using arithmetic average of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('ariave',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def upscale_geoave(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using geometric average of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('geoave',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def upscale_harave(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using harmonic average of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('harave',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def upscale_min(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using minimum of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('min',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def upscale_max(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using maximum of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('max',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def upscale_sum(self, attsink, cmosrc, attsrc=None, stride=(1,0,0), boundary_choice=None, keepatt=False,
                set_id=False):
        '''
        Upscale using sum of cmosrc points within Voronoi volumes of current mesh

        :param attsink: attribute sink
        :type attsink: str
        :param cmosrc: PyLaGriT mesh object source
        :type cmosrc: PyLaGriT Mesh Object
        :param attsrc: attribute src
        :type attsrc: str
        :param stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        :param boundary_choice: method of choice when source nodes are found on the boundary of multiple Voronoi volumes of sink nodes: single, divide, or multiple
        :type boundary_choice: str
        '''
        self.upscale('sum',attsink,cmosrc,attsrc,stride,boundary_choice,keepatt,set_id)

    def intersect_elements(self,mo2,attname=''):
        '''
        create attribute in mesh object of number of elements in mo2 that intersect element in current mesh object
        '''
        self.sendline('/'.join(['intersect_elements',self.name,mo2.name,attname]))
    def gmv(self,exe=None,filename=None):
        if filename is None: filename = self.name+'.gmv'
        if exe is not None: self._parent.gmv_exe = exe
        self.sendline('dump/gmv/'+filename+'/'+self.name)
        os.system(self._parent.gmv_exe+' -i '+filename)
    def paraview(self,exe=None,filename=None):
        if filename is None: filename = self.name+'.inp'
        if exe is not None: self._parent.paraview_exe = exe
        self.sendline('dump/avs/'+filename+'/'+self.name)
        os.system(self._parent.paraview_exe+' '+filename)
    def dump(self,filename=None,format=None,*args):
        if filename is None and format is None:
            print("Error: At least one of either filename or format option is required")
            return
        #if format is not None: cmd = '/'.join(['dump',format])
        #else: cmd = 'dump'
        if filename and format: 
            if format in ['fehm','zone_outside','zone_outside_minmax']: filename = filename.split('.')[0]
            if format is 'stor' and len(args)==0: filename = filename.split('.')[0]
            cmd = '/'.join(['dump',format,filename,self.name])
        elif format: 
            if format in ['avs','avs2']: filename = self.name+'.inp'
            elif format is 'fehm': filename = self.name
            elif format is 'gmv': filename = self.name+'.gmv'
            elif format is 'tecplot': filename = self.name+'.plt'
            elif format is 'lagrit': filename = self.name+'.lg'
            elif format is 'exo': filename = self.name+'.exo'
            cmd = '/'.join(['dump',format,filename,self.name])
        else:
            cmd = '/'.join(['dump',filename,self.name])
        for arg in args: cmd = '/'.join([cmd,str(arg)])
        self.sendline(cmd)
    def dump_avs2(self,filename,points=True,elements=True,node_attr=True,element_attr=True):
        '''
        Dump avs file

        :arg filename: Name of avs file
        :type filename: str
        :arg points: Output point coordinates
        :type points: bool
        :arg elements: Output connectivity
        :type elements: bool
        :arg node_attr: Output node attributes
        :type node_attr: bool
        :arg element_attr: Output element attributes
        :type element_attr: bool
        '''
        self.dump(filename,'avs2',int(points),int(elements),int(node_attr),int(element_attr))
    def dump_exo(self,filename,psets=False,eltsets=False,facesets=[]):
        '''
        Dump exo file

        :arg filename: Name of exo file
        :type filename: str
        :arg psets: Boolean indicating that exodus will only include psets
        :type psets: bool
        :arg eltsets: Boolean indicating that exodus will only include element sets
        :type eltsets: bool
        :arg facesets:  Array of FaceSet objects
        :type facesets: lst(FaceSet)

        Example:
            >>> from pylagrit import PyLaGriT
            >>> l = PyLaGriT()
            >>> m = l.create()
            >>> m.createpts_xyz((3,3,3),(0.,0.,0.),(1.,1.,1.),rz_switch=[1,1,1],connect=True)
            >>> m.status ()
            >>> m.status (brief=True)
            >>> fs = m.create_boundary_facesets(base_name='faceset_bounds')
            >>> m.dump_exo('cube.exo',facesets=fs.values())
        '''
        cmd = '/'.join(['dump/exo',filename,self.name])
        if psets: cmd = '/'.join([cmd,'psets'])
        else: cmd = '/'.join([cmd,' '])
        if eltsets: cmd = '/'.join([cmd,'eltsets'])
        else: cmd = '/'.join([cmd,' '])
        if len(facesets):
            cmd = '/'.join([cmd,'facesets'])
            for fc in facesets: 
                cmd += ' &\n'+fc.filename
        self.sendline(cmd)
    def dump_gmv(self,filename,format='binary'):
        self.dump(filename,'gmv',format)
    def dump_fehm(self,filename,*args):
        self.dump(filename,'fehm',*args)
    def dump_lg(self,filename,format='binary'):
        self.dump(filename,'lagrit',format)
    def dump_zone_imt(self,filename,imt_value):
        cmd = ['dump','zone_imt',filename,self.name,str(imt_value)]
        self.sendline('/'.join(cmd))
    def dump_zone_outside(self,filename,keepatt=False,keepatt_median=False,keepatt_voronoi=False):
        cmd = ['dump','zone_outside',filename,self.name]
        if keepatt: cmd.append('keepatt')
        if keepatt_median and keepatt_voronoi:
            print("Error: keepatt_median and keepatt_voronoi cannot both be True")
            return
        elif keepatt_median: cmd.append('keepatt_median')
        elif keepatt_voronoi: cmd.append('keepatt_voronoi')
        self.sendline('/'.join(cmd))
    def dump_ats_xml(self,filename,meshfilename,matnames={},facenames={}):
        '''
        Write ats style xml file with regions
        :param filename: Name of xml to write
        :type filename: string
        :param meshfilename: Name of exodus file to use in xml
        :type meshfilename: string
        :param matnames: Dictionary of region names keyed by exodus material number
        :type matnames: dict
        :param facenames: Dictionary of faceset names keyed by exodus faceset number
        :type facenames: dict
        '''
        main = ET.Element('ParameterList',{'name':'Main','type':'ParameterList'})

        ET.SubElement(main,'Parameter',{'name':'Native Unstructured Input','type':'bool','value':'true'})
        ET.SubElement(main,'Parameter',{'name':'grid_option','type':'string','value':'Unstructured'})

        mesh = ET.SubElement(main,'ParameterList',{'name':'Mesh','type':'ParameterList'})
        ET.SubElement(mesh,'Parameter',{'isUsed':'true','name':'Framework','type':'string','value':'MSTK'})

        mesh1 = ET.SubElement(mesh,'ParameterList',{'name':'Read Mesh File','type':'ParameterList'})
        ET.SubElement(mesh1,'Parameter',{'name':'File','type':'string','value':meshfilename})
        ET.SubElement(mesh1,'Parameter',{'name':'Format','type':'string','value':'Exodus II'})

        mesh2 = ET.SubElement(mesh,'ParameterList',{'name':'Surface Mesh','type':'ParameterList'})
        ET.SubElement(mesh2,'Parameter',{'name':'surface sideset name','type':'string','value':'surface'})
        mesh2a = ET.SubElement(mesh2,'ParameterList',{'name':'Expert','type':'ParameterList'})
        ET.SubElement(mesh2a,'Parameter',{'name':'Verify Mesh','type':'bool','value':'false'})

        r = ET.SubElement(main,'ParameterList',{'name':'Regions','type':'ParameterList'})

        r1 = ET.SubElement(r,'ParameterList',{'name':'computational domain','type':'ParameterList'})
        l1 = ET.SubElement(r1,'ParameterList',{'name':'Region: Box','type':'ParameterList'})
        ET.SubElement(l1,'Parameter',{'name':'Low Coordinate','type':'Array(double)','value':'{-1.e20,-1.e20,-1.e20}'})
        ET.SubElement(l1,'Parameter',{'name':'High Coordinate','type':'Array(double)','value':'{1.e20,1.e20,1.e20}'})

        r2 = ET.SubElement(r,'ParameterList',{'name':'surface domain','type':'ParameterList'})
        l2 = ET.SubElement(r2,'ParameterList',{'name':'Region: Box','type':'ParameterList'})
        ET.SubElement(l2,'Parameter',{'name':'Low Coordinate','type':'Array(double)','value':'{-1.e20,-1.e20}'})
        ET.SubElement(l2,'Parameter',{'name':'High Coordinate','type':'Array(double)','value':'{1.e20,1.e20}'})

        rmat = []
        lmat = []
        for k,v in matnames.items():
            rmat.append(ET.SubElement(r,'ParameterList',{'name':str(v),'type':'ParameterList'}))
            lmat.append(ET.SubElement(rmat[-1],'ParameterList',{'name':'Region: Labeled Set','type':'ParameterList'}))
            ET.SubElement(lmat[-1],'Parameter',{'name':'Label','type':'string','value':str(k)})
            ET.SubElement(lmat[-1],'Parameter',{'name':'File','type':'string','value':meshfilename})
            ET.SubElement(lmat[-1],'Parameter',{'name':'Format','type':'string','value':'Exodus II'})
            ET.SubElement(lmat[-1],'Parameter',{'name':'Entity','type':'string','value':'Cell'})

        rsurf = []
        lsurf = []
        for k,v in facenames.items():
            rsurf.append(ET.SubElement(r,'ParameterList',{'name':str(v),'type':'ParameterList'}))
            lsurf.append(ET.SubElement(rsurf[-1],'ParameterList',{'name':'Region: Labeled Set','type':'ParameterList'}))
            ET.SubElement(lsurf[-1],'Parameter',{'name':'Label','type':'string','value':str(k)})
            ET.SubElement(lsurf[-1],'Parameter',{'name':'File','type':'string','value':meshfilename})
            ET.SubElement(lsurf[-1],'Parameter',{'name':'Format','type':'string','value':'Exodus II'})
            ET.SubElement(lsurf[-1],'Parameter',{'name':'Entity','type':'string','value':'Face'})

        m_str = ET.tostring(main)
        m_reparsed = minidom.parseString(m_str)
        with open(filename, "w") as f:
                f.write(m_reparsed.toprettyxml(indent="  "))
    def dump_pset(self,filerootname,zonetype='zone',pset=[]):
        '''
        Dump zone file of psets
        :arg filerootname: rootname of files to create, pset name will be added to name
        :type filerootname: string
        :arg zonetype: Type of zone file to dump, 'zone' or 'zonn'
        :type zonetype: string
        :arg pset: list of psets to dump, all psets dumped if empty list
        :type pset: list[strings]
        '''
        if len(pset)==0:
            cmd = ['pset','-all-',zonetype,filerootname,'ascii']
            self.sendline('/'.join(cmd))
        else:
            for p in pset:
                cmd = ['pset',p.name,zonetype,filerootname+'_'+p.name,'ascii']
                self.sendline('/'.join(cmd))
    def delete(self):
        self.sendline('cmo/delete/'+self.name)
        del self._parent.mo[self.name]
    def create_boundary_facesets(self,stacked_layers=False,base_name=None,reorder=False,external=True):
        '''
        Creates facesets for each boundary and writes associated avs faceset file
        :arg base_name: base name of faceset files
        :type base_name: str
        :arg stacked_layers: if mesh is created by stack_layers, user layertyp attr to determine top and bottom
        :type stacked_layers: bool
        :arg reorder_on_meds: reorder nodes on cell medians, usually needed for exodus file
        :type reorder_on_meds: bool
        :returns: Dictionary of facesets
        '''
        if base_name is None: base_name = 'faceset_'+self.name
        mo_surf = self.extract_surfmesh(reorder=reorder,external=external)
        mo_surf.addatt('id_side',vtype='vint',rank='scalar',length='nelements')
        mo_surf.settets_normal()
        mo_surf.copyatt('itetclr','id_side')
        mo_surf.delatt('id_side')
        fs = OrderedDict()
        if stacked_layers:
            pbot = mo_surf.pset_attribute('layertyp',-1)
            ebot = pbot.eltset( membership='exclusive' )
        else:
            ebot = mo_surf.eltset_attribute('itetclr',1)
        fs['bottom'] = ebot.create_faceset(base_name+'_bottom.avs')
        if stacked_layers:
            ptop = mo_surf.pset_attribute('layertyp',-2)
            etop = ptop.eltset( membership='exclusive' )
        else:
            etop = mo_surf.eltset_attribute('itetclr',2)
        fs['top'] =  etop.create_faceset(base_name+'_top.avs')
        er = mo_surf.eltset_attribute('itetclr',3)
        fs['right'] = er.create_faceset(base_name+'_right.avs')
        eback = mo_surf.eltset_attribute('itetclr',4)
        fs['back'] = eback.create_faceset(base_name+'_back.avs')
        el = mo_surf.eltset_attribute('itetclr',5)
        fs['left'] = el.create_faceset(base_name+'_left.avs')
        ef = mo_surf.eltset_attribute('itetclr',6)
        fs['front'] = ef.create_faceset(base_name+'_front.avs')
        return fs
    def createpts(self, crd, npts, mins, maxs, rz_switch=(1,1,1), rz_value=(1,1,1), connect=False):
        '''
        Create and Connect Points
        
        :arg crd: Coordinate type of either 'xyz' (cartesian coordinates), 
                    'rtz' (cylindrical coordinates), or 
                    'rtp' (spherical coordinates).
        :type  crd: str
        :arg  npts: The number of points to create in line
        :type npts: tuple(int)
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(int, int, int)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(int, int, int)
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        
        '''
        
        npts = [str(v) for v in npts]
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        rz_switch = [str(v) for v in rz_switch]
        rz_value = [str(v) for v in rz_value]

        cmd = '/'.join(['createpts',crd,','.join(npts),','.join(mins),','.join(maxs),','.join(rz_switch),','.join(rz_value)])
        self.sendline(cmd)

        if connect:
            if self.elem_type.startswith(('tri','tet')):
                cmd = '/'.join(['connect','noadd'])
            else:
                cmd = '/'.join(['createpts','brick',crd,','.join(npts),'1,0,0','connect'])
            self.sendline(cmd)

    def createpts_xyz(self, npts, mins, maxs, rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        self.createpts('xyz',npts,mins,maxs,rz_switch,rz_value,connect=connect)
    def createpts_dxyz(self, dxyz, mins, maxs, clip='under', hard_bound='min',rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        '''
        Create and Connect Points to create an orthogonal hexahedral mesh. The
        vertex spacing is based on dxyz and the mins and maxs specified. mins
        (default, see hard_bound option) or maxs will be adhered to, while maxs
        (default) or mins will be modified based on the clip option to be
        truncated at the nearest value 'under' (default) or 'over' the range
        maxs-mins. clip and hard_bound options can be mixed by specifying tuples
        (see description below).

        :arg  dxyz: The spacing between points in x, y, and z directions
        :type dxyz: tuple(float,float,float)
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(float,float,float)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(float,float,float)
        :kwarg clip: How to handle bounds if range does not divide by dxyz, either clip 'under' or 'over' range
        :type clip: string or tuple(string,string,string)
        :kwarg hard_bound: Whether to use the "min" or "max" as the hard constraint on dimension
        :type hard_bound: string or tuple(string,string,string)
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        :kwarg connect: Whether or not to connect points
        :type  connect: boolean
        
        '''
        if isinstance(hard_bound,str): hard_bound = numpy.array([hard_bound,hard_bound,hard_bound])
        if isinstance(clip,str): clip = numpy.array([clip,clip,clip])
        dxyz = numpy.array(dxyz)
        mins = numpy.array(mins)
        maxs = numpy.array(maxs)
        dxyz[dxyz==0]=1
        npts = numpy.zeros_like(dxyz).astype('int')
        for i,cl in enumerate(clip):
            if cl == 'under': npts[i] = int(numpy.floor((maxs[i]-mins[i])/dxyz[i]))
            elif cl == 'over': npts[i] = int(numpy.ceil((maxs[i]-mins[i])/dxyz[i]))
            else:
                print("Error: unrecognized clip option")
                return
        for i,bnd in enumerate(hard_bound):
            if bnd == 'min': maxs[i] = mins[i] + npts[i]*dxyz[i]
            elif bnd == 'max': mins[i] = maxs[i] - npts[i]*dxyz[i]
            else:
                print("Error: unrecognized hard_bound option")
                return
        npts += 1
        npts.astype('int')
        self.createpts('xyz',npts,mins,maxs,rz_switch,rz_value,connect=connect)
        if self._parent.verbose:
            self.minmax_xyz()
    def createpts_rtz(self, npts, mins, maxs, rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        self.createpts('rtz',npts,mins,maxs,rz_switch,rz_value,connect=connect)
    def createpts_rtp(self, npts, mins, maxs, rz_switch=(1,1,1), rz_value=(1,1,1), connect=True):
        self.createpts('rtp',npts,mins,maxs,rz_switch,rz_value,connect=connect)
    def createpts_line(self, npts, mins, maxs, rz_switch=(1,1,1)):
        '''
        Create and Connect Points in a line
        
        :arg  npts: The number of points to create in line
        :type npts: int
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(int, int, int)
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(int, int, int)
        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio zoning values.  
        :type  rz_switch: tuple(int, int, int)
        
        '''
        
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        rz_switch = [str(v) for v in rz_switch]

        cmd = '/'.join(['createpts','line',str(npts),' ',' ',','.join(mins+maxs),','.join(rz_switch)])
        self.sendline(cmd)
    def createpts_brick(
            self, crd, npts, mins, maxs,  
            ctr=(1,1,1), rz_switch=(1,1,1), rz_vls=(1,1,1)
        ):
        '''
        Create and Connect Points
        
        Creates a grid of points in the mesh object and connects them. 
        
        :arg crd: Coordinate type of either 'xyz' (cartesian coordinates), 
                    'rtz' (cylindrical coordinates), or 
                    'rtp' (spherical coordinates).
        :type  crd: str
        
        :arg  npts: The number of points to create in each dimension.
        :type npts: tuple(int, int, int)
        
        :arg  mins: The starting value for each dimension.
        :type mins: tuple(int, int, int)
        
        :arg  maxs: The ending value for each dimension.
        :type maxs: tuple(int, int, int)
        
        :kwarg ctr: Defines the center of each cell. For 0, points are placed in
                    the middle of each cell. For 1, points are placed at the 
                    edge of each cell.
        :type  ctr: tuple(int, int, int)

        :kwarg rz_switch: Determines true or false (1 or 0) for using ratio 
                          zmoning values.  
        :type  rz_switch: tuple(int, int, int)
        
        :kwarg rz_vls: Ratio zoning values. Each point will be multiplied by
                       a scale of the value for that dimension.
        :type  rz_vls: tuple(int, int, int)
        '''
        
        ni, nj, nk = map(str, npts)
        mins = [float(v) for v in mins]
        maxs = [float(v) for v in maxs]
        xmn, ymn, zmn = map(str, mins)
        xmx, ymx, zmx = map(str, maxs)
        iiz, ijz, ikz = map(str, ctr)
        iirat, ijrat, ikrat = map(str, rz_switch)
        xrz, yrz, zrz = map(str, rz_vls)

        t = (crd, ni, nj, nk, xmn, ymn, zmn, xmx, ymx, zmx) 
        t = t + (iiz, ijz, ikz, iirat, ijrat, ikrat, xrz, yrz, zrz)
        cmd = 'createpts/brick/%s/%s,%s,%s/%s,%s,%s/%s,%s,%s/%s,%s,%s/'+\
              '%s,%s,%s/%s,%s,%s'
        self.sendline(cmd%t)

    def createpts_brick_xyz(
            self, npts, mins, maxs, 
            ctr=(1,1,1), rz_switch=(1,1,1), rz_vls=(1,1,1)):
        '''Create and connect Cartesian coordinate points.'''
        self.createpts_brick('xyz', **minus_self(locals()))

    def createpts_brick_rtz(
            self, npts, mins, maxs, 
            ctr=(1,1,1), rz_switch=(1,1,1), rz_vls=(1,1,1)):
        '''Create and connect cylindrical coordinate points.'''
        self.createpts_brick('rtz', **minus_self(locals()))
        
    def createpts_brick_rtp(
            self, npts, mins, maxs, 
            ctr=(1,1,1), rz_switch=(1,1,1), rz_vls=(1,1,1)):
        '''Create and connect spherical coordinates.'''
        self.createpts_brick(npts, **minus_self(locals()))
        
    def createpts_median(self):
        self.sendline('createpts/median')
    def subset(self, mins, maxs, geom='xyz'):
        '''
        Return Mesh Object Subset
        
        Creates a new mesh object that contains only a geometric subset defined
        by mins and maxs. 
        
        :arg  mins: Coordinate of one of the shape's defining points.
                     xyz (Cartesian):   (x1, y1, z1); 
                     rtz (Cylindrical): (radius1, theta1, z1);
                     rtp (Spherical):   (radius1, theta1, phi1);
        :typep mins: tuple(int, int, int)
        
        :arg  maxs: Coordinate of one of the shape's defining points.
                     xyz (Cartesian):   (x2, y2, z2); 
                     rtz (Cylindrical): (radius2, theta2, z2);
                     rtp (Spherical):   (radius2, theta2, phi2);
        :type maxs: tuple(int, int, int)
        
        :kwarg geom: Type of geometric shape: 'xyz' (spherical), 
                     'rtz' (cylindrical), 'rtp' (spherical)
        :type  geom: str
        
        Returns: MO object

        Example:
            >>> #To use pylagrit, import the module.
            >>> import pylagrit

            >>> #Start the lagrit session.
            >>> lg = pylagrit.PyLaGriT()

            >>> #Create a mesh object.
            >>> mo = lg.create()
            >>> mo.createpts_brick_xyz((5,5,5), (0,0,0), (5,5,5))

            >>> #Take the subset from (3,3,3)
            >>> mo.subset((3,3,3),(5,5,5))

        '''
        
        lg = self._parent
        new_mo = lg.copy(self)
        sub_pts = new_mo.pset_geom(mins, maxs, geom=geom)
        rm_pts = new_mo.pset_not(sub_pts)
        
        new_mo.rmpoint_pset(rm_pts)
        return new_mo
        
    def subset_xyz(self, mins, maxs):
        '''
        Return Tetrehedral MO Subset
        
        Creates a new mesh object that contains only a tetrehedral subset 
        defined by mins and maxs. 
        
        :arg  mins: Coordinate point of 1 of the tetrahedral's corners. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Coordinate point of 1 of the tetrahedral's corners.
        :type maxs: tuple(int, int, int)
        
        Returns: MO object
        '''
        return self.subset(geom='xyz', **minus_self(locals()))
        
    def quadxy(self,nnodes,pts):
        '''
        Define and connect an arbitrary, logical quad of points in 3D space
        with nnodes(x,y,z) nodes

        :arg nnodes: The number of nodes to create in each dimension. 
                      One value must == 1 and the other two must be > 1.
        :type nnodes: tuple(int, int, int)

        :arg pts: The four corners of the quad surface, defined in counter 
                   clockwise order (the normal to the quad points is defined
                   using the right hand rule and the order of the points).
        :arg pts:  list of 3-tuples (float)

        Example:
            >>> import pylagrit

            >>> #Start the lagrit session.
            >>> lg = pylagrit.PyLaGriT()

            >>> #Create a mesh object.
            >>> qua = lg.create_qua()
            
            >>> #Define 4 points in correct order
            >>> p1 = (0.0,200.0,-400.0)
            >>> p2 = (0.0,-200.0,-400.0)
            >>> p3 = (140.0,-200.0,0.0)
            >>> p4 = (118.0,200.0,0.0)
            >>> pts = [p1,p2,p3,p4]

            >>> #Define nnodes
            >>> nnodes = (29,1,82)

            >>> #Create and connect plane
            >>> qua.quadxy(nnodes,pts)
            
        '''
        self.select()
        quadpts = [n for n in nnodes if n != 1]
        assert len(quadpts) ==2, 'nnodes must be have one value == 1 and two values > 1'
        nnodes = [str(v) for v in nnodes]

        c = ''
        for v in pts:
            assert len(v) == 3,'vectors must be of length 3 (x,y,z)'
            c += '/'+','.join(list(map(str,v)))
        self.sendline('quadxy/%d,%d%s' % (quadpts[0],quadpts[1],c))

        cmd = '/'.join(['createpts','brick','xyz',','.join(nnodes),'1,0,0','connect'])
        self.sendline(cmd)

    def rzbrick(self,n_ijk,connect=True,stride=(1,0,0),coordinate_space='xyz'):
        '''
        Builds a brick mesh and generates a nearest neighbor connectivity matrix

        Currently only configured for this flavor of syntax:
         
            rzbrick/xyz|rtz|rtp/ni,nj,nk/pset,get,name/connect/

        Use this option with quadxyz to connect logically rectangular grids.

        :arg n_ijk: number of points to be created in each direction. 
        :type n_ijk: tuple
        :arg connect: connect points
        :type connect: bool
        :arg stride: Stride to select 
        :type stride: tuple
        :arg coordinate_space: xyz,rtz,or rtp coordinate spaces
        :type coordinate_space: str
        '''

        coordinate_space = coordinate_space.lower()
        assert coordinate_space in ['xyz','rtz','rtp'],'Unknown coordinate space'

        self.select()
        cmd = 'rzbrick/%s' % coordinate_space

        for v in [n_ijk,stride]:
            assert len(v) == 3,'vectors must be of length 3 (x,y,z)'
            cmd += '/'+','.join(list(map(str,v)))

        if connect:
            cmd += '/connect'

        self.sendline(cmd)

    def subset_rtz(self, mins, maxs):
        '''
        Return Cylindrical MO Subset
        
        Creates a new mesh object that contains only a cylindrical subset 
        defined by mins and maxs. 
        
        :arg  mins: Defines radius1, theta1, and z1. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Defines radius2, theta2, and z2.
        :type maxs: tuple(int, int, int)
        
        Returns: MO object
        '''
        return self.subset(geom='rtz', **minus_self(locals()))
        
    def subset_rtp(self, mins, maxs):
        '''
        Return Spherical MO Subset
        
        Creates a new mesh object that contains only a spherical subset 
        defined by mins and maxs. 
        
        :arg  mins: Defines radius1, theta1, and phi1. 
        :type mins: tuple(int, int, int)
        
        :arg  maxs: Defines radius2, theta2, and phi2.
        :type maxs: tuple(int, int, int)
        
        Returns: MO object
        '''
        return self.subset(geom='rtp', **minus_self(locals()))

    def grid2grid(self, ioption, name=None):
        '''
        Convert a mesh with one element type to a mesh with another
        
        :arg ioption: type of conversion:
            quadtotri2   quad to 2 triangles, no new points. 
            prismtotet3   prism to 3 tets, no new points. 
            quadtotri4   quad to 4 triangles, with one new point. 
            pyrtotet4   pyramid to 4 tets, with one new point. 
            hextotet5   hex to 5 tets, no new points. 
            hextotet6   hex to 6 tets, no new points. 
            prismtotet14   prism to 14 tets, four new points (1 + 3 faces). 
            prismtotet18   prism to 18 tets, six new points (1 + 5 faces). 
            hextotet24   hex to 24 tets, seven new points (1 + 6 faces). 
            tree_to_fe   quadtree or octree grid to grid with no parent-type elements. 
        :type option: str
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''
        if name is None: name = make_name('mo',self._parent.mo.keys())
        cmd = '/'.join(['grid2grid',ioption,name,self.name])
        self.sendline(cmd)
        self._parent.mo[name] = MO(name,self._parent)
        return self._parent.mo[name]
    def grid2grid_tree_to_fe(self, name=None):
        '''
        Quadtree or octree grid to grid with no parent-type elements. 
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='tree_to_fe', **minus_self(locals()))
    def grid2grid_quadtotri2(self, name=None):
        '''
        Quad to 2 triangles, no new points. 
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='quadtotri2', **minus_self(locals()))
    def grid2grid_prismtotet3(self, name=None):
        '''
        Quad to 2 triangles, no new points. 
        Prism to 3 tets, no new points. 
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='prismtotet3', **minus_self(locals()))
    def grid2grid_quadtotri4(self, name=None):
        '''
        Quad to 4 triangles, with one new point
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='quadtotri4', **minus_self(locals()))
    def grid2grid_pyrtotet4(self, name=None):
        '''
        Pyramid to 4 tets, with one new point
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='pyrtotet4', **minus_self(locals()))
    def grid2grid_hextotet5(self, name=None):
        '''
        Hex to 5 tets, no new points
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='hextotet5', **minus_self(locals()))
    def grid2grid_hextotet6(self, name=None):
        '''
        Hex to 6 tets, no new points
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='hextotet6', **minus_self(locals()))
    def grid2grid_prismtotet14(self, name=None):
        '''
        Prism to 14 tets, four new points (1 + 3 faces)
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='prismtotet14', **minus_self(locals()))
    def grid2grid_prismtotet18(self, name=None):
        '''
        Prism to 18 tets, four new points (1 + 3 faces)
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='prismtotet18', **minus_self(locals()))
    def grid2grid_hextotet24(self, name=None):
        '''
        Hex to 24 tets, seven new points (1 + 6 faces)
        :arg name: Internal Lagrit name of new mesh object, automatically created if None
        :type name: str

        Returns MO object
        '''        
        return self.grid2grid(ioption='hextotet24', **minus_self(locals()))
    def connect(self, option1='delaunay', option2=None, stride=None, big_tet_coords=[]):
        '''
        Connect the nodes into a Delaunay tetrahedral or triangle grid.
        
        :arg option1: type of connect: delaunay, noadd, or check_interface
        :type option: str
        :arg option2: type of connect: noadd, or check_interface
        :type option: str
        :arg stride: tuple of (first, last, stride) of points
        :type stride: tuple(int)
        '''
        cmd = ['connect',option1]
        if stride is not None and option is 'delaunay': 
            stride = [str(v) for v in stride]
            cmd += [','.join(stride)]
            for b in big_tet_coords:
                bs = [str(v) for v in b]
                cmd += [','.join(bs)]
        if option2 is not None:
                cmd += [option2]
        cmd = '/'.join(cmd)
        self.sendline(cmd)
    def connect_delaunay(self, option2=None, stride=None, big_tet_coords=[]):
        '''
        Connect the nodes into a Delaunay tetrahedral or triangle grid without adding nodes.
        '''
        mo_tmp = self.copypts()
        mo_tmp.setatt('imt',1)
        mo_tmp.setatt('itp',0)
        mo_tmp.rmpoint_compress(filter_bool=True)
        mo_tmp.connect(option1='delaunay', option2=option2,stride=stride,big_tet_coords=big_tet_coords)
        self.sendline('/'.join(['cmo','delete',self.name]))
        self.sendline('/'.join(['cmo','move',self.name,mo_tmp.name]))
        mo_tmp.delete()
        #self.connect(option1='delaunay', **minus_self(locals()))
    def connect_noadd(self):
        '''
        Connect the nodes into a Delaunay tetrahedral or triangle grid without adding nodes.
        '''
        self.connect(option1='noadd')
    def connect_check_interface(self):
        '''
        Connect the nodes into a Delaunay tetrahedral or triangle grid
        exhaustively checking that no edges of the mesh cross a material
        boundary.
        '''
        self.connect(option1='check_interface')
    def copypts(self, elem_type='tet', name=None):
        '''
        Copy points from mesh object to new mesh object

        :arg name: Name to use within lagrit for the created mesh object
        :type name: str
        :arg mesh_type: Mesh type for new mesh
        :type mesh_type: str
        :returns: mesh object
        '''
        if name is None: name = make_name('mo',self._parent.mo.keys())
        mo_new = self._parent.create(elem_type=elem_type,name=name)
        self.sendline('/'.join(['copypts',mo_new.name,self.name]))
        return mo_new
    def extrude(self, offset, offset_type='const', return_type='volume', direction=[], name=None):
        '''
        Extrude mesh object to new mesh object
        This command takes the current mesh object (topologically 1d or 2d mesh (a line, a set of line 
        segments, or a planar or non-planar surface)) and extrudes it into three 
        dimensions along either the normal to the curve or surface (default), 
        along a user defined vector, or to a set of points that the user has specified.
        If the extrusion was along the normal of the surface or along a user 
        defined vector, the command can optionally find the external surface of 
        the volume created and return that to the user.
        Refer to http://lagrit.lanl.gov/docs/commands/extrude.html for more details on arguments.


        :arg name: Name to use within lagrit for the created mesh object
        :type name: str
        :arg offset: Distance to extrude
        :type offset: float
        :arg offset_type: either const or min (interp will be handled in the PSET class in the future)         
        :type offset_type: str
        :arg return_type: either volume for entire mesh or bubble for just the external surface
        :type return_type: str
        :arg direction: Direction to extrude in, defaults to normal of the object
        :type direction: lst[float,float,float]
        :returns: mesh object
        '''
        if name is None: name = make_name('mo',self._parent.mo.keys())
        cmd = ['extrude',name,self.name,offset_type,str(offset),return_type]
        if len(direction) == 3: cmd.append(','.join([str(v) for v in direction]))
        self.sendline('/'.join(cmd))
        self._parent.mo[name] = MO(name, self._parent)
        return self._parent.mo[name]
    def refine_to_object(self, mo, level=None, imt=None, prd_choice=None):
        '''
        Refine mesh at locations that intersect another mesh object

        :arg mo: Mesh object to intersect with current mesh object to determine where to refine
        :type mo: PyLaGriT mesh object
        :arg level: max level of refinement
        :type level: int
        :arg imt: Value to assign to imt (LaGriT material type attribute)
        :type imt: int
        :arg prd_choice: directions of refinement
        :type prd_choice: int
        '''

        itetlevbool = True
        if level == 1: itetlevbool = False
        if level is None: level = 1; itetlevbool = False
        for i in range(level):
            attr_name = self.intersect_elements(mo)
            if itetlevbool:
                e_attr = self.eltset_attribute(attr_name,0,boolstr='gt')
                e_level = self.eltset_attribute('itetlev',level,boolstr='lt')
                e_refine = self.eltset_bool( [e_attr,e_level], boolstr='inter' )
                e_attr.delete()
                e_level.delete()
            else:
                e_refine = self.eltset_attribute(attr_name,0,boolstr='gt')
            if prd_choice is not None:
                p_refine = e_refine.pset()
                p_refine.refine(prd_choice=prd_choice)
                p_refine.delete()
            else:
                e_refine.refine()
            e_refine.delete()
        if imt is not None: 
            attr_name = self.intersect_elements(mo)
            e_attr = self.eltset_attribute(attr_name,0,boolstr='gt')
            p = e_attr.pset()
            p.setatt('imt',13)
            p.delete()
        

    def intersect_elements(self, mo, attr_name='attr00'):
        '''
        This command takes two meshes and creates an element-based attribute in mesh1 
        that contains the number of elements in mesh2 that intersected the respective 
        element in mesh1. We define intersection as two elements sharing any common point.
        
        :arg mo: Mesh object to intersect with current mesh object to determine where to refine
        :type mo: PyLaGriT mesh object
        :arg attr_name: Name to give created attribute 
        :type attr_name: str
        :returns: attr_name
        '''
        self.sendline('/'.join(['intersect_elements',self.name,mo.name,attr_name]))
        return attr_name
    def extract_surfmesh(self,name=None,stride=[1,0,0],reorder=False,resetpts_itp=True,external=False):
        return self._parent.extract_surfmesh( name=name,cmo_in=self,stride=stride,reorder=reorder,resetpts_itp=resetpts_itp,external=external )
    def interpolate(self,method,attsink,cmosrc,attsrc,stride=[1,0,0],tie_option=None,flag_option=None,keep_option=None,interp_function=None):
        '''
        Interpolate values from attribute attsrc from mesh object cmosrc to current mesh object
        '''
        stride = [str(v) for v in stride]
        cmd = ['interpolate',method,self.name,attsink,','.join(stride),cmosrc.name,attsrc]
        if tie_option is not None: cmd += [tie_option]
        if flag_option is not None: cmd += [flag_option]
        if keep_option is not None: cmd += [keep_option]
        if interp_function is not None: cmd.append(interp_function)
        self.sendline('/'.join(cmd))
    def interpolate_voronoi(self,attsink,cmosrc,attsrc,stride=[1,0,0],interp_function=None):
        self.interpolate('voronoi',**minus_self(locals()))
    def interpolate_map(self,attsink,cmosrc,attsrc,stride=[1,0,0],tie_option=None,
                    flag_option=None,keep_option=None,interp_function=None):
        self.interpolate('map',**minus_self(locals()))
    def interpolate_continuous(self,attsink,cmosrc,attsrc,stride=[1,0,0],interp_function=None,nearest=None):
        stride = [str(v) for v in stride]
        cmd = ['intrp','continuous',self.name+' '+attsink,','.join(stride),cmosrc.name+' '+attsrc]
        if nearest is not None: cmd += ['nearest',nearest]
        if interp_function is not None: cmd.append(interp_function)
        print('/'.join(cmd))
        self.sendline('/'.join(cmd))
    def interpolate_default(self,attsink,cmosrc,attsrc,stride=[1,0,0],tie_option='tiemax',
                    flag_option='plus1',keep_option='delatt',interp_function=None):
        self.interpolate('default',**minus_self(locals()))
    def copy(self,name=None):
        '''
        Copy mesh object
        '''
        if name is None: name = make_name('mo',self._parent.mo.keys())
        self.sendline('/'.join(['cmo/copy',name,self.name]))
        self._parent.mo[name] = MO(name,self._parent)
        return self._parent.mo[name]
    def stack_layers(self,filelist,file_type='avs',nlayers=None,matids=None,xy_subset=None,
                     buffer_opt=None,truncate_opt=None,
                     pinchout_opt=None,flip_opt=False,fill=False):
        if nlayers is None: nlayers = ['']*len(filelist-1)
        if matids is None: matids = [1]*len(filelist)
        cmd = ['stack/layers',file_type]
        if xy_subset is not None: cmd.append(xy_subset)
        cmd.append(' &')
        print(cmd)
        self.sendline('/'.join(cmd),expectstr='\r\n')
        self._parent.sendline(' '.join([filelist[0],str(matids[0]),'/ &']),expectstr='\r\n')
        for f,nl,md in zip(filelist[1:-1],nlayers[0:-1],matids[1:-1]): 
            self._parent.sendline(' '.join([f,str(md),str(nl),'/ &']),expectstr='\r\n')
        cmd = [' '.join([filelist[-1],str(matids[-1]),str(nlayers[-1])])]
        if flip_opt is True: cmd.append('flip')
        if buffer_opt is not None: cmd.append('buffer '+buffer_opt)
        if truncate_opt is not None: cmd.append('trun '+truncate_opt)
        if pinchout_opt is not None: cmd.append('pinch '+pinchout_opt)
        if not len(cmd) == 0: 
            self._parent.sendline('/'.join(cmd))
    def stack_fill(self,name=None):
        if name is None: name = make_name('mo', self._parent.mo.keys())
        self.sendline('/'.join(['stack/fill',name,self.name]))
        self._parent.mo[name] = MO(name, self._parent)
        return self._parent.mo[name]
    def math(self,operation,attsink,value=None,stride=[1,0,0],cmosrc=None,attsrc=None):
        stride = [str(v) for v in stride]
        if cmosrc is None: cmosrc = self
        if attsrc is None: attsrc = attsink
        cmd = ['math',operation,self.name,attsink,','.join(stride),cmosrc.name,attsrc]
        if value is not None:
            cmd += [str(value)]
        self.sendline('/'.join(cmd))
    def settets(self,method=None):
        if method is None:
            self.sendline('settets')
        else:
            self.sendline('settets/'+method)
    def settets_parents(self):
        self.settets('parents')
    def settets_geometry(self):
        self.settets('geometry')
    def settets_color_tets(self):
        self.settets('color_tets')
    def settets_color_points(self):
        self.settets('color_points')
    def settets_newtets(self):
        self.settets('newtets')
    def settets_normal(self):
        self.settets('normal')
    def triangulate(self,order='clockwise'):
        '''
        triangulate will take an ordered set of nodes in the current 2d mesh object that define a perimeter of a polygon and create a trangulation of the polygon.  The nodes are assumed to lie in the xy plane; the z coordinate is ignored.  No checks are performed to verify that the nodes define a legal perimeter (i.e. that segments of the perimeter do not cross).  The code will connect the last node to the first node to complete the perimeter. 

        This code support triangulation of self-intersecting polygons (polygon with holes), assuming that the order of the nodes are correct. Moreover the connectivity of the polyline must also be defined correctly. No checks are made. 

        One disadvantage of the algorithm for triangulating self-intersecting polygons is that it does not always work. For example, if the holes have complicated shapes, with many concave vertices, the code might fail. In this case, the user may try to rotate the order of the nodes: 
            NODE_ID: 
                1 -> 2 
                2 -> 3 
                ... 
                N -> 1 

            :param order: direction of point ordering
            :type order: string

            Example:
                >>> from pylagrit import PyLaGriT
                >>> # Create pylagrit object
                >>> lg = PyLaGriT()
                >>> # Define polygon points in clockwise direction
                >>> # and create tri mesh object
                >>> coords = [[0.0, 0.0, 0.0],
                >>>           [0.0, 1000.0, 0.0],
                >>>           [2200.0, 200.0, 0.0],
                >>>           [2200.0, 0.0, 0.0]]
                >>> motri = lg.tri_mo_from_polyline(coords)
                >>> # Triangulate polygon
                >>> motri.triangulate()
                >>> motri.setatt('imt',1)
                >>> motri.setatt('itetclr',1)
                >>> # refine mesh with successively smaller edge length constraints
                >>> edge_length = [1000,500,250,125,75,40,20,15]
                >>> for i,l in enumerate(edge_length):
                >>>     motri.resetpts_itp()
                >>>     motri.refine(refine_option='rivara',refine_type='edge',values=[l],inclusive_flag='inclusive')
                >>>     motri.smooth()
                >>>     motri.recon(0)
                >>> # provide additional smoothing after the last refine
                >>> for i in range(5):
                >>>     motri.smooth()
                >>>     motri.recon(0)
                >>> # create delaunay mesh and clean up
                >>> motri.tri_mesh_output_prep()
                >>> # dump fehm files
                >>> motri.dump_fehm('nk_mesh00')
                >>> # view results
                >>> motri.paraview() 

        '''
        self.sendline('triangulate/'+order)
    def refine(self,refine_option='constant',field=' ',interpolation=' ',refine_type='element',stride=[1,0,0],values=[1.],inclusive_flag='exclusive',prd_choice=None):
        stride = [str(v) for v in stride]
        values = [str(v) for v in values]
        if prd_choice is None:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,' '.join(stride),'/'.join(values),inclusive_flag])
        else:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,' '.join(stride),'/'.join(values),inclusive_flag,'amr '+str(prd_choice)])
        self._parent.sendline(cmd)
    def smooth(self,*args,**kwargs):
        if 'algorithm' not in kwargs: self.sendline('smooth')
        else:
            cmd = ['smooth','position',kwargs['algorithm']]
            for a in args: cmd.append(a)
            self.sendline('/'.join(cmd))
    def recon(self,option='',damage='',checkaxy=False):
        cmd = ['recon',str(option),str(damage)]
        if checkaxy: cmd.append('checkaxy')
        self.sendline('/'.join(cmd))
    def filter(self,stride=[1,0,0],tolerance=None,boolean=None,attribute=None):
        stride = [str(v) for v in stride]
        cmd = ['filter',' '.join(stride)]
        if tolerance is not None: cmd.append(tolerance)
        if boolean is not None and attribute is not None:
            cmd.append(boolean)
            cmd.append(attribute)
        elif (boolean is None and attribute is not None) or (boolean is not None and attribute is None):
            print('Error: Both boolean and attribute must be specified together')
            return
        self.sendline('/'.join(cmd))
    def tri_mesh_output_prep(self):
        '''
        Prepare tri mesh for output, remove dudded points,
        ensure delaunay volumes, etc.
        Combination of lagrit commands:
        filter/1 0 0
        rmpoint/compress
        recon/1
        resetpts/itp
        '''
        self.filter()
        self.rmpoint_compress()
        self.recon(1)
        self.resetpts_itp()
    def surface(self,name=None,ibtype='reflect'):
        if name is None:
            name = make_name('s',self._parent.surface.keys())
        cmd = '/'.join(['surface',name,ibtype,'sheet',self.name])
        self.sendline(cmd)
        self._parent.surface[name] = Surface(name,self._parent)
        return self._parent.surface[name]
    def region_bool(self,bool,name=None): 
        '''
        Create region using boolean string

        :param bool: String of boolean operations
        :type bool: str
        :param name: Internal lagrit name for mesh object
        :type name: string
        :returns: Region

        Example:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> lg = PyLaGriT()
            >>> # Read in mesh
            >>> motet = lg.read('tet_matclr.inp')
            >>> # fault coordinates in feet
            >>> cs = [[498000.,381946.,0.],
            >>>       [497197.,381946.,0.],
            >>>       [494019.,384890.,0.],
            >>>       [490326.,386959.,0.],
            >>>       [487822.,388599.,0.],
            >>>       [486337.,390755.,0.],
            >>>       [486337.,392000.,0.]]
            >>> # Convert to meters
            >>> cs = numpy.array(cs)/3.28
            >>> # Create surfaces of fault
            >>> ss = []
            >>> for p1,p2 in zip(cs[:-1],cs[1:]):
            >>>     p3 = p1.copy()
            >>>     p3[2] = -4000.
            >>>     ss.append(lg.surface_plane(p1,p2,p3))
            >>> # Create region by boolean operations of fault surfaces
            >>> boolstr = ''
            >>> for i,s in enumerate(ss):
            >>>     if not i == 0: boolstr += ' and '
            >>>     boolstr += 'le '+s.name
            >>> r = motet.region_bool(boolstr)
            >>> # Create pset from region
            >>> p = motet.pset_region(r)
            >>> # Change imt value for pset
            >>> p.setatt('imt',21)
            >>> motet.dump_zone_imt('tet_nefault',21)

        '''
        if name is None:
            name = make_name('r',self.region.keys())
        cmd = '/'.join(['region',name,bool])
        self.sendline(cmd)
        self.region[name] = Region(name,self)
        return self.region[name]

 
class Surface(object):
    ''' Surface class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
    def __repr__(self):
        return self.name
    def release(self):
        cmd = 'surface/'+self.name+'/release'
        self._parent.sendline(cmd)
        del self._parent.surface[self.name]

class PSet(object):
    ''' Pset class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
    def __repr__(self):
        return str(self.name)
    def delete(self):
        cmd = 'pset/'+self.name+'/delete'
        self._parent.sendline(cmd)
        del self._parent.pset[self.name]
    @property
    def xmin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[4].split()[1])
    @property
    def xmax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[4].split()[2])
    @property
    def xlength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return int(strarr[4].split()[4])
    @property
    def ymin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[5].split()[1])
    @property
    def ymax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[5].split()[2])
    @property
    def ylength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return int(strarr[5].split()[4])
    @property
    def zmin(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[6].split()[1])
    @property
    def zmax(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return float(strarr[6].split()[2])
    @property
    def zlength(self):
        self.minmax_xyz(verbose=False)
        strarr = self._parent._parent.before.splitlines()
        return int(strarr[6].split()[4])
    def minmax_xyz(self,stride=[1,0,0],verbose=True):
        cmd = '/'.join(['cmo/printatt',self._parent.name,'-xyz-','minmax','pset,get,'+self.name])
        self._parent.sendline(cmd,verbose=verbose)
    def minmax(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,pset=self.name,ptype='minmax')
    def list(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,pset=self.name,ptype='list')
    def setatt(self,attname,value):
        cmd = '/'.join(['cmo/setatt',self._parent.name,attname,'pset get '+self.name,str(value)])
        self._parent.sendline(cmd)
    def refine(self,refine_type='element',refine_option='constant',interpolation=' ',prange=[-1,0,0],field=' ',inclusive_flag='exclusive',prd_choice=None):
        prange = [str(v) for v in prange]
        if prd_choice is None:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,'pset get '+self.name,','.join(prange),inclusive_flag])
        else:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,'pset get '+self.name,','.join(prange),inclusive_flag,'amr '+str(prd_choice)])
        self._parent.sendline(cmd)
    def eltset(self,membership='inclusive',name=None):
        '''
        Create eltset from pset

        :arg membership: type of element membership, one of [inclusive,exclusive,face]
        :type membership: str
        :arg name: Name of element set to be used within LaGriT
        :type name: str
        :returns: PyLaGriT EltSet object
        '''
        if name is None: name = make_name('e',self._parent.eltset.keys())
        cmd = ['eltset',name,membership,'pset','get',self.name]
        self._parent.sendline('/'.join(cmd))
        self._parent.eltset[name] = EltSet(name,self._parent)
        return self._parent.eltset[name]
    def expand(self,membership='inclusive'):
        '''
        Add points surrounding pset to pset

        :arg membership: type of element membership, one of [inclusive,exclusive,face]
        :type membership: str
        '''
        e = self.eltset(membership=membership)
        self._parent.sendline('pset/'+self.name+'/delete')
        self = e.pset(name=self.name)
    def interpolate(self,method,attsink,cmosrc,attsrc,interp_function=None):
        '''
        Interpolate values from attribute attsrc from mesh object cmosrc to current mesh object
        '''
        self._parent.interpolate(method=method,attsink=attsink,stride=['pset','get',self.name],cmosrc=cmosrc,attsrc=attsrc,interp_function=interp_function)
    def interpolate_voronoi(self,attsink,cmosrc,attsrc,interp_function=None):
        self.interpolate('voronoi',**minus_self(locals()))
    def interpolate_map(self,attsink,cmosrc,attsrc,tie_option='tiemax',
                    flag_option='plus1',keep_option='delatt',interp_function=None):
        self.interpolate('map',**minus_self(locals()))
    def interpolate_continuous(self,attsink,cmosrc,attsrc,interp_function=None,nearest=None):
        cmd = ['intrp','continuous',self.name+' '+attsink,','.join(['pset','get',self.name]),cmosrc.name+' '+attsrc]
        if nearest is not None: cmd += ['nearest',nearest]
        if interp_function is not None: cmd.append(interp_function)
        self._parent.sendline('/'.join(cmd))
    def interpolate_default(self,attsink,cmosrc,attsrc,tie_option='tiemax',
                    flag_option='plus1',keep_option='delatt',interp_function=None):
        self.interpolate('default',**minus_self(locals()))
    def dump(self,filerootname,zonetype='zone'):
        '''
        Dump zone file of pset nodes
        :arg filerootname: rootname of files to create, pset name will be added to name
        :type filerootname: string
        :arg zonetype: Type of zone file to dump, 'zone' or 'zonn'
        :tpye zonetype: string
        '''
        cmd = ['pset',self.name,zonetype,filerootname+'_'+self.name,'ascii']
        self._parent.sendline('/'.join(cmd))

    def scale(self,scale_type='relative',scale_geom='xyz',scale_factor=[1,1,1],scale_center=[0,0,0]):
        '''
        Scale pset nodes by a relative or absolute amount
        :arg scale_type: Scaling type may be 'relative' or 'absolute'
        :type scale_type: string
        :arg scale_geom: May be one of the geometry types 'xyz' (Cartesian), 'rtz' (cylindrical), or 'rtp' (spherical)
        :type scale_geom: string
        :arg scale_factor: If scale_factor is relative, scaling factors are unitless multipliers. If absolute, scaling factors are constants added to existing coordinates.
        :type scale_factor: list
        :arg scale_center: Geometric center to scale from
        :type scale_center: list
        '''
        
        scale_type = scale_type.lower()
        scale_geom = scale_geom.lower()
        
        if scale_geom not in ['xyz', 'rtz', 'rtp']:
            print("ERROR: 'scale_geom' must be one of 'xyz', 'rtz', or 'rtp'")
            return
        
        if scale_type not in ['relative', 'absolute']:
            print("ERROR: 'scale_type' must be one of 'relative' or 'absolute'")
            return
        
        scale_factor = [str(v) for v in scale_factor]
        scale_center = [str(v) for v in scale_center]
        
        cmd = ['scale',','.join(['pset','get',self.name]),scale_type,scale_geom,','.join(scale_factor),','.join(scale_center)]
        self._parent.sendline('/'.join(cmd))

    def perturb(self,xfactor,yfactor,zfactor):
        '''
        This command moves node coordinates in the following manner.

        Three pairs of random numbers between 0 and 1 are generated.
        These pairs refer to the x, y and z coordinates of the nodes respectively.
        The first random number of each pair is multiplied by the factor given in
        the command. The second random number is used to determine
        if the calculated offset is to be added or subtracted from the coordinate.
        '''

        cmd = ['perturb',','.join(['pset','get',self.name]),str(xfactor),str(yfactor),str(zfactor)]
        self._parent.sendline('/'.join(cmd))
    
    def trans(self, xold, xnew):
        '''
        Translate points within a pset by the linear translation from (xold, yold, zold) to (xnew, ynew, znew)
        
        :arg xold: Tuple containing point (xold, yold, zold) to translate from
        :type xold: tuple
        :arg xnew: Tuple containing point (xnew, ynew, znew) to translate to
        :type xnew: tuple
        '''
        
        xold = [str(v) for v in xold]
        xnew = [str(v) for v in xnew]
        
        cmd = ['trans',','.join(['pset','get',self.name]),','.join(xold),','.join(xnew)]
        self._parent.sendline('/'.join(cmd))

class EltSet(object):
    ''' EltSet class'''
    def __init__(self, name, parent):
        self.name = name
        self.faceset = None
        self._parent = parent
    def __repr__(self):
        return str(self.name)
    def delete(self):
        cmd = 'eltset/'+self.name+'/delete'
        self._parent.sendline(cmd)
        del self._parent.eltset[self.name]
    def create_faceset(self,filename=None):
        if filename is None: filename = 'faceset_'+self.name+'.avs'
        motmpnm = make_name( 'mo_tmp',self._parent._parent.mo.keys() )
        self._parent._parent.sendline('/'.join(['cmo/copy',motmpnm,self._parent.name]))
        self._parent._parent.sendline('/'.join(['cmo/DELATT',motmpnm,'itetclr0']))
        self._parent._parent.sendline('/'.join(['cmo/DELATT',motmpnm,'itetclr1']))
        self._parent._parent.sendline('/'.join(['cmo/DELATT',motmpnm,'facecol']))
        self._parent._parent.sendline('/'.join(['cmo/DELATT',motmpnm,'idface0']))
        self._parent._parent.sendline('/'.join(['cmo/DELATT',motmpnm,'idelem0']))
        self._parent._parent.sendline('eltset / eall / itetclr / ge / 0')
        self._parent._parent.sendline('eltset/edel/not eall '+self.name)
        self._parent._parent.sendline('rmpoint / element / eltset get edel')
        self._parent._parent.sendline('rmpoint / compress')
        self._parent._parent.sendline('/'.join(['dump / avs2',filename,motmpnm,'0 0 0 2']))
        self._parent._parent.sendline('cmo / delete /'+motmpnm)
        self.faceset = FaceSet(filename,self)
        return self.faceset
    def minmax(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,eltset=self.name,ptype='minmax')
    def list(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,eltset=self.name,ptype='list')
    def refine(self,amr=''):
        '''
        Refine elements in the element set

        Example:
            >>> from pylagrit import PyLaGriT
            >>> import numpy
            >>> import sys
            >>> 
            >>> df = 0.0005 # Fault half aperture
            >>> lr = 7 # Levels of refinement
            >>> nx = 4 # Number of base mesh blocks in x direction
            >>> nz = 20 # Number of base mesh blocks in z direction
            >>> d_base = df*2**(lr+1) # Calculated dimension of base block
            >>> w = d_base*nx # Calculated width of model
            >>> d = d_base*nz # Calculated depth of model
            >>> 
            >>> lg = PyLaGriT()
            >>> 
            >>> # Create discrete fracture mesh
            >>> dxyz = numpy.array([d_base,d_base,0.])
            >>> mins = numpy.array([0.,-d,0.])
            >>> maxs = numpy.array([w,0,0])
            >>> mqua = lg.createpts_dxyz(dxyz,mins,maxs,'quad',hard_bound=('min','max','min'),connect=True)
            >>> 
            >>> for i in range(lr):
            >>>     prefine = mqua.pset_geom_xyz(mins-0.1,(0.0001,0.1,0))
            >>>     erefine = prefine.eltset()
            >>>     erefine.refine()
            >>>     prefine.delete()
            >>>     erefine.delete()
            >>> 
            >>> mtri = mqua.copypts('triplane')
            >>> mtri.connect()
            >>> # Make sure that not nodes are lost during connect
            >>> if 'The mesh is complete but could not include all points.' in lg.before:
            >>>     print 'Error: Lost some points during connect, not completing mesh and exiting workflow!'
            >>>     print ''
            >>>     sys.exit()
            >>> mtri.tri_mesh_output_prep()
            >>> mtri.reorder_nodes(cycle='xic yic zic')
            >>> pfault = mtri.pset_geom_xyz(mins-0.1,(0.0001,0.1,0))
            >>> psource = mtri.pset_geom_xyz(mins-0.1,mins+0.0001)
            >>> mtri.setatt('imt',1)
            >>> pfault.setatt('imt',10)
            >>> psource.setatt('imt',20)
            >>> 
            >>> mtri.paraview(filename='discrete_fracture.inp')
        '''
        cmd = '/'.join(['refine','eltset','eltset,get,'+self.name,'amr '+str(amr)])
        self._parent.sendline(cmd)

    def pset(self,name=None):
        '''
        Create a pset from the points in an element set
        :arg name: Name of point set to be used within LaGriT
        :type name: str
        :returns: PyLaGriT PSet object
        '''
        if name is None: name = make_name('p',self._parent.pset.keys())
        cmd = '/'.join(['pset',name,'eltset',self.name])
        self._parent.sendline(cmd)
        self._parent.pset[name] = PSet(name, self._parent)
        return self._parent.pset[name]
    def setatt(self,attname,value):
        cmd = '/'.join(['cmo/setatt',self._parent.name,attname,'eltset, get,'+self.name,str(value)])
        self._parent.sendline(cmd)

class Region(object):
    ''' Region class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
    def __repr__(self):
        return str(self.name)

class FaceSet(object):
    ''' FaceSet class'''
    def __init__(self, filename, parent):
        self.filename = filename
        self._parent = parent
    def __repr__(self):
        return str(self.filename)

def make_name( base, names ):
    i = 1
    name = base+str(i)
    while name in names:
        i += 1
        name = base+str(i)
    return name 
    
def minus_self(kvpairs):
    del kvpairs['self']
    return kvpairs

def zone_to_zonn(zonefile):
    zonnfile = os.path.splitext(zonefile)[0]+".zonn"
    with open(zonefile,'r') as fh:
        fh.readline()
        lns = fh.readlines()
    with open(zonnfile,'w') as fout:
        fout.write('zonn\n')
        for l in lns: fout.write(l)
