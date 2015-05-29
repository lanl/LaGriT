from pexpect import spawn
from subprocess import call, PIPE
import os
import glob
import re

class PyLaGriT(spawn):
    ''' Python lagrit class'''
    def __init__(self, lagrit_exe=None, verbose=True, batch=False, batchfile='pylagrit.lgi', gmv_exe=None, paraview_exe=None, *args, **kwargs):
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
                print "Unable to open "+batchfile+": {1}".format(e.strerror)
                print "Batch mode disabled"
                self.batch = False
            else:
                self.batchfile = batchfile
                self.fh.write('# PyLaGriT generated LaGriT script\n')
        else:
            super(PyLaGriT, self).__init__(self.lagrit_exe, timeout=300, *args, **kwargs) 
            self.expect()
            if verbose: print self.before
    def run_batch(self):
        self.fh.write('finish\n')
        self.fh.close()
        call(self.lagrit_exe+' <'+self.batchfile, shell=True, stdout=PIPE)
    def expect(self, expectstr='Enter a command'):
        if self.batch:
            print "expect disabled during batch mode"
        else:
            super(PyLaGriT, self).expect(expectstr) 
    def sendline(self, cmd, verbose=True, expectstr='Enter a command'):
        if self.batch:
            self.fh.write(cmd+'\n')
        else:
            super(PyLaGriT, self).sendline(cmd) 
            self.expect(expectstr=expectstr)
            if verbose and self.verbose: print self.before
    def interact(self, escape_character='^'):
        if self.batch:
            print "Interactive mode unavailable during batch mode"
        else:
            print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
            print "Entering interactive mode"
            print "To return to python terminal, type a '"+escape_character+"' character"
            print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
            print self.after
            super(PyLaGriT, self).interact(escape_character=escape_character) 
    def cmo_status(self, cmo=None, brief=False):
        cmd = 'cmo/status'
        if cmo: cmd += '/'+cmo 
        if brief: cmd += '/brief'
        self.sendline(cmd)
    def read(self,format,filename,name=None,binary=False):
        # If format is lagrit, name is irrelevant
        if format != 'lagrit':
            if name is None:
                name = make_name('mo',self.mo.keys())
            cmd = '/'.join(['read',format,filename,name])
        else:
            cmd = '/'.join(['read',format,filename,'dum'])
        if binary: cmd = '/'.join([cmd,'binary'])
        self.sendline(cmd)
        # If format lagrit, cmo read in will not be set to name
        if format == 'lagrit' and not self.batch:
            self.sendline('cmo/status/brief', verbose=False)
            for line in self.before.split('\r\n'):
                if 'current-mesh-object' in line:
                    tmp_name = line.split(':')[1].strip()
                    if name is None: name = tmp_name
                    elif name != tmp_name: 
                        self.sendline('cmo/copy/'+name+'/'+tmp_name)
                        self.sendline('cmo/release/'+tmp_name)
        self.mo[name] = MO(name,self)
        return self.mo[name]
    def addmesh(self, mo1, mo2, style='add', name=None, *args):
        if isinstance(mo1,MO): mo1name = mo1.name
        elif isinstance(mo1,str): mo1name = mo1
        else:
            print "ERROR: MO object or name of mesh object as a string expected for mo1"
            return
        if isinstance(mo2,MO): mo2name = mo2.name
        elif isinstance(mo2,str): mo2name = mo2
        else:
            print "ERROR: MO object or name of mesh object as a string expected for mo2"
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
            print "ERROR: PSet object or name of PSet object as a string expected for pset"
            return
        if isinstance(mo1,MO): mo1name = mo1.name
        elif isinstance(mo1,str): mo1name = mo1
        else:
            print "ERROR: MO object or name of mesh object as a string expected for mo1"
            return
        if isinstance(mo2,MO): mo2name = mo2.name
        elif isinstance(mo2,str): mo2name = mo2
        else:
            print "ERROR: MO object or name of mesh object as a string expected for mo2"
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
    def region_bool(self,bool,name=None): 
        if name is None:
            name = make_name('r',self.region.keys())
        cmd = '/'.join(['region',name,bool])
        self.sendline(cmd)
        self.region[name] = Region(name,self)
        return self.region[name]
    def _check_rc(self):
        # check if pyfehmrc file exists
        rc_wd1 = os.sep.join(os.getcwd())+os.sep+'.pylagritrc'
        rc_wd2 = os.sep.join(os.getcwd())+os.sep+'pylagritrc'
        rc_home1 = os.path.expanduser('~')+os.sep+'.pylagritrc'
        rc_home2 = os.path.expanduser('~')+os.sep+'pylagritrc'
        if os.path.isfile(rc_wd1): fp = open(rc_lib1)
        elif os.path.isfile(rc_wd2): fp = open(rc_lib2)
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
                    print 'WARNING: unrecognized .pylagritrc line \''+ln.strip()+'\''
            else:
                print 'WARNING: unrecognized .pylagritrc line \''+ln.strip()+'\''
    def extract_surfmesh(self,name=None,cmo_in=None,stride=[1,0,0],reorder=False):
        if name is None:
            name = make_name('mo',self.mo.keys())
        if cmo_in is not None:
            if not isinstance( cmo_in, MO):
                print "ERROR: MO object or name of mesh object as a string expected for cmo_in"
                return
        if reorder:
            cmo_in.sendline('createpts/median')
            self.sendline('/'.join(['sort',cmo_in.name,'index/ascending/ikey/itetclr zmed ymed zmed']))
            self.sendline('/'.join(['reorder',cmo_in.name,'ikey']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'xmed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'ymed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'zmed']))
            self.sendline('/'.join(['cmo/DELATT',cmo_in.name,'ikey']))
        stride = [str(v) for v in stride]
        self.sendline( '/'.join(['extract/surfmesh',','.join(stride),name,cmo_in.name]))
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
            
    def merge(self, *mesh_objs):
        '''
        Merge Mesh Objects
        
        Merges two or more mesh objects together and returns the combined mesh
        object.
        
        :param mesh_objs: An argument list of mesh objects.
        :type  mesh_objs: MO list
        
        Returns: MO.
        ''' 
        
        if len(mesh_objs) > 1:
            return reduce(self.addmesh_merge, mesh_objs)
        else:
            raise ValueError('Must provide at least two objects to merge.')
            
    def create(self, name=None, mesh='tet', npoints=0, nelements=0):
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
        self.sendline('cmo/create/%s/%i/%i/%s'%(name, npoints, nelements, mesh))
        self.mo[name] = MO(name, self)
        return self.mo[name]
        
    def create_tet(self, name=None, npoints=0, nelements=0):
        '''Create a tetrahedron mesh object.'''
        return self.create(mesh='tet', **minus_self(locals()))
        
    def create_hex(self, name=None, npoints=0, nelements=0):
        '''Create a hexagon mesh object.'''
        return self.create(mesh='hex', **minus_self(locals()))
        
    def create_pri(self, name=None, npoints=0, nelements=0):
        '''Create a prism mesh object.'''
        return self.create(mesh='pri', **minus_self(locals()))  
            
    def create_pyr(self, name=None, npoints=0, nelements=0):
        '''Create a pyramid mesh object.'''
        return self.create(mesh='pyr', **minus_self(locals()))
        
    def create_tri(self, name=None, npoints=0, nelements=0):
        '''Create a triangle mesh object.'''
        return self.create(mesh='tri', **minus_self(locals()))
        
    def create_qua(self, name=None, npoints=0, nelements=0):
        '''Create a quadrilateral mesh object.'''
        return self.create(mesh='qua', **minus_self(locals())) 
             
    def create_hyb(self, name=None, npoints=0, nelements=0):
        '''Create a hybrid mesh object.'''
        return self.create(mesh='hyb', **minus_self(locals()))
        
    def create_lin(self, name=None, npoints=0, nelements=0):
        '''Create a line mesh object.'''
        return self.create(mesh='lin', **minus_self(locals()))
        
    def create_triplane(self, name=None, npoints=0, nelements=0):
        '''Create a triplane mesh object.'''
        return self.create(mesh='triplane', **minus_self(locals()))
        
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
    
class MO(object):
    ''' Mesh object class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
        self.pset = {}
        self.eltset = {}
    def __repr__(self):
        return self.name
    def sendline(self,cmd):
        self._parent.sendline('cmo select '+self.name)
        self._parent.sendline(cmd)
    def status(self,brief=False):
        print self.name
        self._parent.cmo_status(self.name,brief=brief)
    def printatt(self,attname=None,stride=[1,0,0],pset=None,eltset=None,type='value'):
        stride = [str(v) for v in stride]
        if attname is None: attname = '-all-'
        if pset is None and eltset is None:
            cmd = '/'.join(['cmo/printatt',self.name,attname,type,','.join(stride)])
        else:
            if pset is not None:
                if isinstance(pset,PSet): setname = pset.name
                elif isinstance(pset,str): setname = pset
                else:
                    print "ERROR: PSet object or name of PSet object as a string expected for pset"
                    return
                cmd = '/'.join(['cmo/printatt',self.name,attname,type,','.join(['pset','get',setname])])
            if eltset is not None:
                if isinstance(eltset,EltSet): setname = eltset.name
                elif isinstance(eltset,str): setname = eltset
                else:
                    print "ERROR: EltSet object or name of EltSet object as a string expected for eltset"
                    return
                cmd = '/'.join(['cmo/printatt',self.name,attname,type,','.join(['eltset','get',setname])])
        self.sendline(cmd)
    def minmax(self,attname=None,stride=[1,0,0],pset=None):
        self.printatt(attname=attname,stride=stride,pset=None,type='minmax')
    def list(self,attname=None,stride=[1,0,0],pset=None):
        self.printatt(attname=attname,stride=stride,pset=pset,type='list')
    def setatt(self,attname,value,stride=[1,0,0]):
        stride = [str(v) for v in stride]
        cmd = '/'.join(['cmo/setatt',self.name,attname,','.join(stride),str(value)])
        self.sendline(cmd)
        
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
        :typep mins: tuple(int, int, int)
        
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
        Define PSet by Cylydrical Geometry
        
        Selects points from a Tetrahedral region.
        
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
        Define PSet by Cylydrical Geometry
        
        Selects points from a Tetrahedral region.
        
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
        
    def eltset_region(self,region,name=None):
        if name is None:
            name = make_name('e',self.pset.keys())
        if isinstance(region,Region): region_name = region.name
        elif isinstance(region,str): region_name = region
        else:
            print 'region must be a string or object of class Region'
            return
        cmd = '/'.join(['eltset',name,'region',region_name])
        self.sendline(cmd)
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def eltset_attribute(self,attribute_name,attribute_value,boolstr='eq',name=None):
        if name is None:
            name = make_name('e',self.pset.keys())
        cmd = '/'.join(['eltset',name,attribute_name,boolstr,str(attribute_value)])
        self.sendline(cmd)
        self.eltset[name] = EltSet(name,self)
        return self.eltset[name]
    def rmpoint_pset(self,pset,itype='exclusive'):
        if isinstance(pset,PSet): name = pset.name
        elif isinstance(pset,str): name = pset
        else:
            print 'p must be a string or object of class PSet'
            return
        cmd = 'rmpoint/pset,get,'+name+'/'+itype
        self.sendline(cmd)
    def rmpoint_eltset(self,eltset):
        if isinstance(eltset,EltSet): name = eltset.name
        elif isinstance(eltset,str): name = eltset
        else:
            print 'eltset must be a string or object of class EltSet'
            return
        cmd = 'rmpoint/element/eltset,get,'+name
        self.sendline(cmd)
    def rmpoint_compress(self):
        self.sendline('rmpoint/compress')
    def reorder_nodes(self, order='ascending',cycle='zic yic xic'):
        self.sendline('resetpts itp')
        self.sendline('/'.join(['sort',self.name,'index',order,'ikey',cycle]))
        self.sendline('reorder / '+self.name+' / ikey')
        self.sendline('cmo / DELATT / '+self.name+' / ikey')
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
    def dump(self,format,filename=None,*args):
        if filename: 
            if format in ['fehm','zone_outside','zone_outside_minmax']: filename = filename.split('.')[0]
            if format is 'stor' and len(args)==0: filename = filename.split('.')[0]
        else: 
            if format in ['avs','avs2']: filename = self.name+'.inp'
            elif format is 'fehm': filename = self.name
            elif format is 'gmv': filename = self.name+'.gmv'
            elif format is 'tecplot': filename = self.name+'.plt'
            elif format is 'lagrit': filename = self.name+'.lg'
            elif format is 'exo': filename = self.name+'.exo'
        cmd = '/'.join(['dump',format,filename,self.name])
        for arg in args: cmd = '/'.join(cmd,arg)
        self.sendline(cmd)
    def dump_exo(self,filename,psets=False,eltsets=False,facesets=[]):
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
    def delete(self):
        self.sendline('cmo/delete/'+self.name)
    
    def createpts_brick(
            self, crd, npts, mins, maxs,  
            ctr=(1,1,1), rz_switch=(0,0,0), rz_vls=(1,1,1)
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
            ctr=(1,1,1), rz_switch=(0,0,0), rz_vls=(1,1,1)):
        '''Create and connect Cartesian coordinate points.'''
        self.createpts_brick('xyz', **minus_self(locals()))
        
    def createpts_brick_rtz(
            self, npts, mins, maxs, 
            ctr=(1,1,1), rz_switch=(0,0,0), rz_vls=(1,1,1)):
        '''Create and connect cylindrical coordinate points.'''
        self.createpts_brick('rtz', **minus_self(locals()))
        
    def createpts_brick_rtp(
            self, npts, mins, maxs, 
            ctr=(1,1,1), rz_switch=(0,0,0), rz_vls=(1,1,1)):
        '''Create and connect spherical coordinates.'''
        self.createpts_brick(ntps, **minus_self(locals()))
        
    def pset_not(self, ps, name=None):
        '''
        Return PSet from Logical Not
        
        Defines and returns a PSet from points that are not inside the PSet, ps.
        '''
        
        #Generated a name if one is not specified.
        if name is None:
            name = make_name('p',self.pset.keys())
        
        #Create the new PSET in lagrit and the pylagrit object.
        cmd = 'pset/%s/not/%s'%(name, str(ps))
        self.sendline(cmd)
        self.pset[name] = PSet(name, self)
        
        return self.pset[name]
        
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
    def minmax(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,pset=self.name,type='minmax')
    def list(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,pset=self.name,type='list')
    def setatt(self,attname,value,stride=[1,0,0]):
        cmd = '/'.join(['cmo/setatt',self._parent.name,attname,'pset get '+self.name,str(value)])
        self._parent.sendline(cmd)
    def refine(self,refine_option,refine_type,interpolation=' ',prange=[-1,0,0],field=' ',inclusive_flag='exclusive',prd_choice=None):
        prange = [str(v) for v in prange]
        if prd_choice is None:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,'pset get '+self.name,','.join(prange),inclusive_flag])
        else:
            cmd = '/'.join(['refine',refine_option,field,interpolation,refine_type,'pset get '+self.name,','.join(prange),inclusive_flag,'amr '+str(prd_choice)])
        self._parent.sendline(cmd)

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
    def create_faceset(self,filename=None,reorder=True):
        if filename is None:
            filename = make_name( 'faceset', self.facesets.keys() )
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
        self._parent.printatt(attname=attname,stride=stride,eltset=self.name,type='minmax')
    def list(self,attname=None,stride=[1,0,0]):
        self._parent.printatt(attname=attname,stride=stride,eltset=self.name,type='list')

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






