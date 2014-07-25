from pexpect import spawn
from subprocess import call, PIPE
import os
import glob

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
        '''Convert File(s)
        
        For each file of the pattern, creates a new file in the new_ft format. 
        The new files will be inside the directory that the LaGriT object was
        instantiated. The name of each file will be the same as the original 
        file with the extension changed to new_ft.
        
        Supports conversion from avs files.
        Supports conversion to gmv files.
         
        :param pattern: Unix style file pattern of files to be converted.
        :type  pattern: str
        
        :param new_ft: New format to convert files.
        :type  new_ft: str
        '''
        
        #Make sure I support the new filetype.
        if new_ft not in ['gmv']:
            raise ValueError('Conversion to %s not supported.'%new_ft)

        for rpath in glob.glob(pattern):
            #Set everything up for lagrit.
            path = os.path.abspath(rpath)
            fname = path[path.rfind('/')+1:path.rfind('.')]
            old_ft = path[path.rfind('.')+1:]
            
            #Check that I support the old filetype.
            if old_ft not in ['avs']:
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
    def pset_geom_xyz(self,mins,maxs,stride=[1,0,0],name=None):
        if name is None:
            name = make_name('p',self.pset.keys())
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        stride = [str(v) for v in stride]
        cmd = '/'.join(['pset',name,'geom/xyz',','.join(stride),','.join(mins),','.join(maxs)])
        self.sendline(cmd)
        self.pset[name] = PSet(name,self)
        return self.pset[name]
    def pset_geom_rtz(self,rtz1,rtz2,center=[0,0,0],stride=[1,0,0],name=None):
        if name is None:
            name = make_name('p',self.pset.keys())
        rtz1 = [str(v) for v in rtz1]
        rtz2 = [str(v) for v in rtz2]
        center = [str(v) for v in center]
        stride = [str(v) for v in stride]
        cmd = '/'.join(['pset',name,'geom/rtz',','.join(stride),','.join(rtz1),','.join(rtz2),','.join(center)])
        self.sendline(cmd)
        self.pset[name] = PSet(name,self)
        return self.pset[name]
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
        print cmd
        self.sendline(cmd)
    def delete(self):
        self.sendline('cmo/delete/'+self.name)

        

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






