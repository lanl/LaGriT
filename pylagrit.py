from pexpect import spawn
import os

class PyLaGriT(spawn):
    ''' Python lagrit class'''
    def __init__(self, exe, verbose=True, *args, **kwargs):
        super(PyLaGriT, self).__init__(exe, *args, **kwargs) 
        self.expect()
        self.mo = {}
        self.surface = {}
        self.region = {}
        self.verbose = verbose
        if verbose: print self.before
    def expect(self):
        super(PyLaGriT, self).expect('Enter a command') 
    def sendline(self, cmd, verbose=True):
        super(PyLaGriT, self).sendline(cmd) 
        self.expect()
        if verbose and self.verbose: print self.before
    def interact(self, escape_character='^'):
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
    def read(self,format,filename,mo_name=None,binary=False):
        # If format is lagrit, mo_name is irrelevant
        if format != 'lagrit':
            if mo_name is None:
                mo_name = make_name('mo',self.mo.keys())
            cmd = '/'.join(['read',format,filename,mo_name])
        else:
            cmd = '/'.join(['read',format,filename,'dum'])
        if binary: cmd = '/'.join([cmd,'binary'])
        self.sendline(cmd)
        # If format lagrit, cmo read in will not be set to mo_name
        if format == 'lagrit':
            self.sendline('cmo/status/brief', verbose=False)
            for line in self.before.split('\r\n'):
                if 'current-mesh-object' in line:
                    tmp_name = line.split(':')[1].strip()
                    if mo_name is None: mo_name = tmp_name
                    elif mo_name != tmp_name: 
                        self.sendline('cmo/copy/'+mo_name+'/'+tmp_name)
                        self.sendline('cmo/release/'+tmp_name)
        self.mo[mo_name] = MO(mo_name,self)
        return self.mo[mo_name]
    def surface_box(self,mins,maxs,name=None,ibtype='reflect'):
        if name is None:
            name = make_name('s',self.surface.keys())
        mins = [str(v) for v in mins]
        maxs = [str(v) for v in maxs]
        cmd = '/'.join(['surface',name,ibtype,'box',','.join(mins),','.join(maxs)])
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
    def rmpoint_pset(self,pset,itype='exclusive'):
        if isinstance(pset,PSet): name = pset.name
        elif isinstance(pset,str): name = pset
        else:
            print 'pset must be a string or object of class PSet'
            return
        cmd = "rmpoint/pset get "+name+'/'+itype
        self.sendline(cmd)
    def rmpoint_eltset(self,eltset):
        if isinstance(eltset,EltSet): name = eltset.name
        elif isinstance(eltset,str): name = eltset
        else:
            print 'eltset must be a string or object of class PSet'
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
    def gmv(self,exe,filename=None):
        if filename is None: filename = self.name+'.gmv'
        self.sendline('dump/gmv/'+filename+'/'+self.name)
        os.system(exe+' -i '+filename)
    def paraview(self,exe,filename=None):
        if filename is None: filename = self.name+'.inp'
        self.sendline('dump/avs/'+filename+'/'+self.name)
        os.system(exe+' '+filename)
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

class EltSet(object):
    ''' EltSet class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
    def __repr__(self):
        return str(self.name)
    def delete(self):
        cmd = 'pset/'+self.name+'/delete'
        self._parent.sendline(cmd)
        del self._parent.eltset[self.name]

class Region(object):
    ''' Region class'''
    def __init__(self, name, parent):
        self.name = name
        self._parent = parent
    def __repr__(self):
        return str(self.name)

def make_name( base, names ):
    i = 1
    name = base+str(i)
    while name in names:
        i += 1
        name = base+str(i)
    return name






