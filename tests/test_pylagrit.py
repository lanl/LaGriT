import unittest
import pylagrit
import glob
import sys
import os
from contextlib import contextmanager
import itertools

class TestPyLaGriT(unittest.TestCase):
    '''
    A PyLagriT Test
    
    Represents a test of PyLaGriT functionality.
    '''

    def setUp(self):
        #Sets upt a lagrit object to be used during tests.
        with suppress_stdout():
            self.lg = pylagrit.PyLaGriT('/n/swdev/LAGRIT/bin/lagrit_lin')

    def test_read_script(self):
        '''
        Test the Read Script Function
        
        Tests that pylagrit can read a lagrit script. 
        '''
        
        lg = self.lg
        for f in glob.glob('test_scripts/*.lg'):
            with suppress_stdout():
                lg.read_script(f)
                
    def test_convert(self):
        '''
        Test the Conversion Function
        
        Tests that pylagrit can convert files from one format to a new one.
        '''
        
        lg = self.lg      
        
        old_formats = ['avs', 'gmv']
        new_formats = ['avs', 'gmv', 'exo']
        
        for old_ft, new_ft in itertools.product(old_formats, new_formats):
            #Convert all avs files to gmv with pylagrit
            with suppress_stdout():
                lg.convert('test_convert/*.%s'%old_ft, new_ft)
            
            #If the file did not get generated, assume fail.
            if not os.path.isfile('test.%s'%new_ft): 
                raise OSError('Failed Conversion.')         
            os.remove('test.%s'%new_ft)
            
    def test_merge(self):
        '''
        Tests the Merge Function
        
        Tests that pylagrit can merge 3 mesh objects and return a single mesh
        object.
        '''
        
        lg = self.lg
        with suppress_stdout():
            mo1 = lg.read('avs', 'contour_file.avs')
            mo2 = lg.read('avs', 'contour_file.avs')
            mo3 = lg.read('avs', 'contour_file.avs')
            new_mo = lg.merge(mo1, mo2, mo3)        
        #Test that the merge created a new mesh object.
        if type(new_mo) is type(None):
            raise ValueError('An expected mesh object was not created.')
                    
    def test_subset(self):
        '''
        Tests the Subset Function
        
        Tests that the subset function returns a mesh object.
        '''
        
        lg = self.lg
        mo = lg.read('avs', 'contour_file.avs')
        
        sub = lg.subset((0,0,0), (1,1,1))
        #Test that the merge created a new mesh object.
        if type(sub) is type(None):
            raise ValueError('An expected mesh object was not created.')
        
    def test_create(self):
        '''
        Tests the Create Function
        
        Tests that the create function returns a mesh object.
        '''
    
        lg = self.lg
        with suppress_stdout():
            mo1 = lg.create() 
            mo2 = lg.create_hex() 
            mo3 = lg.create(mesh='pri') 
        if any([type(x) is type(None) for x in [mo1, mo2, mo3]]):
            raise ValueError('An expected mesh object was not created.')
            
    def test_createpts_brick(self):
        '''
        Test the Create Points Function
        
        Tests that a mesh object can run the createpts method with fail.
        '''
        
        lg = self.lg
        with suppress_stdout():
            mo = lg.create()
            npts = (4, 4, 4)
            mins = (0, 0, 0)
            maxs = (4, 4, 4)
            mo.createpts_brick('xyz', npts, mins, maxs)    
         
          
@contextmanager
def suppress_stdout():
    #Utility to supress standard output.
    with open(os.devnull, 'w') as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout
                               
if __name__ == '__main__':    
    runner = unittest.TextTestRunner(verbosity=2)    
    suite = unittest.TestSuite()
    suite.addTest(TestPyLaGriT('test_read_script'))
    suite.addTest(TestPyLaGriT('test_convert'))
    suite.addTest(TestPyLaGriT('test_merge'))
    suite.addTest(TestPyLaGriT('test_create'))
    suite.addTest(TestPyLaGriT('test_createpts_brick'))
    runner.run(suite)
    
    
    
