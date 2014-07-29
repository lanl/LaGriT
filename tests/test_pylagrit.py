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
        #Tests that pylagrit can read a lagrit script. 
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
            mo1 = lg.read('avs', 'contour_file.avs', name='cmo1')
            mo2 = lg.read('avs', 'contour_file.avs', name='cmo2')
            mo3 = lg.read('avs', 'contour_file.avs', name='cmo3')
            new_mo = lg.merge(mo1, mo2, mo3)        
        #Test that the merge created a new mesh object.
        if type(new_mo) is type(None):
            raise ValueError('The new mesh object was not created.')
            
    def test_define(self):
        'Tests that '
        lg = self.lg
        lg.define('x', '4.0')
        lg.define('y', '2.0')
        lg.define('z', '1.0/(x-y)')
        self.assertEqual(lg.values['z'], 0.5)
        
            
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
    suite.addTest(TestPyLaGriT('test_define'))
    runner.run(suite)
    
    
    
