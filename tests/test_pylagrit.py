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
        
        Tests that pylagrit can convert a list of avs files to gmv.
        '''
        
        lg = self.lg      
        
        old_formats = ['avs', 'gmv']
        new_formats = ['avs', 'gmv']
        
        for old_ft, new_ft in itertools.product(old_formats, new_formats):
            #Convert all avs files to gmv with pylagrit
            with suppress_stdout():
                lg.convert('test_convert/*.%s'%old_ft, new_ft)
            
            if not os.path.isfile('test.%s'%new_ft): 
                raise OSError('Failed Conversion.')
                
            os.remove('test.%s'%new_ft)
          
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
    runner.run(suite)
    
    
    
