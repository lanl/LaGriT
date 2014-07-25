import unittest
import pylagrit
import glob
import sys
import os
from contextlib import contextmanager

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
        
        #Convert all avs files to gmv with pylagrit.
        with suppress_stdout():
            lg.convert('test_convert/*.avs', 'gmv')
        
        #Checks that the generated files are the same as the compare files.
        for filename in glob.glob('test_convert/compare_*.gmv'):
            f1 = open(filename)
            f2 = open(filename[21:])        
            old_data = f1.readlines()
            new_data = f2.readlines()
            self.assertEqual(old_data, new_data)
            os.remove('test.gmv')
                      
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
    
    
    
