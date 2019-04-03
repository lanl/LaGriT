'''
CONFIGURATION FILE FOR TINERATOR
'''

import logging

NOTHING     = 0
DEBUG       = 10
INFO        = 20
FULL        = 30
DEBUG_MODE  = False

remove_temp_files = True
log_file = None

# create log
log = logging.getLogger('tinerator')
log.setLevel(logging.INFO)

# create console handler and set level to debug
ch = logging.StreamHandler()
ch.setLevel(logging.INFO)

# create formatter
formatter = logging.Formatter('[%(asctime)s - %(funcName)20s()] %(levelname)s - %(message)s', "%H:%M:%S")

# add formatter to ch
ch.setFormatter(formatter)

# add ch to log
log.addHandler(ch)


'''
CONSTANTS - NOT FOR END-USER MODIFICATIONS
'''

def __isnotebook():
    '''
    Detects if package is being run in Jupyter
    notebook, so that interactive functions may be
    enabled.

    Function author: Gustavo Bezerra
    '''
    try:
        shell = get_ipython().__class__.__name__
        if shell == 'ZMQInteractiveShell':
            return True   # Jupyter notebook or qtconsole
        elif shell == 'TerminalInteractiveShell':
            return False  # Terminal running IPython
        else:
            return False  # Other type (?)
    except NameError:
        return False      # Probably standard Python interpreter

IN_NOTEBOOK = __isnotebook() # change this variable if function fails
GLOBAL_NDV = -9999.
MATERIAL_ID = 'itetclr'