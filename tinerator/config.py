'''
CONFIGURATION FILE FOR TINERATOR
'''

import logging
import os

NOTHING     = 0
DEBUG       = 10
INFO        = 20
FULL        = 30
DEBUG_MODE  = False
KEEP_LAGRIT_LOGS = False

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


def activate_virtual_framebuffer():
    '''
    Activates a virtual (headless) framebuffer for rendering 3D
    scenes via VTK.

    Most critically, this function is useful when this code is being run
    in a Dockerized notebook, or over a server without X forwarding.

    * Requires the following packages:
      * `sudo apt-get install libgl1-mesa-dev xvfb`
    '''

    import subprocess
    import vtki

    vtki.OFFSCREEN = True
    os.environ['DISPLAY']=':99.0'

    commands = ['Xvfb :99 -screen 0 1024x768x24 > /dev/null 2>&1 &',
                'sleep 3',
                'exec "$@"']

    for command in commands:
        subprocess.call(command,shell=True)

    log.debug('Activated framebuffer')


# This is the home directory of the TINerator
# Docker - this could be made more elegant
if os.path.isdir('/home/jovyan'):
    activate_virtual_framebuffer()

IN_NOTEBOOK = __isnotebook()
GLOBAL_NDV = -9999.
MATERIAL_ID = 'itetclr'
