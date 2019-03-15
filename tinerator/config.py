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
formatter = logging.Formatter('[%(asctime)s] %(levelname)s - %(message)s')

# add formatter to ch
ch.setFormatter(formatter)

# add ch to log
log.addHandler(ch)


'''
CONSTANTS - NOT FOR END-USER MODIFICATIONS
'''
