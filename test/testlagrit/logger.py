import logging

# Configure the logger that will write test info to a file
logging.basicConfig(filename='lagrit-tests.log', level=logging.DEBUG, format='%(levelname)s:%(message)s')
log = logging.getLogger(__name__)
