from distutils.core import setup
import os

setup(name='pylagrit',
	version='1.0.0',
	description='Python interface for LaGriT',
	author='Dylan R. Harp',
	author_email='dharp@lanl.gov',
	url='lagrit.lanl.gov',
	license='LGPL',
	packages=[
		'pylagrit',
		'pylagrit.pexpect']
	)
