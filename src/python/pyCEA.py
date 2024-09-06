import os
import sys
from objCEA import CEA
masterpath = os.environ.get("NewCEADIR")
if masterpath is None:
    print("NewCEA environment variable is not set.")
    sys.exit(1)
libpath = masterpath + '/lib/thermo-transport/'

filename = input('Enter the input file name:')
cea = CEA()  # Initialize the CEA object
cea.indx = 1
cea.solve(filename)
