import NewCEA
import numpy as np

libpath = '/Users/marcogrossi/Codici/myCEA/lib/thermo-transport/'
section_ = 3

# Create arrays for output
perfo = np.zeros((2, 3), dtype=np.float64)
state = np.zeros((2, 6), dtype=np.float64)
spec_frac = np.zeros(600, dtype=np.float64)
spec_name = np.zeros(600, dtype='U20')

# Call the Fortran subroutine via f2py
spec_name, spec_frac, perfo, state = NewCEA.cea2(libpath, 'CEA', section_)

# Print the results
print("SE Performance:", perfo[0,:])
print("FE Performance:", perfo[1,:])
print("SE State:", state[0,:])
print("FE State:", state[1,:])
