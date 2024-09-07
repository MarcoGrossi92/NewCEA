from NewCEA import CEA

filename = input('Enter the input file name:')
cea = CEA()  # Initialize the CEA object
cea.indx = 1
cea.solve(filename)
