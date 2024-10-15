import numpy as np
import code

N = 10  # Number of Fibonacci numbers you want
A = np.zeros(N, dtype=np.float64)  # Create a double precision array of size N

print(code.NCOL)

code.fib(A,N)  # Call the subroutine from the module 'code'

print(A)  # Print the result

