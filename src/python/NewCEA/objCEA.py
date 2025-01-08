import os
import numpy as np
import FCEA2
import sys

masterpath = os.environ.get("NewCEADIR")
if masterpath is None:
    print("NewCEA environment variable is not set.")
    sys.exit(1)

libpath = masterpath + '/lib/thermo-transport/'

class Propellant:
    def __init__(self, name='', prop_type='', T=0.0, wt=0.0, more=''):
        self.T = T      # Temperature in K
        self.wt = wt    # Weight fraction
        self.name = name
        self.type = prop_type  # 'F' for fuel, 'O' for oxidizer
        self.more = more

class Species:
    def __init__(self, n=0):
        self.n = n  # Number of species
        self.massf = np.zeros(n)  # Mass fractions
        self.molef = np.zeros(n)  # Mole fractions
        self.name = ['' for _ in range(n)]  # Names of species

class CEAOutput:
    def __init__(self):
        self.temperature = 0.0
        self.pressure = 0.0
        self.w = 0.0
        self.cp = 0.0
        self.cpeq = 0.0
        self.gamma = 0.0
        self.a = 0.0
        self.Mach = 0.0
        self.h0 = 0.0
        self.h = 0.0
        self.ivac = 0.0
        self.cstar = 0.0
        self.cf = 0.0
        self.species = Species()

class CEA:
    def __init__(self):
        self.input_mode = 'data'
        self.nprop = 0
        self.indx = 0
        self.prop = []
        self.eps_type = 'sup'
        self.pressure = 0.0
        self.OF = 0.0
        self.epsilon = 0.0
        self.FE = CEAOutput()
        self.SE = CEAOutput()
        self.only = False
        self.OG = False

    def solve(self, filename):
        """
        Call the external CEA solver to compute the thermodynamic and performance data.
        """
        # Simulate calling an external CEA2 library (e.g. via ctypes, subprocess, or an external Python wrapper)
        # This example assumes the function returns data arrays
        spec_name, spec_frac, perfo, state = FCEA2.cea2(libpath, filename, self.indx)
        good_spec_name = []
        for name in spec_name:
            try:
                good_spec_name.append(name.decode('utf-8').strip())
            except:
                pass


        # Performance data
        self.SE.cstar, self.SE.cf, self.SE.ivac = perfo[0]
        self.FE.cstar, self.FE.cf, self.FE.ivac = perfo[1]

        # Flow state data
        self.SE.pressure, self.SE.temperature, self.SE.h = state[0, :3]
        self.FE.pressure, self.FE.temperature, self.FE.h = state[1, :3]
        self.SE.w, self.SE.a, self.SE.Mach = state[0, 3:]
        self.FE.w, self.FE.a, self.FE.Mach = state[1, 3:]

        self.SE.h0 = self.SE.h + 0.5 * (self.SE.Mach * self.SE.a) ** 2
        self.FE.h0 = self.FE.h + 0.5 * (self.FE.Mach * self.FE.a) ** 2

        # Species composition
        self.SE.species.n = len(good_spec_name)
        self.SE.species.name = good_spec_name
        self.SE.species.massf = spec_frac

        # Recompute the species fractions
        if (self.OG): self.recompute_fractions()

    def recompute_fractions(self):
        """
        Adjusts the species composition to account for condensed phases (liquids or crystals).
        """
        dummy_name = self.SE.species.name[:]
        dummy_fraction = self.SE.species.massf if len(self.SE.species.massf) > 0 else self.SE.species.molef

        ncond = 0
        yCondP = 0.0

        for s in range(self.SE.species.n):
            # Convert bytes to string using .decode() before comparison
            if '(L)' in dummy_name[s] or '(cr)' in dummy_name[s]:
                ncond += 1
                yCondP += dummy_fraction[s]

        if ncond > 0 and self.OG:
            remaining_species = [
                (name, fraction / (1 - yCondP)) 
                for name, fraction in zip(dummy_name, dummy_fraction) 
                if '(L)' not in name and '(cr)' not in name
            ]

            self.SE.species.n = len(remaining_species)
            self.SE.species.name, new_fractions = zip(*remaining_species)

            if len(self.SE.species.massf) > 0:
                self.SE.species.massf = np.array(new_fractions)
            else:
                self.SE.species.molef = np.array(new_fractions)

    def write_cea_input(self, what, only_species=None):
        """
        Generate a CEA input file based on the current object properties.
        """
        with open('CEAfile.inp', 'w') as file:
            file.write('problem case=MOCA_was_here\n')
            file.write(f'rocket {what}\n')
            file.write(f'p,bar={self.pressure}\n')
            if 'sub' in self.eps_type:
                file.write(f'sub,at/ae={self.epsilon}\n')
            else:
                file.write(f'sup,ae/at={self.epsilon}\n')
            if self.OF > 1e-20:
                file.write(f'o/f={self.OF}\n')
            file.write('react\n')

            for prop in self.prop:
                if prop.type == 'F':
                    file.write(f'fuel={prop.name} wt={prop.wt} t,k={prop.T}\n')
                else:
                    file.write(f'oxid={prop.name} wt={prop.wt} t,k={prop.T}\n')
                if prop.more:
                    file.write(f'{prop.more}\n')

            if self.only and only_species:
                file.write('only\n')
                for species_name in only_species.name:
                    file.write(f'{species_name}\n')

            file.write('output\n')
            file.write('   siunits short massf transport\n')
            file.write('end\n')
