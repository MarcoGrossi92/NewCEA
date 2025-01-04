from setuptools import setup, find_packages
from setuptools.command.build_ext import build_ext
from setuptools.command.build_py import build_py
import subprocess
import os


class CustomBuildExt(build_ext):
    def run(self):
        # Step 1: Run CMake to build the Fortran library
        build_dir = os.path.join(os.getcwd(), "build")
        os.makedirs(build_dir, exist_ok=True)
        subprocess.check_call(["cmake", "..", "-DCMAKE_BUILD_TYPE=RELEASE"], cwd=build_dir)
        subprocess.check_call(["make"], cwd=build_dir)

        # Step 2: Use f2py to generate the .so file
        src_dir = os.path.join(os.getcwd(), "src", "fortran", "lib")
        target_dir = os.path.join(os.getcwd(), "src", "python", "NewCEA")
        os.makedirs(target_dir, exist_ok=True)
        subprocess.check_call([
            "f2py", "-c", "-m", "FCEA2",
            os.path.join(src_dir, "CEAinc.f90"),
            os.path.join(src_dir, "cea2.f")
        ], cwd=target_dir)
        super().run()


class CustomBuildPy(build_py):
    def run(self):
        # Ensure the custom build_ext step is run first
        self.run_command("build_ext")
        super().run()


setup(
    name="NewCEA",
    version="1.0.0",
    packages=find_packages(where="src/python"),
    package_dir={'': 'src/python'},
    package_data={
        "NewCEA": ["*.so"],  # Include all .so files in the NewCEA package
    },
    cmdclass={
        "build_ext": CustomBuildExt,
        "build_py": CustomBuildPy,
    },
    install_requires=["meson","ninja"],  # Other dependencies can go here
    description="A modern CEA interface for Fortran and Python codes",
    author="Marco Grossi",
    author_email="marco.grossi@uniroma1.it",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
    ],
)
