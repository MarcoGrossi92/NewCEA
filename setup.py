from setuptools import setup, find_packages
from setuptools.command.build_ext import build_ext
from setuptools.command.build_py import build_py
import subprocess
import os
import glob


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

        # Step 3: Rename the .so file
        pattern = os.path.join(target_dir, "FCEA2*.so")
        matching_files = glob.glob(pattern)
        if matching_files:
            for old_file_name in matching_files:
                new_file_name = os.path.join(target_dir, "FCEA2.so")
                os.rename(old_file_name, new_file_name)


class CustomBuildPy(build_py):
    def run(self):
        # Ensure the custom build_ext step is run first
        self.run_command("build_ext")
        super().run()


setup(
    packages=find_packages(where="src/python"),
    package_dir={'': 'src/python'},
    package_data={
        "NewCEA": ["FCEA2.so"],  # Include all .so files in the NewCEA package
    },
    cmdclass={
        "build_ext": CustomBuildExt,
        "build_py": CustomBuildPy,
    },
)