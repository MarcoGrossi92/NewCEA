from setuptools import setup, find_packages

setup(
    name="NewCEA",
    version="1.0",
    install_requires=[
        "meson",
        # other dependencies
    ],
    packages=find_packages(where="src/python"),
    package_dir={'': 'src/python'},
    package_data={
        "NewCEA": ["*.so"],  # Include all .so files in the NewCEA package
    },
    include_package_data=True,
    description="A modern CEA interface for Fortran and Python codes",
    author="Marco Grossi",
    author_email="marco.grossi@uniroma1.it",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
    ],
)
