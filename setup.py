from setuptools import setup, find_packages

setup(
    name="NewCEA",
    version="1.0",
    packages=find_packages(where="src/python"),
    package_dir={'': 'src/python'},
    package_data={
        "NewCEA": ["*.so"],  # Include all .so files in the NewCEA package
    },
    include_package_data=True,
    description="A package that includes a renamed shared object file and Python code",
    author="Your Name",
    author_email="your.email@example.com",
    url="https://your.package.url",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
    ],
)
