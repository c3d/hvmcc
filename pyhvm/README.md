# PyHVM

PyHVM is a tool to utilize the HVM runtime inside python.

## Instalation

As it is not published yet, it needs to be built.

It uses the maturin build tool to build a python wheel, which can then be installed through pip.
```sh
python3 -m maturin build
python3 -m pip install ../target/wheels/pyhvm-0.1.0-cp310-cp310-manylinux_2_34_x86_64.whl  --force-reinstall
```

## Usage

It provides access to the `@hvm` decorator, which will execute all functions decorated with it from within the HVM's runtime.
