#!/bin/sh
python3 -m maturin build
python3 -m pip install ../target/wheels/pyhvm-0.1.0-cp310-cp310-manylinux_2_34_x86_64.whl  --force-reinstall
