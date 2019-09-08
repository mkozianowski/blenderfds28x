# 3rd party executables

These executables are precompiled tools for Linux, OSX, and Windows

## Jingwei Huang: quadriflow, manifold, simplify

Thanks to Jingwei Huang (http://stanford.edu/~jingweih) for this code!
Geometric Computing Lab, Dept. of Computer Science, Stanford University, USA

http://stanford.edu/~jingweih/papers/quadriflow/quadriflow.pdf
https://github.com/hjwdzh/QuadriFlow
https://github.com/hjwdzh/Manifold

`@article {10.1111:cgf.13498,
    journal = {Computer Graphics Forum},
    title = {{QuadriFlow: A Scalable and Robust Method for Quadrangulation}},
    author = {Huang, Jingwei and Zhou, Yichao and Niessner, Matthias and Shewchuk, Jonathan Richard and Guibas, Leonidas J.},
    year = {2018},
    publisher = {The Eurographics Association and John Wiley & Sons Ltd.},
    ISSN = {1467-8659},
    DOI = {10.1111/cgf.13498}
}`

`@article{huang2018robust,
  title={Robust Watertight Manifold Surface Generation Method for ShapeNet Models},
  author={Huang, Jingwei and Su, Hao and Guibas, Leonidas},
  journal={arXiv preprint arXiv:1802.01698},
  year={2018}
}`

### How to use Quadriflow

We take a manifold triangle mesh `input.obj` and generate a manifold quad mesh `output.obj`.
The face number increases linearly with the resolution controled by the user.

`./quadriflow -mcf -sharp -i input.obj -o output.obj -f [resolution]`

### How to use Manifold

We take a triangle mesh `input.obj` and generate a manifold `output.obj`.
The resolution is the number of leaf nodes of octree.
The face number increases linearly with the resolution (default 20000).

`./manifold input.obj output.obj [resolution]`

### How to use Simplify

A mesh simplification can be used, to reduce the number of the faces.

`./simplify -i input.obj -o output.obj [-m] [-f face_num] [-c max_cost] [-r max_ratio]`

Where:

  -m            Turn on manifold check, we don't output model if check fails
  -f face_num   Add termination condition when current_face_num <= face_num
  -c max_cost   Add termination condition when quadric error >= max_cost
  -r max_ratio  Add termination condition when current_face_num / origin_face_num <= max_ratio

Example:

`./simplify -i input.obj -o output.obj -m -c 1e-2 -f 10000 -r 0.2`


### How to compile this code on Ubuntu Linux

```
sudo apt install build-essential libeigen3-dev libglpk-dev libboost-all-dev
cd /tmp
git clone git://github.com/hjwdzh/quadriflow
cd quadriflow
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=release
make -j

cp quadriflow ~

cd /tmp
git clone --recursive -j8 git://github.com/hjwdzh/Manifold
cd Manifold
mkdir build
cd build
cmake ..
make

cp manifold ~
cp simplify ~`
```