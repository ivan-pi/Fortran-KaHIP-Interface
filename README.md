# Fortran-KaHIP-Interface

A Fortran interface to the [KaHIP](http://algo2.iti.kit.edu/documents/kahip/index.html) graph partitioning framework.

* [Getting started](#getting-started)

## Getting started

Install dependencies:
```
sudo apt-get install scons
sudo apt-get install openmpi-bin openmpi-doc libopenmpi-dev
```

Download and build KaHIP:
```
git clone https://github.com/schulzchristian/KaHIP.git
cd KaHIP
./compile_withcmake.sh
```

Test it works:
```
./deploy/kaffpa examples/delaunay_n15.graph --k 2 --preconfiguration=strong
```

For the Fortran interface and test:
```
gfortran -o interface_test kahip_interface.f90 interface_test.f90 ./../../deploy/libkahip.a -lstdc++
```
Not linking the C++ standard library will cause a huge list of undefined references!

Running the test yields the output
```
 partitioning graph from the manual
 edge_cut            2
```

## KaHIP API

```Fortran
call kaffpa(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,edgecut,part)

call kaffpa_balance_NE(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,edgecut,part)

call node_separator(n,vwgt,xadj,adjcwgt,adjncy,nparts,imbalance,suppress_output,&
            seed,mode,num_separator_vertices,separator)
```