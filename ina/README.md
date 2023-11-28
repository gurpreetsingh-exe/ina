# generating llvm bindings for ocaml

[more info here](https://github.com/llvm/llvm-project/tree/main/llvm/bindings/ocaml)

find [ocaml install path] using
```
$ ocamlc -which
path/to/lib/ocaml
```
remove the "ocaml" dir from path so the path becomes "path/to/lib"

run cmake
```
S cmake -DLLVM_OCAML_OUT_OF_TREE=TRUE \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DLLVM_OCAML_INSTALL_PATH=[ocaml install path] ../llvm
```

generate and install the bindings
```
$ make ocaml_all
$ cmake -P bindings/ocaml/cmake_install.cmake
```
