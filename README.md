```sh
$ dune exec src/resolve.exe -- path/ object.cmx
$ dune exec src/resolve.exe -- $(opam config var lib)/ $(opam config var lib)/irmin/irmin.cmxa
```
