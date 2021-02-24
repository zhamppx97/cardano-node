{ stateDir
, graphviz
}:

''
  args=( --output-file "${stateDir}/topology.json"
         --dot-output  "${stateDir}/topology.dot"
         --loc          ${locations}
         --size         ${size}
       )

  topology "''${args[@]}"

  ${graphviz}/bin/neato -s120 -Tpdf \
     "${stateDir}/topology.dot" > "${stateDir}/topology.pdf"
''
