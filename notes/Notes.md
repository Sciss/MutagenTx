Shrink db were possible:

- {No} remove `S#ID` from `Vertex.UGen`
- {OK} replace name storage in `Vertex.UGen` by short index
- {OK} replace `Double` by `Float` in fitness
-      replace inlet name in `Edge` by short index
-      apparently, evaluation has unbounded concurrent processes (scsynth); should limit to 4