open System.Collections.Generic 

// it could be anything
let graph = new Dictionary<int,int seq>()
graph.Add (1,seq [2;3])
graph.Add (2,seq [1])
graph.Add (3,seq [1])

let remove_table = new Dictionary<int,bool>()
remove_table.Add (1,false)
remove_table.Add (2,false)
remove_table.Add (3,false)

let label = new Dictionary<int,int seq>()
label.Add (1,seq [1])
label.Add (2,seq [1])
label.Add (3,seq [1])
// 

type wgraphobj =
     { Graph : Dictionary<int,int seq>
       RemoveTable : Dictionary<int,bool>
       Label : Dictionary<int,int seq>
     }

let WG1 = {Graph = graph;
          RemoveTable = remove_table;
          Label = label;
          }

let WGmin = WG1