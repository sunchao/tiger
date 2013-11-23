structure Flow =
struct
datatype node = Node of {def: Temp.temp list,
                         use: Temp.temp list,
                         ismove: bool,
                         succ: node list ref,
                         prev: node list ref,
                         liveOut: Temp.temp list ref}
type flowgraph = node list
end
