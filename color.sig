signature COLOR =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val color : {interference : Liveness.igraph,
               initial : allocation,
               spillCost : Temp.temp -> real,
               registers : Frame.register list}
              -> allocation * Temp.temp list
end
