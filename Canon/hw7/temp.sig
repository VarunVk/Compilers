signature TEMP = 
sig
  eqtype temp
  val newtemp : unit -> temp
  structure Table : TABLE sharing type Table.key = temp
  val makestring: temp -> string
  eqtype label
  val newlabel : unit -> label
  val namedlabel : string -> label
  val labname: label -> string  (* a hack for printing *)
  val tempname: temp -> string  (* a hack for printing *)
end
