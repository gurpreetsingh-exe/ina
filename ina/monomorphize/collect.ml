open Ir
open Middle
open Structures.Vec

let collect tcx mdl =
    let open Module in
    let items = new vec in
    mdl.items#iter (fun fn -> ());
    items

