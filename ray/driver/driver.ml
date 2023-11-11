open Printf
open Front
open Session
open Utils
open Resolve

let () =
  let start = Sys.time () in
  let sess = Sess.create (Args.parse_args ()) in
  let time, res =
    Timer.time (fun () ->
        Parser.parse_mod_from_file sess.parse_sess sess.options.input)
  in
  match res with
  | Ok modd ->
      sess.options.output <-
        (if Filename.basename modd.mod_path = "lib.ray"
         then Filename.dirname modd.mod_path
         else Path.join [Filename.dirname modd.mod_path; modd.mod_name]);
      sess.timings.parse <- time;
      (match sess.options.command with
       | Build ->
           let resolver = new Resolver.resolver sess modd in
           let visitor = new Module_graph.visitor resolver modd None in
           visitor#visit_mod;
           resolver#init visitor#modul;
           let printer = new Printer.printer in
           Resolver.print_modul "" printer visitor#modul;
           printer#print;
           resolver#resolve
       | Fmt ->
           let open Ast_printer in
           render_module modd "";
           Format.print_string !out
       | _ -> ());
      let time = (Sys.time () -. start) *. 1000. in
      if sess.options.display_time
      then printf "  total: %f ms\n%s" time (Sess.display sess.timings);
      exit 0
  | Error e -> sess.parse_sess.span_diagnostic#emit_diagnostic e
;;

(*
open Printf
open Front
open Sema
open Codegen
open Resolve
open Session
open Utils
open Ty

let () =
  let start = Sys.time () in
  let sess = Sess.create (Args.parse_args ()) in
  let tcx = tcx_create sess in
  let time, modd =
    Timer.time (fun () -> Parser.parse_mod_from_file tcx sess.options.input)
  in
  tcx.sess.options.output <-
    (if Filename.basename modd.mod_path = "lib.ray"
     then Filename.dirname modd.mod_path
     else Path.join [Filename.dirname modd.mod_path; modd.mod_name]);
  sess.timings.parse <- time;
  (match sess.options.command with
   | Build ->
       let time, _ =
         Timer.time (fun () ->
             let resolver = Resolver.create tcx modd in
             let modul = Resolver.resolve resolver true in
             resolver.disambiguator.stack <- [0];
             Paths.resolve_paths resolver modul modd;
             if tcx.sess.options.output_type = Unit
             then (
               let enc = tcx.sess.enc in
               Resolver.encode_module enc modul;
               encode_metadata tcx);
             modul.resolutions)
       in
       sess.timings.resolve <- time;
       if sess.handler.err_count <> 0 then exit 1;
       let time, _ =
         Timer.time (fun () ->
             let infer_ctx = Infer.infer_ctx_create tcx in
             ignore (Infer.infer_begin infer_ctx modd);
             let ty_ctx = Tychk.ty_ctx_create infer_ctx in
             ignore (Tychk.tychk ty_ctx modd))
       in
       sess.timings.sema <- time;
       if sess.handler.err_count <> 0 then exit 1;
       let time, modulee =
         Timer.time (fun () ->
             let lowering_ctx = Lowering.Context.create tcx modd in
             Lowering.Item.lower_ast lowering_ctx)
       in
       sess.timings.lowering <- time;
       if sess.options.print_ir
       then (
         Ir.Module.render modulee;
         exit 0);
       if sess.options.dot_cfg
       then (
         Ir.Module.dot_graph modulee;
         exit 0);
       let cx = Llvm_gen.codegen_ctx tcx in
       let time, _ = Timer.time (fun () -> Llvm_gen.gen_module cx modulee) in
       sess.timings.llvm <- time;
       let time, _ = Timer.time (fun () -> Llvm_gen.emit cx) in
       sess.timings.gen_and_link <- time
   | Fmt -> printf "%s" (Fmt.render_mod modd)
   | _ -> ());
  let time = (Sys.time () -. start) *. 1000. in
  if sess.options.display_time
  then printf "  total: %f ms\n%s" time (Sess.display sess.timings);
  exit 0
;;
*)
