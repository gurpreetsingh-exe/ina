open Printf
open Front
open Session
open Utils
open Resolve
open Middle.Ctx
open Sema
open Lowering

let () =
  let start = Sys.time () in
  let sess = Sess.create (Args.parse_args ()) in
  let tcx = new tcx sess in
  let time, res =
    Timer.time (fun () ->
        Parser.parse_mod_from_file sess.parse_sess sess.options.input)
  in
  match res with
  | Ok modd ->
      sess.options.output <-
        (if Filename.basename modd.mod_path = "lib.ina"
         then Filename.dirname modd.mod_path
         else Path.join [Filename.dirname modd.mod_path; modd.mod_name]);
      sess.timings.parse <- time;
      if sess.options.print_ast
      then (
        Front.Ast_printer.render_module modd "";
        print_endline !Front.Ast_printer.out;
        exit 0);
      (match sess.options.command with
       | Build ->
           let time, resolver =
             Timer.time (fun () ->
                 let resolver = new Resolver.resolver tcx modd in
                 let visitor = new Module_graph.visitor resolver modd None in
                 visitor#visit_mod;
                 resolver#init visitor#mdl;
                 resolver#resolve;
                 (* let printer = new Printer.printer in *)
                 (* Resolver.Module.print_modul "" printer visitor#mdl; *)
                 (* printer#print; *)
                 resolver)
           in
           sess.timings.resolve <- time;
           (new Late.type_lowering resolver modd)#lower;
           let time, _ =
             Timer.time (fun () ->
                 let infcx = Infer.infer_ctx_create tcx in
                 let cx = Tychk.create infcx in
                 Tychk.tychk cx modd)
           in
           sess.timings.sema <- time;
           let time, modulee =
             Timer.time (fun () ->
                 let lcx = new Context.lcx tcx modd in
                 Item.lower lcx)
           in
           sess.timings.lowering <- time;
           if sess.options.print_ir
           then (
             Ir.Module.render tcx modulee;
             exit 0)
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
