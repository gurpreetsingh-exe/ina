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
  let resolve_and_sema mdl =
    let time, resolver =
      Timer.time (fun () ->
          let resolver = new Resolver.resolver tcx mdl in
          let visitor =
            new Module_graph.visitor resolver mdl None (Owned None)
          in
          visitor#visit_mod;
          if sess.options.print_module_graph
          then (
            let printer = new Printer.printer in
            Resolver.Module.print_modul "" printer visitor#mdl;
            printer#print;
            exit 0);
          resolver#init visitor#mdl;
          resolver#resolve;
          resolver)
    in
    sess.timings.resolve <- time;
    (new Late.type_lowering resolver mdl)#lower;
    let time, _ =
      Timer.time (fun () ->
          let infcx = Infer.infer_ctx_create tcx in
          let cx = Tychk.create infcx in
          Tychk.tychk cx mdl)
    in
    sess.timings.sema <- time;
    if tcx#has_errors then exit 1
  in
  match res with
  | Ok mdl ->
      sess.options.output <-
        (if Filename.basename mdl.mod_path = "lib.ina"
         then Filename.dirname mdl.mod_path
         else Path.join [Filename.dirname mdl.mod_path; mdl.mod_name]);
      sess.timings.parse <- time;
      if sess.options.print_ast
      then (
        Front.Ast_printer.render_module mdl "";
        print_endline !Front.Ast_printer.out;
        exit 0);
      (match sess.options.command with
       | Build ->
           resolve_and_sema mdl;
           let time, mdl =
             Timer.time (fun () ->
                 let lcx = new Context.lcx tcx in
                 Item.lower lcx mdl;
                 lcx#mdl)
           in
           sess.timings.lowering <- time;
           mdl.items#iter (fun fn ->
               Transform.run_passes tcx fn.basic_blocks);
           if sess.options.print_ir
           then (
             Ir.Module.render tcx mdl;
             exit 0);
           if not sess.options.skip_codegen then Codegen.Ctx.codegen tcx mdl
       | Check -> resolve_and_sema mdl
       | Fmt ->
           let open Ast_printer in
           render_module mdl "";
           Format.print_string !out
       | _ -> ());
      let time = (Sys.time () -. start) *. 1000. in
      if sess.options.display_time
      then printf "  total: %f ms\n%s" time (Sess.display sess.timings);
      exit 0
  | Error e -> sess.parse_sess.span_diagnostic#emit_diagnostic e
;;
