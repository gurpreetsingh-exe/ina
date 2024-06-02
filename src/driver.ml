open Ina
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
  let passes =
    Array.map (fun (module P : Transform.Pass) -> P.name) Transform.passes
  in
  Args.passes := passes;
  let sess = Sess.create (Args.parse_args ()) in
  if sess.options.list_passes
  then (
    printf
      "IR Passes:\n  %s\n\n"
      (passes
       |> Array.to_list
       |> List.sort String.compare
       |> String.concat "\n  ");
    exit 0);
  Array.iter
    (fun (module P : Transform.Pass) ->
      match sess.options.skip_passes with
      | Some [||] -> P.is_enabled := false
      | Some arr -> P.is_enabled := not @@ Array.mem P.name arr
      | None -> ())
    Transform.passes;
  let tcx = new tcx sess in
  let time, res =
    Timer.time (fun () ->
        Parser.parse_mod_from_file sess.parse_sess sess.options.input)
  in
  let resolve_and_sema mdl =
    let time, (resolver, _) =
      Timer.time (fun () ->
          let resolver = new Resolver.resolver tcx mdl in
          let visitor =
            new Module_graph.visitor resolver mdl None (Owned None)
          in
          visitor#visit_mod_root;
          resolver#extmods#insert' mdl.mod_name visitor#mdl;
          if sess.options.print_module_graph
          then (
            let printer = new Printer.printer in
            Seq.iter
              (fun mdl -> Resolver.Module.print_modul "" printer mdl)
              resolver#extmods#values;
            printer#print;
            exit 0);
          resolver#init visitor#mdl;
          resolver#resolve;
          if tcx#has_errors then exit 1;
          resolver, visitor#mdl)
    in
    sess.timings.resolve <- time;
    (new Late.type_lowering resolver mdl)#lower;
    if sess.options.output_type = ExtMod
    then (
      Metadata.Encoder.encode_hashmap
        tcx#sess.enc
        resolver#extmods
        (fun enc s -> enc#emit_str s)
        (fun enc mdl' ->
          let mdl =
            match mdl'.mkind with
            | Def (_, _, name) when mdl.mod_name <> name ->
                let mdl =
                  (Array.of_seq mdl'.resolutions#values).(0)
                  |> function Module m -> m | _ -> assert false
                in
                mdl
            | _ -> mdl'
          in
          Resolver.Module.encode enc mdl);
      tcx#encode_metadata);
    let time, _ =
      Timer.time (fun () ->
          let infcx = Infer.infer_ctx_create tcx in
          let cx = Tychk.create infcx in
          Tychk.tychk cx mdl;
          if tcx#has_errors then exit 1;
          Linearity.analyze_module tcx mdl)
    in
    sess.timings.sema <- time;
    if tcx#has_errors then exit 1
  in
  match res with
  | Ok mdl ->
      tcx#sess.enc#set_mod_name mdl.mod_name;
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
       | Build | Test ->
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
           if sess.options.output_type = ExtMod
           then Ir.Module.encode tcx#sess.enc mdl;
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
