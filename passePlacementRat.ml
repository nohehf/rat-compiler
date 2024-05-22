(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let rec analyse_placement_instruction depl reg i =
  match i with
  | AstType.Declaration (info, e) -> (
      match Tds.info_ast_to_info info with
      | Tds.InfoVar (_, t, _, _) ->
          modifier_adresse_variable depl reg info;
          (AstPlacement.Declaration (info, e), getTaille t)
      | _ -> failwith "Unreachable")
  | AstType.Affectation (i, e) -> (AstPlacement.Affectation (i, e), 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.Conditionnelle (c, t, e) ->
      let t' = analyse_placement_bloc depl reg t in
      let e' = analyse_placement_bloc depl reg e in
      (AstPlacement.Conditionnelle (c, t', e'), 0)
  | AstType.TantQue (c, b) ->
      let b' = analyse_placement_bloc depl reg b in
      (AstPlacement.TantQue (c, b'), 0)
  | AstType.Retour (e, i) -> failwith "TODO"
  | AstType.Empty -> (AstPlacement.Empty, 0)

and analyse_placement_bloc depl reg li =
  match li with
  | [] -> ([], 0)
  | i :: q ->
      let i', t = analyse_placement_instruction depl reg i in
      let q', t' = analyse_placement_bloc (depl + t) reg q in
      (i' :: q', t + t')

let analyse_placement_fonction (AstType.Fonction (info, params, bloc)) =
  failwith "TODO"

let analyser (AstType.Programme (fonctions, prog)) =
  let depl = 0 in
  let reg = "SB" in
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc depl reg prog in
  AstPlacement.Programme (nf, nb)
