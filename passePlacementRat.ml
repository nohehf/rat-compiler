(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
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
  | AstType.Retour (e, info) -> (
      match Tds.info_ast_to_info info with
      | Tds.InfoFun (_, t_retour, t_params) ->
          let taille_params =
            List.fold_right (fun t acc -> acc + getTaille t) t_params 0
          in
          let taille_retour = getTaille t_retour in
          (AstPlacement.Retour (e, taille_retour, taille_params), 0)
      | _ -> failwith "Unreachable")
  | AstType.Empty -> (AstPlacement.Empty, 0)

and analyse_placement_bloc depl reg li =
  match li with
  | [] -> ([], 0)
  | i :: q ->
      let i', t = analyse_placement_instruction depl reg i in
      let q', t' = analyse_placement_bloc (depl + t) reg q in
      (i' :: q', t + t')
(*
   let placer_param depl reg info_ast =
     let info = info_ast_to_info info_ast in
     match info with
     | Tds.InfoVar (_, t, _, _) ->
         let taille = getTaille t in
         modifier_adresse_variable (depl - taille) reg info_ast;
         (info_to_info_ast info, taille)
     | _ -> failwith "Unreachable"

   let analyse_params reg params =
     let rec aux depl reg _params =
       match _params with
       | [] -> []
       | i :: q ->
           let i', t = placer_param depl reg i in
           (* on se deplace vers le bas (pour mapper sur la pile avant l'appel) *)
           let q' = aux (depl - t) reg q in
           i' :: q'
     in
     aux 0 reg params

   let analyse_placement_fonction func_reg
       (AstType.Fonction (info_ast, params, bloc)) =
     (* On doit placer dans l'odre inverse pour consommer la pile (premier param en dernier sur la pile) *)
     let rev_params = List.rev params in
     let params' = analyse_params func_reg rev_params in
     (* On deplace de 3, pour l'activation (retour, lien dynamique, lien statique) *)
     let bloc' = analyse_placement_bloc 3 func_reg bloc in
     AstPlacement.Fonction (info_ast, params', bloc') *)

(* retourne un couple (info depl) *)
let rec analyse_placement_fonction_param li depl =
  match li with
  | [] -> []
  | info :: q -> (
      match info_ast_to_info info with
      | InfoVar (_, t, _, _) ->
          let taille = depl - getTaille t in
          modifier_adresse_variable taille "LB" info;
          info :: analyse_placement_fonction_param q taille
      | _ -> failwith "Erreur interne")

(* informations associées à l'identificateur (dont son nom), liste des paramètres, corps *)
(* type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc *)
let analyse_placement_fonction (AstType.Fonction (info, li, b)) =
  AstPlacement.Fonction
    ( info,
      analyse_placement_fonction_param (List.rev li) 0,
      analyse_placement_bloc 3 "LB" b )

let analyser (AstType.Programme (fonctions, prog)) =
  let depl = 0 in
  let main_reg = "SB" in
  let func_reg = "LB" in
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc depl main_reg prog in
  AstPlacement.Programme (nf, nb)
