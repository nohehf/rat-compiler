(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

let rec analyse_type_expression e =
  match e with
  | AstTds.AppelFonction (ident, expl) -> failwith "TODO"
  | AstTds.Ident ident -> failwith "TODO"
  | AstTds.Booleen b -> AstType.Booleen b
  | AstTds.Entier i -> AstType.Entier i
  | AstTds.Unaire (u, exp) -> failwith "TODO"
  | AstTds.Binaire (op, exp1, exp2) -> failwith "TODO"

let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, n, e) -> failwith "TODO"
  | AstTds.Affectation (n, e) -> failwith "TODO"
  | AstTds.Affichage e -> failwith "TODO"
  | AstTds.Conditionnelle (c, t, e) -> failwith "TODO"
  | AstTds.TantQue (c, b) -> failwith "TODO"
  | AstTds.Retour (e, i) -> failwith "TODO"
  | AstTds.Empty -> failwith "TODO"

and analyse_type_bloc li =
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
  let nli = List.map analyse_type_instruction li in
  (* afficher_locale tdsbloc ; *)
  (* décommenter pour afficher la table locale *)
  nli

let analyse_type_fonction (AstTds.Fonction (return_type, info, params, bloc)) =
  failwith "TODO"

let analyser (AstTds.Programme (fonctions, prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf, nb)
