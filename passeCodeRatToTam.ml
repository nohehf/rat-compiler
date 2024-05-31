(* Generation du code tam *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

let sep = ""
let concat_lines l = String.concat sep l

let rec analyser_code_expression e =
  match e with
  | AstType.AppelFonction (info, le) -> failwith "TODO"
  | AstType.Ident info -> (
      match info_ast_to_info info with
      | InfoVar (_, typ, depl, reg) ->
          let taille = getTaille typ in
          Tam.load taille depl reg
      | _ -> failwith "Unreachable")
  | AstType.Booleen b -> if b then Tam.loadl_int 1 else Tam.loadl_int 0
  | AstType.Entier i -> Tam.loadl_int i
  | AstType.Unaire (op, e) -> (
      analyser_code_expression e
      ^
      match op with
      (*
         Les rationnels sont stockes dans la pile de la maniere suivante:
         | Numerateur |
         | Denominateur |
      *)
      | AstType.Numerateur -> Tam.pop 0 1
      | AstType.Denominateur -> Tam.pop 1 1)
  | AstType.Binaire (op, e1, e2) -> (
      analyser_code_expression e1
      ^ analyser_code_expression e2
      ^
      match op with
      (* Rat Deja sur la pile *)
      | AstType.Fraction -> ""
      | AstType.PlusInt -> Tam.subr "IAdd"
      | AstType.PlusRat -> Tam.call "SB" "RAdd"
      | AstType.MultInt -> Tam.subr "IMul"
      | AstType.MultRat -> Tam.call "SB" "RMul"
      | AstType.EquInt -> Tam.subr "IEq"
      (* On utilse eq int pour les booleens *)
      | AstType.EquBool -> Tam.subr "IEq"
      | AstType.Inf -> Tam.subr "ILss")

let rec analyser_code_instruction i =
  match i with
  | AstPlacement.Declaration (info, e) -> (
      match info_ast_to_info info with
      (* Constantes sont deja enlevees *)
      | InfoConst (_, _) -> failwith "Unreachable"
      | InfoVar (_, typ, depl, reg) ->
          let taille = getTaille typ in
          concat_lines
            [
              Tam.push taille;
              analyser_code_expression e;
              Tam.store taille depl reg;
            ]
      | InfoFun (_, _, _) -> failwith "TODO")
  | AstPlacement.Affectation (info, e) -> (
      match info_ast_to_info info with
      | InfoVar (_, typ, depl, reg) ->
          let taille = getTaille typ in
          concat_lines [ analyser_code_expression e; Tam.store taille depl reg ]
      | _ -> failwith "todo")
  | AstPlacement.AffichageInt e ->
      concat_lines [ analyser_code_expression e; Tam.subr "IOut" ]
  | AstPlacement.AffichageRat e ->
      (* Defini dans une bibliotheque *)
      concat_lines [ analyser_code_expression e; Tam.call "SB" "ROut" ]
  | AstPlacement.AffichageBool e ->
      concat_lines [ analyser_code_expression e; Tam.subr "BOut" ]
  | AstPlacement.Conditionnelle (e, bloc_if, bloc_else) ->
      let label_else = getEtiquette () in
      let label_fin = getEtiquette () in
      concat_lines
        [
          analyser_code_expression e;
          (* Si e est false *)
          Tam.jumpif 0 label_else;
          analyser_code_bloc bloc_if;
          Tam.jump label_fin;
          Tam.label label_else;
          analyser_code_bloc bloc_else;
          Tam.label label_fin;
        ]
  | AstPlacement.TantQue (e, bloc) ->
      let label_debut = getEtiquette () in
      let label_fin = getEtiquette () in
      concat_lines
        [
          Tam.label label_debut;
          analyser_code_expression e;
          Tam.jumpif 0 label_fin;
          analyser_code_bloc bloc;
          Tam.jump label_debut;
          Tam.label label_fin;
        ]
  | AstPlacement.Retour (e, tret, tparams) -> failwith "TODO"
  | AstPlacement.Empty -> ""

(* todo: utiliser t *)
and analyser_code_bloc (li, t) =
  concat_lines (List.map analyser_code_instruction li) ^ Tam.pop 0 t

let analyser_code_fonction (AstPlacement.Fonction (info, li, b)) =
  failwith "todo"

let analyser (AstPlacement.Programme (fonctions, prog)) =
  getEntete () ^ sep ^ Tam.label "main" ^ sep ^ analyser_code_bloc prog
  ^ Tam.halt
