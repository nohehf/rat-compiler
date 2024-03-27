(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

let syntax_unaire_to_type_unaire u =
  match u with
  | AstSyntax.Denominateur -> AstType.Denominateur
  | AstSyntax.Numerateur -> AstType.Numerateur

let syntax_binaire_to_type_binaire op t1 t2 =
  if not (est_compatible t1 t2) then raise (TypeBinaireInattendu (op, t1, t2))
  else
    match op with
    | AstSyntax.Fraction -> (AstType.Fraction, Rat)
    | AstSyntax.Plus -> (
        match t1 with
        | Int -> (PlusInt, Int)
        | Rat -> (PlusRat, Rat)
        | _ -> raise (TypeBinaireInattendu (op, t1, t2)))
    | AstSyntax.Mult -> (
        match t1 with
        | Int -> (MultInt, Int)
        | Rat -> (MultRat, Rat)
        | _ -> raise (TypeBinaireInattendu (op, t1, t2)))
    | AstSyntax.Equ -> (
        match t1 with
        | Int -> (EquInt, Bool)
        | Bool -> (EquBool, Bool)
        | _ -> raise (TypeBinaireInattendu (op, t1, t2)))
    | AstSyntax.Inf -> (
        match t1 with
        | Int -> (Inf, Bool)
        | _ -> raise (TypeBinaireInattendu (op, t1, t2)))

let rec analyse_type_expression e =
  match e with
  | AstTds.AppelFonction (info, expl) -> (
      let expl = List.map analyse_type_expression expl in
      let expression_types_list = List.map snd expl in
      match info_ast_to_info info with
      | InfoFun (_, return_type, parameter_types) ->
          if not (est_compatible_list expression_types_list parameter_types)
          then
            raise
              (TypesParametresInattendus (expression_types_list, parameter_types))
          else (AstType.AppelFonction (info, List.map fst expl), return_type)
      | _ -> failwith "Unreachable")
  | AstTds.Ident info -> (
      match info_ast_to_info info with
      | InfoConst (_, _) -> (AstType.Ident info, Int)
      | InfoVar (_, t, _, _) -> (AstType.Ident info, t)
      | InfoFun (_, t, _) -> (AstType.Ident info, t))
  | AstTds.Booleen b -> (AstType.Booleen b, Bool)
  | AstTds.Entier i -> (AstType.Entier i, Int)
  | AstTds.Unaire (u, exp) -> (
      let exp, exp_type = analyse_type_expression exp in
      match exp_type with
      | Rat ->
          let u = syntax_unaire_to_type_unaire u in
          (AstType.Unaire (u, exp), Int)
      | _ -> raise (TypeInattendu (exp_type, Rat)))
  | AstTds.Binaire (op, exp1, exp2) ->
      let expt1, t1 = analyse_type_expression exp1 in
      let expt2, t2 = analyse_type_expression exp2 in
      let opt, t = syntax_binaire_to_type_binaire op t1 t2 in
      (AstType.Binaire (opt, expt1, expt2), t)

let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, i, e) ->
      let expt, et = analyse_type_expression e in
      if not (est_compatible t et) then raise (TypeInattendu (et, t))
      else
        let _ = modifier_type_variable et i in
        AstType.Declaration (i, expt)
  | AstTds.Affectation (i, e) -> (
      let expt, et = analyse_type_expression e in
      match info_ast_to_info i with
      | InfoVar (_, t, _, _) ->
          if not (est_compatible t et) then raise (TypeInattendu (et, t))
          else AstType.Declaration (i, expt)
      | _ -> failwith "Unreachable")
  | AstTds.Affichage e -> (
      let expt, et = analyse_type_expression e in
      match et with
      | Bool -> AstType.AffichageInt expt
      | Int -> AstType.AffichageInt expt
      | Rat -> AstType.AffichageInt expt
      | Undefined -> failwith "Unreachable")
  | AstTds.Conditionnelle (c, t, e) ->
      let c, ct = analyse_type_expression c in
      if ct <> Bool then raise (TypeInattendu (ct, Bool))
      else AstType.Conditionnelle (c, analyse_type_bloc t, analyse_type_bloc e)
  | AstTds.TantQue (c, b) ->
      let c, ct = analyse_type_expression c in
      if ct <> Bool then raise (TypeInattendu (ct, Bool))
      else AstType.TantQue (c, analyse_type_bloc b)
  | AstTds.Retour (e, i) -> (
      let e, et = analyse_type_expression e in
      match info_ast_to_info i with
      | InfoFun (_, t, _) ->
          if et <> t then raise (TypeInattendu (et, t))
          else AstType.Retour (e, i)
      | _ -> failwith "Unreachable")
  | AstTds.Empty -> AstType.Empty

and analyse_type_bloc li = List.map analyse_type_instruction li

let analyse_type_fonction (AstTds.Fonction (return_type, info, params, bloc)) =
  let bloc = analyse_type_bloc bloc in
  match info_ast_to_info info with
  (* check return type, then param types *)
  | InfoFun (_, expected_return_type, expected_params_types) ->
      if not (est_compatible expected_return_type return_type) then
        raise (TypeInattendu (expected_return_type, return_type))
      else
        let params_info = List.map snd params in
        let _ =
          List.iter2
            (fun i t -> modifier_type_variable t i)
            params_info expected_params_types
        in
        AstType.Fonction (info, params_info, bloc)
  | _ -> failwith "Unreachable"

let analyser (AstTds.Programme (fonctions, prog)) =
  let nf = List.map analyse_type_fonction fonctions in
  let nb = analyse_type_bloc prog in
  AstType.Programme (nf, nb)
