[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Token
open User_syntax
open Earley_core

let lexing_position i c =
  Asai.Span.of_lex_position @@
  Lexing.{
    pos_fname = Input.filename i;
    pos_lnum = Input.line_num i;
    pos_bol = Input.line_offset i;
    pos_cnum = Input.line_offset i + c;
    (* This should be BYTE-oriented! *)
  }

let locate i1 c1 i2 c2 =
  Asai.Span.make (lexing_position i1 c1) (lexing_position i2 c2)

let located p =
  p |> Earley.apply_position @@ fun i1 c1 i2 c2 x ->
  Asai.Span.{
    value = x;
    loc = Some (locate i1 c1 i2 c2);
  }


let parser program = cmd*

and parser cmd_ =
  | "def" name:name colon tp:term assign tm:term ->
    Def {name; tp = Some tp; tm}
  | "def" name:name assign tm:term ->
    Def {name; tp = None; tm}
and cmd = located cmd_

and parser atomic_term_ =
  | "(" term_ ")"
  | type_ -> Univ
  | x:name -> Var x
  | nat -> Nat
  | zero -> Zero
and atomic_term = located atomic_term_

and parser spine_term_ =
  | atomic_term_
  | f:spine_term arg:atomic_term -> App (f, arg)
  | suc arg:spine_term -> Suc arg
and spine_term = located spine_term_

and parser term_ =
  | spine_term_
  | tm:spine_term colon tp:term ->
    Ann {tm; tp}
  | "(" x:name colon base:term ")" arrow fam:term ->
    Pi (base, x, fam)
  | base:spine_term arrow fam:term ->
    Pi (base, "_", fam)
  | binds:name+ maps_to tm:term ->
    Lam (binds, tm)
  | rec_ id:name "[" "|"? zero maps_to z:term "|" suc n:name maps_to s:term "]" ->
    Rec {name = id; zero = z; suc = (n,s)}

and term = located term_