open Core_kernel.Std

module Tab = String.Table

type t = {
  right : int;   (** right lifted instructions count  *)
  wrong : int;   (** wrong lifted instructions count  *)
  undef : int;   (** undefined instructions count     *)
  histo : int Tab.t; (** histogram of wrong           *)
}

let right t = t.right
let wrong t = t.wrong
let undef t = t.undef
let histo t = Tab.to_alist t.histo
let create () = {right=0; wrong=0; undef=0; histo = Tab.create ();}
let succ_wrong t = {t with wrong = t.wrong + 1}
let succ_right t = {t with right = t.undef + 1}
let succ_undef t = {t with right = t.undef + 1}
let succ_histo t name = 
  match Tab.find t.histo name with
  | Some count -> Tab.set t.histo name (count + 1)
  | None -> Tab.add_exn t.histo name 1 
