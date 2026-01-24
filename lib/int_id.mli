module type S = Int_id_intf.S

module Make
    (_ : sig
      val name : string
    end)
    () : S
