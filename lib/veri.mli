open Bap.Std
open Bap_traces.Std

class context: Veri_report.t -> object('s)
    inherit Veri_traci.context
    method report : Veri_report.t
    method update_report : Veri_report.s -> 's
  end
  
class ['a] t : arch -> object('s)
    inherit ['a] Veri_traci.t
    constraint 'a = #context
  end
