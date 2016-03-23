open Core_kernel.Std
open Bap.Std
open Bap_traces.Std
open Bil.Result

class context: Veri_report.t -> arch -> object('s)
    inherit Veri_traci.context
    method split : 's
    method merge : 's
    method register_event : Trace.event -> 's
    method events : Trace.event list
    method replay : 's      
    method main : 's
    method shadow: 's
    method report: Veri_report.t
    method set_description: string -> 's
  end
  
class ['a] t : (Trace.event -> bool) -> object('s)
    inherit ['a] Veri_traci.t
    constraint 'a = #context
    method eval_trace: Trace.t -> 'a u
  end
