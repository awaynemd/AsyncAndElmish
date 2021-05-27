module AsyncAndElmish.Program

open Elmish
open Elmish.WPF
open System
open AsyncAndElmish.View
open System.Printing


type Printer = {
    Id: Guid
    fullname: string             
    comment:string
    defaultPrintTicket: PrintTicket option
    description: string
    isInError: bool
    isOffline: bool
}

let defaultPrinters = [for p in 1..4 -> {fullname="Unknown Printer...Printers being constructed"; Id= Guid.NewGuid(); 
                                         comment="Printer construction"; defaultPrintTicket=None; description = ""; isInError=false; isOffline=false}]


type Model =
  { IsRefreshing: bool
    Printers: Printer list }

type Msg =
  | RefreshMsg 
  | GetPrintersMsg
  | OnPrintersResult of PrintQueue list
  | OnPrintersError of exn

let GetPrinters = 
    let LocalPrintServer = new PrintServer()
    let printQueues = LocalPrintServer.GetPrintQueues [|EnumeratedPrintQueueTypes.Local; EnumeratedPrintQueueTypes.Connections|]
    let printerList =
        printQueues
            |> Seq.cast<PrintQueue>
            |> Seq.toList
    printerList

let init () = 
   { IsRefreshing = false
     Printers = [] }, Cmd.none

let update msg m   = 
    match msg with
    | RefreshMsg -> {m with IsRefreshing = true; Printers = defaultPrinters }, Cmd.ofMsg GetPrintersMsg
    | GetPrintersMsg ->                               
                                let GetPrinters = async {
                                   let LocalPrintServer = new PrintServer()
                                   let printQueues = LocalPrintServer.GetPrintQueues [|EnumeratedPrintQueueTypes.Local; EnumeratedPrintQueueTypes.Connections|]
                                   let printerList =
                                       printQueues
                                           |> Seq.cast<PrintQueue>
                                           |> Seq.toList
                                   return printerList 
                                }

                                let GetPrintersAsync() = 
                                    async {                      
                                            let! token = Async.StartChild(GetPrinters)
                                            let! p = token
                                            return p
                                    }

                                m, Cmd.OfAsync.either GetPrintersAsync ()   OnPrintersResult OnPrintersError 

    | OnPrintersResult printers -> let getprinters =  
                                        printers 
                                             /// The calling thread cannot access this object because a different thread owns it.
                                             
                                             |> List.map (fun pq ->  {fullname = pq.FullName; comment = pq.Comment; defaultPrintTicket= Some pq.DefaultPrintTicket; 
                                                                     description= pq.Description; isInError=pq.IsInError; isOffline=pq.IsOffline; Id= Guid.NewGuid()  } ) 
            
                                   { m with Printers = getprinters; IsRefreshing = false }, Cmd.none

    | OnPrintersError error -> let printerError =
                                 [for p in 1..4 -> {fullname=error.Message; Id= Guid.NewGuid(); comment="Printer construction"; defaultPrintTicket=None; description = ""; isInError=false; isOffline=false}]
                                  
                               {m with IsRefreshing = false; Printers = printerError }, Cmd.none
         

let bindings () : Binding<Model, Msg> list = [
   "IsRefreshing"     |> Binding.oneWay(fun m -> m.IsRefreshing)
   "RefreshCommand"   |> Binding.cmd RefreshMsg
   "Printers"         |> Binding.oneWay(fun m -> m.Printers)
]

[<EntryPoint; STAThread>]
let main argv =
   //Run it this way when finished.
    Program.mkProgramWpf init update bindings
    |> Program.runWindow  (MainWindow())
