[%%shared
open Eliom_lib
open Eliom_content
open Html.D
]

module Myh42n42_app =
  Eliom_registration.App (
  struct
    let application_name = "myh42n42"
    let global_data_path = None
  end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()



let index =
	body
	[
		Init.gameArea
	]

let () =
  Myh42n42_app.register ~service:main_service
    (fun () () ->
		let _ = [%client (Init.initalizer () : unit)] in
       Lwt.return
         (Eliom_tools.D.html ~title:"myh42n42" ~css:[["css";"myh42n42.css"]] index)
	)
