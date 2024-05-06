[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D

	let areaWidth = 500
	let areaHeight = 500
	let gameArea = div ~a:[a_class ["container"]] []
]

[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

type point =
{
	xPos : int;
	yPos : int;
}

type creet_type = Healthy | Sick | Berserk | Mean

type mcache =
{
	mutable xDiff : int;
	mutable yDiff : int;
	mutable multed : int;
	mutable xPos : int;
	mutable yPos : int;
	mutable active : bool;
}

type monster =
{
	mutable xPos : float;
	mutable yPos : float;
	mutable typee : creet_type;
	mutable dirX : int;
	mutable dirY : int;
	mutable speed : float;
	mutable size : float;
	mutable id : int;
	mutable lastTouchId : int;
	elt : Html_types.div elt;
	dom: Dom_html.divElement Js.t;
	mutable cache : mcache;
	mutable available : bool;
}

type gameData =
{
	mutable loopCount : int;
	mutable speedIter : float;
	mutable monsters : monster list;
	mutable lastId : int;
	mutable counterelt: Html_types.div elt;
}

let monsterDeafultSpeed : float = 0.50 

let make_healty monster =
	monster.speed <- monsterDeafultSpeed;
	monster.size <- 45.;
	monster.typee <- Healthy;
	monster.dom##.style##.backgroundColor := Js.string("black");
	monster.dom##.style##.width := Js.string(Printf.sprintf "%fpx" monster.size);
	monster.dom##.style##.height := Js.string(Printf.sprintf "%fpx" monster.size)


let _event_handler monster event =
	let radius = monster.size /. 2. in
	let left = float_of_int event##.clientX -. radius in
	let top = float_of_int event##.clientY -. radius in
	monster.xPos <- left;
	monster.yPos <- top;
	monster.dom##.style##.left := Js.string(Printf.sprintf "%fpx" left);
	monster.dom##.style##.top := Js.string(Printf.sprintf "%fpx" top)


let _handle_events monster mouse_down _ =
	monster.available <- false;
	monster.dom##.style##.cursor := Js.string "grabing";
	_event_handler monster mouse_down;
	Lwt.pick [
      mousemoves Dom_html.document (fun mouse_move _ ->
    	_event_handler monster mouse_move;
    	Lwt.return ());
      (let%lwt mouse_up = mouseup Dom_html.document in
		_event_handler monster mouse_up;
		if monster.yPos > float_of_int (areaHeight - (areaHeight / 5)) then make_healty monster;
		monster.available <- true;
		monster.dom##.style##.cursor := Js.string "grab";
		Lwt.return ());
	]

let createMonster data =
	let tmpxPos = Random.float (float_of_int areaWidth) in
	let tmpyPos = Random.float (float_of_int (areaHeight - (areaHeight / 5))) in
	let tmpyPos = tmpyPos +. float_of_int (areaHeight / 5) in
	let tmpDirX = Random.int 2 in
	let tmpDirY = Random.int 2 in
	let monsterDiv = div ~a:[a_class ["monster"]] [] in
	let cache =
	{
		xDiff = 0;
		yDiff = 0;
		multed = 0;
		xPos = 0;
		yPos = 0;
		active = false;
	} in
	let mymonster =
	{
		xPos = tmpxPos;
		yPos = tmpyPos;
		dirX = tmpDirX;
		dirY = tmpDirY;
		typee = Healthy;
		speed = monsterDeafultSpeed;
		size = 45.;
		id = data.lastId;
		lastTouchId = 0;
		elt = monsterDiv;
		dom = Html.To_dom.of_div monsterDiv;
		cache;
		available = true;
	} in
	let counterDiv = div ~a:[a_class ["creetcounter"]] [txt (Printf.sprintf "%d creet" data.lastId)] in
	Html.Manip.replaceSelf data.counterelt counterDiv;
	data.counterelt <- counterDiv;
	data.lastId <- data.lastId + 1;
	mymonster.dom##.style##.backgroundColor := Js.string("black");
	mymonster.dom##.style##.width := Js.string(Printf.sprintf "%fpx" mymonster.size);
	mymonster.dom##.style##.height := Js.string(Printf.sprintf "%fpx" mymonster.size);
	mymonster.dom##.style##.left := Js.string(Printf.sprintf "%fpx" mymonster.xPos);
	mymonster.dom##.style##.top := Js.string(Printf.sprintf "%fpx" mymonster.yPos);
	Lwt.async (fun () -> mousedowns mymonster.dom (_handle_events mymonster));
	mymonster


let make_sick monster =
	let rand = Random.int 100 in
	if rand < 10 then (
		(*berserk*)
		monster.dom##.style##.backgroundColor := Js.string("orange");
		monster.typee <- Berserk;
		monster.speed <- monsterDeafultSpeed -. (monsterDeafultSpeed /. 100. *. 15.);
	)
	else if rand >= 10 && rand < 20 then (
		(*mean*)
		monster.dom##.style##.backgroundColor := Js.string("purple");
		monster.typee <- Mean;
		monster.speed <- monsterDeafultSpeed -. (monsterDeafultSpeed /. 100. *. 15.);
		monster.size <- 30.;
		monster.dom##.style##.width := Js.string(Printf.sprintf "%fpx" monster.size);
		monster.dom##.style##.height := Js.string(Printf.sprintf "%fpx" monster.size);
	)
	else (
		(*sick*)
		monster.dom##.style##.backgroundColor := Js.string("red");
		monster.typee <- Sick;
		monster.speed <- monsterDeafultSpeed -. (monsterDeafultSpeed /. 100. *. 15.);
	);
	()

let distance m1 m2 =
	sqrt (((m1.xPos +. (m1.size /. 2.)) -. (m2.xPos +. (m2.size /. 2.))) ** 2. +. ((m1.yPos +. (m1.size /. 2.)) -. (m2.yPos +. (m2.size /. 2.))) ** 2.)

let _moveMonster monster data =
	if monster.available = true then (
	let nearest_monster = ref (Obj.magic ()) in
	let min_distance = ref infinity in
	if monster.typee != Healthy then (
		List.iter (fun datamonster ->
			if datamonster != monster then (
				let dist = distance datamonster monster in
				if dist < !min_distance && datamonster.typee = Healthy then (
					min_distance := dist;
					nearest_monster := datamonster;
				);
			)
		) data.monsters;
	);
	if data.loopCount mod 500 = 0 then (
		monster.dirX <- Random.int 2;
		monster.dirY <- Random.int 2;
	);
	if monster.typee = Berserk && data.loopCount mod 20 = 0 then (
		monster.size <- monster.size +. 0.1;
		monster.dom##.style##.width := Js.string(Printf.sprintf "%fpx" monster.size);
		monster.dom##.style##.height := Js.string(Printf.sprintf "%fpx" monster.size);
	);
	if monster.typee = Mean && data.loopCount mod 200 = 0 then (
		if !nearest_monster.xPos > monster.xPos then monster.dirX <- 1;
		if !nearest_monster.xPos < monster.xPos then monster.dirX <- 0;
		if !nearest_monster.yPos > monster.yPos then monster.dirY <- 1;
		if !nearest_monster.yPos < monster.yPos then monster.dirY <- 0;
	);
	if monster.dirX = 0 then ( (*x azaltılacak sola gidiyor*)
		if monster.xPos -. monster.speed -. data.speedIter < 0. then ( (*monster arena dışına çıkıyor*)
			monster.dirX <- 1;
			monster.xPos <- monster.xPos +. monster.speed +. data.speedIter;
		)
		else (
			monster.xPos <- monster.xPos -. monster.speed -. data.speedIter;
		)
	)
	else ( (*x arttırılacak monster saga gidiyor*)
		if monster.xPos +. monster.speed +. monster.size +. data.speedIter > float_of_int areaWidth then ( (*monster arenanın dışına çıkıyor*)
			monster.dirX <- 0;
			monster.xPos <- monster.xPos -. monster.speed -. data.speedIter;
		)
		else (
			monster.xPos <- monster.xPos +. monster.speed +. data.speedIter;
		)
	);
	if monster.dirY = 0 then ( (* y azaltılacak yukarı gidiyor *)
		if monster.yPos -. monster.speed -. data.speedIter < 0. then ( (* monster arena dışına çıkıyor *)
			monster.dirY <- 1;
			monster.yPos <- monster.yPos +. monster.speed +. data.speedIter;
		)
		else (
			monster.yPos <- monster.yPos -. monster.speed -. data.speedIter;
    	)
	)
	else ( (* y arttırılacak monster aşağı gidiyor *)
		if monster.yPos +. monster.speed +. monster.size +. data.speedIter > float_of_int areaHeight then ( (* monster arenanın dışına çıkıyor *)
			monster.dirY <- 0;
			monster.yPos <- monster.yPos -. monster.speed -. data.speedIter;
		)
		else (
			monster.yPos <- monster.yPos +. monster.speed +. data.speedIter;
		)
	);
	monster.dom##.style##.left := Js.string(Printf.sprintf "%fpx" monster.xPos);
	monster.dom##.style##.top := Js.string(Printf.sprintf "%fpx" monster.yPos);
	

	if monster.yPos < float_of_int (areaHeight / 5) && monster.typee = Healthy then make_sick monster;

	(*
	if !min_distance > (monster.size /. 2.) +. ((!nearest_monster.size) /. 2.) then (
		monster.lastTouchId <- 0;
	);*)
	if monster.typee != Healthy && !nearest_monster.typee = Healthy (*&& monster.lastTouchId != !nearest_monster.id*) && !nearest_monster.available = true && !min_distance <= (monster.size /. 2.) +. ((!nearest_monster.size) /. 2.) then (
		(*monster.lastTouchId <- !nearest_monster.id;*)
		let rand = Random.int 100 in
		if rand < 2 then make_sick !nearest_monster;
	);
	()
	)
	else (
		()
	)


let rec gameLoop data =
	let%lwt () = Lwt_js.sleep 0.001 in
	data.loopCount <- data.loopCount + 1;
	if List.exists (fun x -> x.typee = Healthy) data.monsters = false then (
		alert "Game Over!";
		Lwt.return ()
	)
	else (
		if data.loopCount > 1000 then (
			data.loopCount <- 0;
			let monster = createMonster data in
			Html.Manip.appendChild ~%gameArea monster.elt;
			data.monsters <- monster :: data.monsters;
			data.speedIter <- data.speedIter +. 0.1;
		);
		List.iter (fun monster -> _moveMonster monster data) data.monsters;
		gameLoop data
	)

let initalizer () =
	Random.self_init ();

	let dom = Html.To_dom.of_div gameArea in
	dom##.style##.width := Js.string (Printf.sprintf "%dpx" areaWidth);
	dom##.style##.height := Js.string (Printf.sprintf "%dpx" areaHeight);
	Html.Manip.replaceSelf gameArea gameArea;

	let river = div ~a:[a_class ["river"]] [] in
	let hospital = div ~a:[a_class ["hospital"]] [] in
	let riverDom = Html.To_dom.of_div river in
	let hospitalDom = Html.To_dom.of_div hospital in
	riverDom##.style##.width := Js.string (Printf.sprintf "%dpx" areaWidth);
	riverDom##.style##.height := Js.string (Printf.sprintf "%dpx" (areaHeight / 5));
	
	hospitalDom##.style##.width := Js.string (Printf.sprintf "%dpx" areaWidth);
	hospitalDom##.style##.height := Js.string (Printf.sprintf "%dpx" (areaHeight / 5));
	hospitalDom##.style##.marginTop := Js.string (Printf.sprintf "%dpx" (areaHeight - (areaHeight / 5 * 2)));

	Html.Manip.appendChild ~%gameArea river;
	Html.Manip.appendChild ~%gameArea hospital;

	let counterDiv = div ~a:[a_class ["creetcounter"]] [txt "creet count = 0"] in
	Html.Manip.appendChild ~%gameArea counterDiv;
	let data : gameData = {loopCount = 0; speedIter = 0.; monsters = []; lastId = 1; counterelt = counterDiv;} in

	let monster = createMonster data in
	Html.Manip.appendChild ~%gameArea monster.elt;
	data.monsters <- monster :: data.monsters;

	Lwt.async (fun () -> gameLoop data)

]