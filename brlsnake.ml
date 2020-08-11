let sleep_seconds = 0.2

module ANSI = struct
  let hide_cursor = "\027[?25l"

  let show_cursor = "\027[?25h"

  let save_position = "\0277"

  let restore_position = "\0278"
end

let sigint = 2

type point = { x : int; y : int }

type direction = { dx : int; dy : int }

let offset { x; y } { dx; dy } =
  { x = x + dx; y = y + dy }

module Screen : sig
  type t = bool array array

  val width : int

  val height : int

  val create : unit -> t

  val mem : point -> bool

  val get : t -> point -> bool

  val set : t -> point -> bool -> unit

  val print : t -> unit
end = struct
  type t = bool array array

  let column = 80

  let width = column * 2

  let height = 4

  let create () =
    Array.make_matrix width height false

  type dot = { index : int; position : point }

  let get screen { x; y } =
    screen.(x).(y)

  let mem { x; y } =
     x >= 0 && y >= 0 && x < width && y < height

  let set screen { x; y } value =
    screen.(x).(y) <- value

  let dots =
    [{ index = 1; position = { x = 0; y = 0 }};
      { index = 2; position = { x = 0; y = 1 }};
      { index = 3; position = { x = 0; y = 2 }};
      { index = 4; position = { x = 1; y = 0 }};
      { index = 5; position = { x = 1; y = 1 }};
      { index = 6; position = { x = 1; y = 2 }};
      { index = 7; position = { x = 0; y = 3 }};
      { index = 8; position = { x = 1; y = 3 }}]

  let braille_pattern_blank = 0x2800

  let to_string screen =
    let buffer = Buffer.create (column * 3) in
    for i = 0 to 79 do
      let sum_dots sum { index; position = { x; y }} =
        if screen.(i * 2 + x).(y) then
          sum + 1 lsl (index - 1)
        else
          sum in
      let sum = List.fold_left sum_dots 0 dots in
      Buffer.add_utf_8_uchar buffer (Uchar.of_int (braille_pattern_blank + sum))
    done;
    Buffer.contents buffer

  let print screen =
    print_string ANSI.save_position;
    print_string (to_string screen);
    print_string ANSI.restore_position;
    flush stdout
end

let read_char () =
  let bytes = Bytes.create 1 in
  try
    if Unix.read Unix.stdin bytes 0 1 = 1 then
      Some (Bytes.get bytes 0)
    else
      None
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    None

let (let*) = Option.bind

let check b =
  if b then
    Some ()
  else
    None

let check_sigint () =
  if List.mem sigint (Unix.sigpending ()) then
    raise Exit

let read_direction () =
  let* c = read_char () in
  let* () = check (c = '\027') in
  let* c = read_char () in
  let* () = check (c = '[') in
  let* c = read_char () in
  match c with
  | 'A' -> Some { dx = 0; dy = -1 }
  | 'B' -> Some { dx = 0; dy = 1 }
  | 'C' -> Some { dx = 1; dy = 0 }
  | 'D' -> Some { dx = -1; dy = 0 }
  | _ -> None

let rec wait_direction () =
  check_sigint ();
  match read_direction () with
  | Some direction -> direction
  | None -> wait_direction ()

module Tail = struct
  type 'a t = {
      length : int;
      front : 'a list;
      back : 'a list;
    }

  let empty = {
    length = 0 ;
    front = [];
    back = [];
  }

  let push item tail =
    { tail with length = succ tail.length; front = item :: tail.front }

  let pop tail =
    match tail.back with
    | item :: back ->
        item, { tail with length = pred tail.length; back }
    | [] ->
        match List.rev tail.front with
        | item :: back ->
            item, { length = pred tail.length; front = []; back }
        | [] -> invalid_arg "pop"
end

let rec draw_new_goal screen =
  let goal = { x = Random.int Screen.width; y = Random.int Screen.height } in
  if Screen.get screen goal then
    draw_new_goal screen
  else
    begin
      Screen.set screen goal true;
      goal
    end

let rec loop screen position direction tail length goal =
  check_sigint ();
  let tail = Tail.push position tail in
  let position = offset position direction in
  let length, goal =
    if position = goal then
      succ length, draw_new_goal screen
    else if not (Screen.mem position) || Screen.get screen position then
      begin
        Printf.printf "\nGame over (length: %d).\n" length;
        raise Exit
      end
    else
      length, goal in
  Screen.set screen position true;
  Screen.print screen;
  Unix.sleepf sleep_seconds;
  let direction = Option.value ~default:direction (read_direction ()) in
  let tail =
    if tail.length + 1 >= length then
      begin
        let item, tail = Tail.pop tail in
        Screen.set screen item false;
        tail
      end
    else
      tail in
  loop screen position direction tail length goal

let with_non_canonical_terminal f =
  let terminfo = Unix.tcgetattr Unix.stdin in
  let newterminfo =
    { terminfo with c_icanon = false; c_vmin = 0; c_vtime = 0;
      c_echo = false } in
  Unix.tcsetattr Unix.stdin TCSAFLUSH newterminfo;
  print_string ANSI.hide_cursor;
  let finally () =
    print_string ANSI.show_cursor;
    flush stdout;
    Unix.tcsetattr Unix.stdin TCSAFLUSH terminfo in
  ignore (Unix.sigprocmask SIG_BLOCK [sigint]);
  Fun.protect ~finally f

let main () =
  Random.self_init ();
  let screen = Screen.create () in
  let initial_position = { x = Screen.width / 2; y = Screen.height / 2 } in
  try
    with_non_canonical_terminal (fun () ->
      Screen.set screen initial_position true;
      Screen.print screen;
      let direction = wait_direction () in
      Unix.set_nonblock Unix.stdin;
      let goal = draw_new_goal screen in
      loop screen initial_position direction Tail.empty 3 goal)
  with Exit -> ()

let () =
  main ()
