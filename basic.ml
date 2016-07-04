(*----------------------------*)
(*---Dimensions d'une image---*)
(*----------------------------*)
let get_dims img =
  let w = (Sdlvideo.surface_info img).Sdlvideo.w in
  let h = (Sdlvideo.surface_info img).Sdlvideo.h in
  (w,h)

type 'a option =
  | Some of 'a
  | Espace

type corners =
    {
     mutable up_left: int*int;
     mutable up_right: int*int;
     mutable down_left: int*int;
     mutable down_right: int*int;
   }

let print_corner corner =
  print_string("(");
  print_int(fst(corner.up_left));
  print_string(" ; ");
  print_int(snd(corner.up_left));
  print_string(") - (");
  print_int(fst(corner.up_right));
  print_string(" ; ");
  print_int(snd(corner.up_right));
  print_string(")");
  print_string(" : (");
  print_int(fst(corner.down_left));
  print_string(" ; ");
  print_int(snd(corner.down_left));
  print_string(") - (");
  print_int(fst(corner.down_right));
  print_string(" ; ");
  print_int(snd(corner.down_right));
  print_string(")");
  print_newline()

let reset_corner corner =
        corner.up_left <- (-1,-1);
        corner.up_right <- (-1,-1);
        corner.down_left <- (-1,-1);
        corner.down_right <- (-1,-1)


let min (a,b) = if (a < b) then a else b


let draw_rectangle img corners length color =
  for i = 0 to length-1 do
    if (length < 300) then
    begin
    print_int(i);
    print_corner corners.(i);
    end;
    for x =fst(corners.(i).up_left) to fst(corners.(i).up_right) do
      Sdlvideo.put_pixel_color img x (snd(corners.(i).up_left)) color;
    done;
    for x =fst(corners.(i).up_left) to fst(corners.(i).up_right) do
      Sdlvideo.put_pixel_color img x (snd(corners.(i).down_left)) color;
    done;
    for y =snd(corners.(i).up_left) to snd(corners.(i).down_left) do
      Sdlvideo.put_pixel_color img (fst(corners.(i).up_left)) y color;
    done;
    for y =snd(corners.(i).up_left) to snd(corners.(i).down_left) do
      Sdlvideo.put_pixel_color img (fst(corners.(i).up_right)) y color;
    done;
  done

let rec draw_rec_rect img corners color =
  match corners with
  | [] -> ()
  | e::l ->
  begin

  for x =fst(e.up_left) to fst(e.up_right) do
      Sdlvideo.put_pixel_color img x (snd(e.up_left)) color;
    done;
    for x =fst(e.up_left) to fst(e.up_right) do
      Sdlvideo.put_pixel_color img x (snd(e.down_left)) color;
    done;
    for y =snd(e.up_left) to snd(e.down_left) do
      Sdlvideo.put_pixel_color img (fst(e.up_left)) y color;
    done;
    for y =snd(e.up_left) to snd(e.down_left) do
      Sdlvideo.put_pixel_color img (fst(e.up_right)) y color;
    done;
  draw_rec_rect img l color;
  end

(* fonction pour uniformiser les dimensions des images des caractères *)
let uniformisation image =
  let (w,h,p) = Sdlvideo.surface_dims image in
  if (w >= h) then
    begin
    let delta = (w-h)/2 in
    let _new = Sdlvideo.create_RGB_surface_format image [] w w in
    for x = 0 to w-1 do
      for y = 0 to w-1 do
        Sdlvideo.put_pixel_color _new x y (255,255,255);
      done;
    done;
    for x = 0 to w-1 do
      for y = delta to w-delta-2 do
        let px = Sdlvideo.get_pixel image x (y-delta) in
        Sdlvideo.put_pixel _new x y px;
      done;
    done;
    _new;
    end
  else
    begin
    let delta = (h-w)/2 in
    let _new = Sdlvideo.create_RGB_surface_format image [] h h in
        for x = 0 to h-1 do
      for y = 0 to h-1 do
        Sdlvideo.put_pixel_color _new x y (255,255,255);
      done;
    done;
    for x = delta to h-delta-2 do
      for y = 0 to h-1 do
        let px = Sdlvideo.get_pixel image (x-delta) y in
        Sdlvideo.put_pixel _new x y px;
      done;
    done;
    _new;
    end

(*-----------------*)
(*---init de SDL---*)
(*-----------------*)
let sdl_init () =
    begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
    end

(*-------------------------*)
(*---attendre une touche---*)
(*-------------------------*)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(*-----------------------------------*)
(*----affiche la surface img sur ----*)
(*---la surface de destination dst---*)
(*-------(normalement l'écran)-------*)
(*-----------------------------------*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
