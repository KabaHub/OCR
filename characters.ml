let get_area_average l =
  let sum_area = ref 0 in
  let sum_height = ref 0 in
  let sum_width = ref 0 in
  for i = 0 to (List.length l)-1 do
    let e = List.nth l i in
    let height = (snd(e.Basic.down_left) - snd(e.Basic.up_left)) in
    let width = (fst(e.Basic.down_right) - fst(e.Basic.down_left)) in
    sum_area := !sum_area + width*height;
    sum_height := !sum_height + height;
    sum_width := !sum_width + width;
  done;
  (float !sum_area /. float ((List.length l)-1),
   float !sum_width /. float ((List.length l)-1),
   float !sum_height /. float ((List.length l)-1))


let area_cleaner lst length treshold =
  print_string("Cleaning areas that are too small...");
  let rec aux newlist = function
  | [] -> print_newline(); newlist
  | e::l1 ->
    let height = (snd(e.Basic.down_left) - snd(e.Basic.up_left)) in
    let width = (fst(e.Basic.down_right) - fst(e.Basic.down_left)) in
    if (float (width * height) < treshold /. 20. ||
            float (width * height) > treshold *. 10.) then
    begin
      length := !length - 1;
      aux newlist l1
    end
    else
      aux (e::newlist) l1
  in
   (aux [] lst)

let add_space lst length width_avg height_avg =
  print_string("Adding spaces to the equation...\n");
  let rec aux newlist = function
  | [] -> newlist
  | e::[] -> (Basic.Some e)::newlist
  | curr::next::l1 ->
    (*Basic.print_corner curr;*)
	let x_gap = fst(next.Basic.up_left) - fst(curr.Basic.down_right) in
	let y_gap = snd(next.Basic.down_left) - snd(curr.Basic.down_left) in(*
	Printf.printf("Next Down :  %d ; Curr Up : %d ; Gap : %d |
		       Average Width : %f\n") (snd(next.Basic.down_left))
		       (snd(curr.Basic.down_left)) (y_gap) (height_avg);*)
    if (float x_gap >  width_avg/.2.) || (float y_gap >  height_avg/.2.) then
    begin
      length := !length + 1;
      aux (Basic.Espace::Basic.Some curr::newlist) (next::l1)
    end
    else
      aux (Basic.Some curr::newlist) (next::l1)
  in
    List.rev(aux [] lst)




let scan_characters img lines length =
  let cornersArray = ref [] in
  let index = ref 0 in
  (* Les coins en cours *)
  let corner = ref Basic.({  up_left=(-1,-1);
                             up_right=(-1,-1);
                             down_left=(-1,-1);
                             down_right=(-1,-1);}) in
  let inLine = ref false in
  (* On parcourt l'image *)
  let rec aux = function
  | [] -> ()
  | e::l ->
  inLine := false;
  corner := Basic.({  up_left=(-1,-1);
               up_right=(-1,-1);
               down_left=(-1,-1);
               down_right=(-1,-1);});
    for x = fst(e.Basic.up_left) to fst(e.Basic.up_right) do
    (* La moyenne de gris sur une ligne *)
    let line = ref 0. in
      for y = snd(e.Basic.up_left) to snd(e.Basic.down_left) do
        let grey = Prepr.level (Sdlvideo.get_pixel_color img x y) in
        line := !line +. grey;
        (* Si il y a du noir *)
        let upperOne = y < snd(!corner.Basic.up_left) in
        if (grey = 0. && (!corner.Basic.up_left = (-1,-1) || upperOne)) then
        begin
          if (!corner.Basic.up_left = (-1,-1)) then
          begin
            !corner.Basic.up_left <- (x,y);
          end
          else
          begin
            !corner.Basic.up_left <- (fst(!corner.Basic.up_left),y);
          end
        end;
        (* S'il y a du blanc *)
        if (grey = 1. && !inLine) then
        begin
        let currentX =
          if (x > fst(!corner.Basic.down_left) &&
                  fst(!corner.Basic.down_left) <> -1) then
           fst(!corner.Basic.down_left)
          else
            x
          in
 
        let eof = ref true in
        for j = y+1 to snd(e.Basic.down_left) do
          if (!eof) then
            eof := Prepr.level (Sdlvideo.get_pixel_color img x j) = 1.;
        done;
        if (!eof) then
          !corner.Basic.down_left <- (currentX,y);
        end;
        done;
    let height = 1 + (snd(e.Basic.down_left) - snd(e.Basic.up_left)) in
    (* TRESHOLD DETERMINATION : APPROXIMATIF *)
    let normalHeight = 24. in
    let treshold = 0.95 +. 
	Basic.min (0.95, 0.95 *. (float_of_int(height) /. normalHeight)) in
    (* TRESHOLD DETERMINATION : APPROXIMATIF *)
    let average = !line /. float_of_int(height) in
    if (average <= treshold/.2.) then
    begin
      inLine := true
    end;
    if (!inLine && average > treshold/.2. &&
        !corner.Basic.up_left <> (-1,-1)) then
    begin
    !corner.Basic.up_right <- (x,snd(!corner.Basic.up_left));
    !corner.Basic.down_right <- (x,snd(!corner.Basic.down_left));

        cornersArray := Basic.({up_left= !corner.Basic.up_left;
				up_right= !corner.Basic.up_right;
				down_left= !corner.Basic.down_left;
				down_right= !corner.Basic.down_right;})
				::(!cornersArray);

    index := !index + 1;
    corner := Basic.({  up_left=(-1,-1);
                             up_right=(-1,-1);
                             down_left=(-1,-1);
                             down_right=(-1,-1);})
    end
    done;
    aux l
  in
  aux lines;
  let (area_avg,width_avg,height_avg) = get_area_average (!cornersArray) in
  Printf.printf("Image analyzed. Chars found : %d\nAverage Width :
		 %f\nAverage Height : %f\nAverage Area : %f\n")
  (!index) area_avg width_avg height_avg;
  cornersArray := area_cleaner
                    !cornersArray index
                       area_avg;
  (*Basic.draw_rec_rect img !cornersArray (0,0,255);*)
  Sdlvideo.save_BMP img "bin/output/scannedCharIMG.bmp";
  let charsArray = add_space (!cornersArray) (index) width_avg height_avg in
  print_newline();
  Printf.printf("Image analyzed. Chars found : %d\n")(!index);
  print_newline();
  ("bin/output/scannedCharIMG.bmp",charsArray)
