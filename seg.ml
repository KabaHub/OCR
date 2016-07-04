type corners =
{
        mutable up_left: int*int;
        mutable up_right: int*int;
        mutable down_left: int*int;
        mutable down_right: int*int;
}
let reset_corner corner =
        corner.up_left <- (-1,-1);
        corner.up_right <- (-1,-1);
        corner.down_left <- (-1,-1);
        corner.down_right <- (-1,-1)

let rec draw_rectangle img corners =
  for x = fst(corners.up_left) to fst(corners.up_right) do
    Sdlvideo.put_pixel_color img x (snd(corners.up_left)) (0,0,255);
  done;
  for x = fst(corners.up_left) to fst(corners.up_right) do
    Sdlvideo.put_pixel_color img x (snd(corners.down_left)) (0,0,255);
  done;
  for y = snd(corners.up_left) to snd(corners.down_left) do
    Sdlvideo.put_pixel_color img (fst(corners.up_left)) y (0,0,255);
  done;
  for y = snd(corners.up_left) to snd(corners.down_left) do
    Sdlvideo.put_pixel_color img (fst(corners.up_right)) y (0,0,255);
  done

let scan_lines img =
  let (width, height) = Preprocessing.get_dims(img) in
  (* La liste des coins de chaque rectangle par ligne *)
  let cornersList = ref [] in
  (* Si on a repéré le début d'une ligne *)
  let inLine = ref false in
  (* Les coins en cours *)
  let corner = ref {  up_left=(-1,-1);
		      up_right=(-1,-1);
		      down_left=(-1,-1);
		      down_right=(-1,-1);} in
  (* On parcourt l'image *)
  for y = 0 to height-1 do
  (* La moyenne de gris sur une ligne *)
    let line = ref 0. in
    for x = 0 to width-1 do
      let grey = Preprocessing.level (Sdlvideo.get_pixel_color img x y) in
      (* Si on est sur un pixel noir et qu'on a pas encore initialisé
         ou qu'on a trouvé un pixel plus à gauche
         --> Début de ligne *)
      if (grey = 0. && (fst(!corner.up_left) < 0 || x < fst(!corner.up_left))) 
      then begin
	let bof = ref true in
	(* On regarde si ce n'est pas un pixel isolé -- Comportement bizarre *)
	for i = x to x+2 do
	  if (!bof) then
	    bof := Preprocessing.level (Sdlvideo.get_pixel_color img i y) = 0.;
	done;
	(* Si tout est bon : on enregistre les coordonnées - 
	   mais on garde la première ordonnée enregistrée *)
	if (!bof) then
	begin
	  if (snd(!corner.up_left) < 0) then
	  begin
	    !corner.up_left <- (x,y);
	  end
	  else
	  begin
	    !corner.up_left <- (x,snd(!corner.up_left));
	  end;
	end;
      end;
      (* Si on est sur un pixel blanc *)
      if (grey = 1. && (y > snd(!corner.up_right) && x > fst(!corner.up_right)))
      then begin
	let eof = ref true in
	for i = x to width-2 do
	  if (!eof) then
	    eof := Preprocessing.level (Sdlvideo.get_pixel_color img i y) = 1.;
	done;
	if (!eof) then
	!corner.up_right <- (x-1,y);
      end;
      line := !line +. grey;
    done;
    if ((!line /. float_of_int(width)) <= 0.99) then
    begin
      inLine := true;
      for i = fst(!corner.up_left) to fst(!corner.up_right) do
        let (r,g,b) = Sdlvideo.get_pixel_color img i y in
          Sdlvideo.put_pixel_color img i y (255,g,b-196);
      done;
    end;
    if ((!line /. float_of_int(width)) = 1. && !inLine) then
    begin
      !corner.down_left <- (fst(!corner.up_left),y);
      !corner.down_right <- (fst(!corner.up_right),y);
      cornersList := !corner::!cornersList;
      draw_rectangle img !corner;

                (* DEBUG *)
                (*
                print_newline();
                print_int (fst((List.hd !cornersList).up_left));
                print_string("-");
                print_int (snd((List.hd !cornersList).up_left));
                print_string(";");
                print_int (fst((List.hd !cornersList).up_right));
                print_string("-");
                print_int (snd((List.hd !cornersList).up_right));
                print_string(";");
                print_int (fst((List.hd !cornersList).down_left));
                print_string("-");
                print_int (snd((List.hd !cornersList).down_left));
                print_string(";");
                print_int (fst((List.hd !cornersList).down_right));
                print_string("-");
                print_int (snd((List.hd !cornersList).down_right));
                print_newline();
                *)
                (* DEBUG *)
      reset_corner !corner;
      inLine := false;
    end;

    print_int(100*y/(height-1));
    print_string("%");
    print_newline();

  done;
  Sdlvideo.save_BMP img "bin/output/scannedIMG.bmp"
