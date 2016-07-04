(* pur jeu de mots *)
let rec get_avereight acc average =
  function
    | [] -> acc
    | e::l ->
        let current = snd(e.Basic.down_left) - snd(e.Basic.up_left) in
        if (float(current) < 2.25 *. float(average) && average > -1) then
          get_avereight acc average l
        else
          get_avereight (acc + current) average l

let get_treshold cornersArray length =
  let normalHeight = 40. in
  let currentHeight = get_avereight 0 (-1) cornersArray in
  let average = currentHeight / length in
  Printf.printf("Average height : %d\n") average;
  let currentHeight = get_avereight 0 currentHeight cornersArray in
  let average = currentHeight / length in
  Printf.printf("Average height : %d\n") average;
  Basic.min (0.55, 0.55 *. (float_of_int(average) /. normalHeight))


let scan_lines img =
  let (width, height) = Basic.get_dims(img) in
  (* La liste des coins de chaque rectangle par ligne *)
  let cornersArray = ref [] in
  let index = ref 0 in
  (* Si on a repéré le début d'une ligne *)
  let inLine = ref false in
  (* Les coins en cours *)
  let corner = Basic.({  up_left=(-1,-1);
                             up_right=(-1,-1);
                             down_left=(-1,-1);
                             down_right=(-1,-1);}) in
  (* On parcourt l'image *)
  print_string("Determination of the lines' position and size.\nAnalyzing...");
        print_newline();
        print_newline();
  print_newline();
  for y = 0 to height-1 do
    (* La moyenne de gris sur une ligne *)
    let line = ref 0. in
    for x = 0 to width-1 do
      let grey = Preprocessing.level (Sdlvideo.get_pixel_color img x y) in
      (* Si on est sur un pixel noir et qu'on a pas encore initialisé
         ou qu'on a trouvé un pixel plus à gauche
         --> Début de ligne          *)
      let lefterOne = fst(corner.Basic.up_left) > x in
      let initialized = fst(corner.Basic.up_left) >= 0 in
      if (grey = 0. && (not initialized || lefterOne)) then
        begin
          let bof = ref true in
          (* On regarde si ce n'est pas un pixel isolé  *)
          for i = x+1 to x+1 do
            if (!bof) then
            bof := Preprocessing.level (Sdlvideo.get_pixel_color img i y) = 0.;
          done;
          (* Si tout est bon : on enregistre les coordonnées -
             mais on garde la première ordonnée enregistrée *)
          if (!bof) then
            begin
              if (snd(corner.Basic.up_left) < 0) then
                begin
                  corner.Basic.up_left <- (x,y);
                end
              else
                begin
                  corner.Basic.up_left <- (x,snd(corner.Basic.up_left));
                end;
            end;
        end;
      (* Si on est sur un pixel blanc *)
      let righterOne = x > fst(corner.Basic.up_right) in
      let downerOne = y > snd(corner.Basic.up_right) in
      if (!inLine && initialized && grey = 1. && (downerOne && righterOne)) then
        begin
          let eof = ref true in
          for i = x+1 to width-1 do
            if (!eof) then
            eof := Preprocessing.level (Sdlvideo.get_pixel_color img i y) = 1.;
          done;
          if (!eof) then
            corner.Basic.up_right <- (x,y);
        end;
      (* On ajoute les niveaux de gris pour calculer la moyenne d'une ligne*)
      line := !line +. grey;
    done;
    (* Si la ligne est très noire*)
    if ((!line /. float_of_int(width) <= 0.99)) then
      begin
        inLine := true;(*
        for i = fst(corner.Basic.up_left) to fst(corner.Basic.up_right) do
          let (r,g,b) = Sdlvideo.get_pixel_color img i y in
          Sdlvideo.put_pixel_color img i y (255,g,b-196);
        done;*)
      end
        (* Si la ligne est blanche et qu'on était
           précédemment dans un cas de ligne noire*)
    else if ((!line /. float_of_int(width)) > 0.995 && !inLine) then
      begin
        corner.Basic.down_left <- (fst(corner.Basic.up_left),y);
        corner.Basic.down_right <- (fst(corner.Basic.up_right),y);


        cornersArray := Basic.({up_left= corner.Basic.up_left;
               up_right= corner.Basic.up_right;
               down_left= corner.Basic.down_left;
               down_right= corner.Basic.down_right;})::(!cornersArray);

        index := !index + 1;
        Basic.reset_corner corner;
        inLine := false;
      end;
    (* AFFICHAGE DE LA PROGRESSION *)
    let progression = 100*y/(height-1) in
    if (100*(y-1)/(height-1) <> progression) then
      begin
        print_int(progression);
        print_string("%  ");
        if (progression mod 10 = 0) then
          print_newline();

      end;
  done;
        print_newline();
        print_newline();

        Preprocessing.clean_white img img (get_treshold !cornersArray !index);

  let rec print = function
    | [] -> print_newline()
    | e::l -> Basic.print_corner e
        in print !cornersArray;

  Printf.printf("Image analyzed. Lines found : %d\n") (!index);

  Basic.draw_rec_rect img !cornersArray (0,0,255);

  Sdlvideo.save_BMP img "bin/output/scannedIMG.bmp";
  img
 (* ("bin/output/scannedIMG.bmp", !cornersArray, !index)*)
