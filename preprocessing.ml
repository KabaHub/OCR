(*--------------------------------------------------------------------------*)
(* Renvoie un réel entre 0 et 1 représentant l'intensité lumineuse du pixel *)
(*--------------------------------------------------------------------------*)
let level (r,g,b)  =
  let gr = 0.30 *. float_of_int(r) in
  let gg = 0.59 *. float_of_int(g) in
  let gb = 0.11 *. float_of_int(b) in
  (gr +. gg +. gb) /. 255.


(*-------------------------------------------------------------------*)
(* Renvoie un triplet gris en utilisant la fonction level précédente *)
(*-------------------------------------------------------------------*)
let color2grey rgb =
        let grey = int_of_float(255. *. (level rgb)) in
                (grey,grey,grey)

(*---------------------------------------------*)
(* Parcours tous les pixels de l'image src et  *)
(* met le pixel correspondant dans dst en gris *)
(*---------------------------------------------*)
let image2grey src dst =
  let (width, height) = Basic.get_dims(src) in
  for x = 0 to width-1 do
    for y = 0 to height-1 do
      let greyedcolor = color2grey (Sdlvideo.get_pixel_color src x y) in
        Sdlvideo.put_pixel_color dst x y greyedcolor
    done
  done;
  Sdlvideo.save_BMP dst "bin/output/greyedIMG.bmp";
  "bin/output/greyedIMG.bmp"

(*-----------------------------------------*)
(* Amplification des marques de caractères *)
(*-----------------------------------------*)
let sharpen img dest treshold =
  let (w,h) = Basic.get_dims img in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
        let grey = level (Sdlvideo.get_pixel_color img i j) in
          if (grey >= treshold) then
            Sdlvideo.put_pixel_color dest i j (255,255,255)
          else
            Sdlvideo.put_pixel_color dest i j (0,0,0)
      done
    done;
    Sdlvideo.save_BMP dest "bin/output/sharpenedIMG.bmp";
    "bin/output/sharpenedIMG.bmp"




(*-----------------------------------------*)
(* Récupère la moyenne de gris des 8 pixels adjacents *)
(*-----------------------------------------*)
let get_average img i j w h =
        let moyenne = ref 0. in
        for l = -1 to 1 do
                for m = -1 to 1 do
                        if (l <> 0 || m <> 0) then
                        begin
                                let current = Sdlvideo.get_pixel_color img (i+l) (j+m) in
                                let grey = level current in
                                moyenne := !moyenne +. grey
                        end;
                done;
        done;
        let rgb =
        if (i = 0 || j = 0) then
                255
        else
                int_of_float(255. *. (!moyenne /. 8.));
        in
        (rgb,rgb,rgb)


(*--------------------------*)
(* Gommage du bruit de fond *)
(*--------------------------*)
let clean img dest treshold =
        let (w,h) = Basic.get_dims img in
       
                for i = 0 to w-1 do
                        for j = 0 to h-1 do
                                let current = Sdlvideo.get_pixel_color img i j in
                                let (grey,_,_) = color2grey current in
                                if (i <> 0 && j <> 0) &&
                                        (i <> w-1 && j <> h-1)
                                              && (treshold > grey) then
                                        Sdlvideo.put_pixel_color dest i j (get_average img i j w h)
                                else
                                        Sdlvideo.put_pixel_color dest i j current
                        done;
                done;
        print_string("Image cleant.");
        print_newline();
Sdlvideo.save_BMP dest "bin/output/cleantIMG.bmp"

(*--------------------------*)
(* Unification des caractères (suppresions du bruit blanc *)
(*--------------------------*)
let clean_white img dest treshold =
        let (w,h) = Basic.get_dims img in
                for i = 1 to w-2 do
                        for j = 1 to h-2 do
                                let moyenne = ref 0. in
                                for l = -1 to 1 do
                                        for m = -1 to 1 do
                                                let grey = level (Sdlvideo.get_pixel_color img (i+l) (j+m)) in
                                                moyenne := !moyenne +. grey
                                        done;
                                done;
                                if ((!moyenne /. 8.) < treshold) then
                                        Sdlvideo.put_pixel_color dest i j (0,0,0)
                        done;
                done;
               
        print_string("White marks removed with : ");
        print_float(treshold);
        print_string(" treshold.");
        print_newline();
        print_newline();
Sdlvideo.save_BMP dest "bin/output/unifiedIMG.bmp"
