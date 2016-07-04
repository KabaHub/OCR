(*------------------------*)
(* Dimensions d'une image *)
(*------------------------*)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)


(*--------------------------------------------------------------------------*)
(* Renvoie un réel entre 0 et 1 représentant l'intensité lumineuse du pixel *)
(*--------------------------------------------------------------------------*)
let level (r,g,b)  =
         (0.30 *. float_of_int(r) +. 0.59 *. float_of_int(g)
       +. 0.11 *. float_of_int(b)) /. 255.


(*-------------------------------------------------------------------*)
(* Renvoie un triplet gris en utilisant la fonction level précédente *)
(*-------------------------------------------------------------------*)
let color2grey rgb =
        let grey = int_of_float(255. *. (level rgb)) in
                (grey,grey,grey)

(*--------------------------------------------*)
(* Parcours tous les pixels de l'image src et *)
(* met le pixel correspondant dans dst en gris*)
(*--------------------------------------------*)
let image2grey src dst =
        let (width, height) = get_dims(src) in
                for x = 0 to width-1 do
                        for y = 0 to height-1 do
                        let greyedcolor =
	color2grey (Sdlvideo.get_pixel_color src x y) in
                        Sdlvideo.put_pixel_color dst x y greyedcolor
                        done
                done;
Sdlvideo.save_BMP dst "bin/output/greyedIMG.bmp"

(*-----------------------------------------*)
(* Amplification des marques de caractères *)
(*-----------------------------------------*)
let binarize img dest treshold =
  let (w,h) = get_dims img in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	let grey = 255.*.level (Sdlvideo.get_pixel_color img i j) in
	if (int_of_float(grey) >= treshold) then
		Sdlvideo.put_pixel_color dest i j (255,255,255)
	else
		Sdlvideo.put_pixel_color dest i j (0,0,0)
      done
    done;
Sdlvideo.save_BMP dest "bin/output/sharpenedIMG.bmp"


let otsu_tresholding img dest =
  let (width,height) = get_dims img in
  let sum = ref 0 and sumB = ref 0 in
  let wB = ref 0 and wF = ref 0 in
  let varMax = ref 0. and treshold = ref 0 in
  let hist = ref [||] in
      hist := Array.make 256 0;
      for x = 0 to width-1 do
	for y = 0 to height-1 do
	  let currentColor = level (Sdlvideo.get_pixel_color img x y) in
	  let current = int_of_float(255. *. currentColor) in
		!hist.(current) <- !hist.(current) + 1;
        done;
      done;
      for i = 0 to 255 do
	sum := !sum + i * !hist.(i);
      done;
  let break = ref false in
    for i = 0 to 255 do
      if (not !break) then
      begin
	wB := !wB + !hist.(i);
	if (!wB <> 0) then
	begin
	  wF := (width*height) - !wB;
	  if (!wF = 0) then
	  begin
	    break := true;
	  end
	  else
	  begin
	    sumB := !sumB + (i * !hist.(i));
	    let mB = (float)(!sumB/(!wB)) and 
		mF = (float)((!sum-(!sumB))/(!wF)) in
	    let varB = (float) !wB *.(float) !wF *.(mB-.mF) *.(mB-.mF) in
	      treshold := if varB > !varMax then i else !treshold;
	      varMax := if varB > !varMax then varB else !varMax;
	   end;
	 end;
       end;
     done;
     print_int(!treshold);
     print_newline();
     print_float(!varMax);
     print_newline();
     binarize img dest !treshold

(*----------------------------------------------------*)
(* Récupère la moyenne de gris des 8 pixels adjacents *)
(*----------------------------------------------------*)
let get_average img i j w h =
  let moyenne = ref 0. in
    for l = -1 to 1 do
      for m = -1 to 1 do
      (*if (l <> 0 || m <> 0) then *)
	begin
	let current = Sdlvideo.get_pixel_color img (i+l) (j+m) in
	let grey = level current in
	  moyenne := !moyenne +. grey
	end;
      done;
    done;
  let rgb =
    if (i = 0 || j = 0) then  255
    else
	int_of_float(255. *. (!moyenne /. 9.));
  in
  (rgb,rgb,rgb)


(*--------------------------*)
(* Gommage du bruit de fond *)
(*--------------------------*)
let clean img dest =
  let (w,h) = get_dims img in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	Sdlvideo.put_pixel_color dest i j (get_average img i j w h);
      done;
    done;
Sdlvideo.save_BMP dest "bin/output/cleantIMG.bmp"
 
