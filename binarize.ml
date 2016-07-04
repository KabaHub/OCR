(*-------------------------*)
(* Binarisation de l'image *)
(*-------------------------*)
let binarize img dest treshold =
  let (w,h) = Basic.get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let grey = Preprocessing.level (Sdlvideo.get_pixel_color img i j) in
      if (int_of_float(255.*.grey) >= treshold) then
        Sdlvideo.put_pixel_color dest i j (255,255,255)
      else
        Sdlvideo.put_pixel_color dest i j (0,0,0)
    done
  done;
  print_string("Image binarized.");
  print_newline();
  Sdlvideo.save_BMP dest "bin/output/binarizedIMG.bmp"

(*--------------------------------*)
(* Détection automatique du seuil *)
(*--------------------------------*)
let otsu_tresholding img dest =
  let (width,height) = Basic.get_dims img in
  let sum = ref 0 and sumB = ref 0 in
  let wB = ref 0 and wF = ref 0 in
  let varMax = ref 0. and treshold = ref 0 in
  let hist = Array.make 256 0 in
    for x = 0 to width-1 do
      for y = 0 to height-1 do
	let currentColor = (Sdlvideo.get_pixel_color img x y) in
	let current =
	  int_of_float(255. *. (Preprocessing.level currentColor)) in
	hist.(current) <- hist.(current) + 1;
      done;
    done;
    for i = 0 to 255 do
      sum := !sum + i * hist.(i);
    done;
    let break = ref false in
      for i = 0 to 255 do
	if (not !break) then
	begin
	wB := !wB + hist.(i);
	  if (!wB <> 0) then
	  begin
	    wF := (width*height) - !wB;
	    if (!wF = 0) then
	    begin
	      break := true;
	    end
	    else
	    begin
	      sumB := !sumB + (i * hist.(i));
	      let mB = (float)(!sumB/(!wB)) in
	      let mF = (float)((!sum-(!sumB))/(!wF)) in
	      let varB = (float) !wB *.(float) !wF *.(mB-.mF) *.(mB-.mF) in
	      if (varB > !varMax) then
		begin
		treshold := i;
		varMax := varB;
	      end;
	    end;
	  end;
	end;
      done;
      print_string("Otsu's treshold found : ");
      print_int(!treshold);
      print_newline();
      Preprocessing.clean img dest !treshold;
      binarize img dest !treshold;
      "bin/output/binarizedIMG.bmp"
