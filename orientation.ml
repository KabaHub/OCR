(*----------------------------*)
(*---Dimensions d'une image---*)
(*----------------------------*)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

exception Angle_Not_Found

let valabs = function
  |x ->
  if (x < 0 ) then -x else x

(*Scan image créer un tableau contenant le nb de pixels par ligne*)
let scan img =
  let (width, heigth) = get_dims(img) in
  let tab = Array.make (heigth) 0 in
  let current = ref 0 in

  for y = 0 to (heigth-1) do
    let count = ref 0 in
    for x = 0 to width-1 do
      if (Sdlvideo.get_pixel_color img x y) = (0,0,0) then
        count := ((!count) +1);
      if x = width -1 then
        begin
        tab.(!current) <- (!count);
        current := !current +1;
        (*print_int tab.(!current-1); print_string "; "*)
        end;
     done;
   done;
  tab

let get_average tab =
let sum = ref 0 in
  for i = 0 to (Array.length tab -1) do
    sum := !sum + tab.(i)
  done;
  (!sum) / (Array.length tab)

let get_variance tab average =
  let newArray = Array.make (Array.length tab) 0 in
  for i = 0 to (Array.length tab -1) do
    newArray.(i) <- (tab.(i) - average)*(tab.(i) - average)
  done;
  newArray

let get_sum tab =
  let sum = ref 0 in
  for i = 0 to (Array.length tab -1) do
    sum := !sum + tab.(i)
  done;
  !sum



(*On rérupère les coordonnées x-y du 1er pixel noir en partant du haut*)
let rec get_max width height img x y =
  if (y <= (height-1)) then
    if (x <= (width-1)) then
      if (Sdlvideo.get_pixel_color img x y = (255,255,255)) then
        get_max width height img (x+1) y
      else
        let result = (x,y) in
        print_string "Les coordonnÃ©es de max sont : "; print_int (fst result);
        print_string "," ; print_int (snd result);
        print_string " "; print_newline();
        result
    else
      get_max width height img 0 (y+1)
  else raise Angle_Not_Found

 (*On détermine de quel côté on doit effectuer la rotation*)
  let detect_rotation_side width x =
    if (x > width/2) then
      'r'  (*On doit effectuer la rotation à  droite*)
    else
      'l' (*On doit effectuer la rotation à  gauche*)
 

let coord theta width height (x,y) =
  let i =  int_of_float(float_of_int(width)/.2. +.
           cos(theta)*.((float_of_int x)-.(float_of_int(width)/.2.)) -.
           sin(theta)*.((float_of_int y)-.(float_of_int(height)/.2.))) in
  let j = int_of_float(+.(float_of_int(height)/.2.0) +.
          sin(theta)*.((float_of_int x)-.(float_of_int(width)/.2.)) +.
          cos(theta)*.((float_of_int y)-.(float_of_int(height)/.2.))) in
    (i,j)




let putallwhite src dst =
  let (width, height) = get_dims(src) in
    for x = 0 to width-1 do
      for y = 0 to height-1 do
        Sdlvideo.put_pixel_color dst x y (255,255,255)
      done
    done


(*----------------------------------------------------------------------------*)
(*parcourt tous les pixels de l'image src et effectue une rotation de l'image *)
(*----------------------------------------------------------------------------*)
let rotation src dst angle =
  let (width, height) = get_dims(src) in
      (*On récupère les coordonnées x et y qu'aura un point après une rotation*)
      (*Reste à rajouter les coordonnÃ©es y du centre de rotation *)
        for x = 0 to width-1 do
          for y = 0 to height-1 do
            let pixelcolor = (Sdlvideo.get_pixel_color src x y) in
            let (coordx,coordy) = coord angle width height (x,y) in
            if (coordx > 0 && coordy > 0
                  && coordx < width && coordy < height) then
              Sdlvideo.put_pixel_color dst (coordx) (coordy) pixelcolor
     done;
  done;
  print_string("Image rotated.");
  print_newline();
Sdlvideo.save_BMP dst "bin/output/rotatedIMG.bmp"
(*"bin/output/rotatedIMG.bmp"*)
(*---------------------*)
(*---Rotation faite--- *)
(*---------------------*)

let get_angle img dst =
  let (width, height) = get_dims(dst) in
  let current = ref 0 in
  let previous = ref 0 in
(*On détecte dans quel sens on doit effectuer la rotation*)
    let side = detect_rotation_side width (fst(get_max width height img 0 0)) in
    let sumvarArray = Array.make (721) (0) in
(*On délare la fonction récursive qui va permettre de trouver l'angle*)
  let rec recfun i =
    let angle_test = ref 0.0174539 in
(*On effectue la rotation élémentaire d'un degré ici exprimé en radians*)
    if side = 'r' then
      begin
      rotation img dst (i *. !angle_test);
      angle_test := i *.0.0174539;
      end
    else
      begin
      rotation img dst (-.i *. !angle_test);
      angle_test := -.i *.0.0174539;
      end;
(*On récupère le tableau contenant le nombre de pixels de chaque ligne*)
    let tab = scan dst in
(*On fait la moyenne du nombre de pixels contenus dans chaque ligne de l'image*)
    let average = get_average tab in
(*On trouve la variance entre la moyenne et *)
(* le nbre réel de pts noirs sur chaque ligne*)
    let varArray = get_variance tab average in
(*On effectue la somme des éléments du tableau varArray*)
    let sum = get_sum varArray in
(*On met dans le tableau la valeur de la somme des var pour la comparaison *)
    sumvarArray.(!current) <- sum;
(*On teste si jamais l'élément actuel du tableau, donc la var actuelle est sup à
 l'ancienne, sinon on retourne la valeur car on a trouvé le maximum *)
    if ((sumvarArray.(!current)) >= !previous) then
      begin
      previous := sumvarArray.(!current);
      print_int !previous;
      current := !current +1;
      recfun (i+.0.5)
      end
    else
      if i = 0.5 then 0.
      else
    !angle_test
  in recfun 0.
