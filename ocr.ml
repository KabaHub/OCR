(* Création de l'image*)
let file = ref ""

(* Main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    Basic.sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = Basic.get_dims img in
    (* On crée la surface d'affichage en- 1 doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    (* On affiche l'image *)
    Basic.show img display;
    (* On attend une touche *)
    Basic.wait_key ();

    (* On crée une nouvelle surface *)
    let greyedimg = Sdlvideo.create_RGB_surface_format img [] w h in
    (* On convertit en gris *)
    file := Preprocessing.image2grey img greyedimg;

    Basic.show greyedimg display;
    Basic.wait_key ();

    (* Binarisation *)
    let binarizedimg = Sdlvideo.create_RGB_surface_format img [] w h in
    file := Binarize.otsu_tresholding greyedimg binarizedimg;

    Basic.show binarizedimg display;
    Basic.wait_key ();

    (* On sharpen = contours *)
    let sharpenedimg = Sdlvideo.create_RGB_surface_format img [] w h in
    file := Preprocessing.sharpen greyedimg sharpenedimg 0.7;

    Basic.show sharpenedimg display;
    Basic.wait_key ();


    (* Rotation *)
    let rotatedimg = Sdlvideo.create_RGB_surface_format img [] w h in
    let tempImg = Sdlvideo.create_RGB_surface_format img [] w h in
    (* On transforme cette surface en image blanche aux bonnes dimensions *)
    Orientation.putallwhite binarizedimg rotatedimg;
    Orientation.putallwhite binarizedimg tempImg;

    (* On détecte l'angle *)
    let angle = Orientation.get_angle binarizedimg tempImg in
    (* On applique la rotation *)
    Orientation.rotation binarizedimg rotatedimg (angle);

    Basic.show rotatedimg display;
    Basic.wait_key ();

    (* Lines correspond au tableau contenant les coordonnées des lignes *)
    Segmentation.scan_lines rotatedimg;

    Basic.show rotatedimg display;
    Basic.wait_key ();

    (*
    (* On scanne les caractères *)
    let (filename, charsArray) =
      Chars.scan_characters rotatedimg (List.rev(lines)) linesLength in
    file := filename;
    *)


    (*
    (*On clean les blancs*)
    let cleant_white = Sdlvideo.create_RGB_surface_format img [] w h in
    Preprocessing.clean_white sharpenedimg cleant_white 0.55;
    *)
    Basic.wait_key ();

    exit 0
  end

let _ = main ()
