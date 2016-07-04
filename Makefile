#TP sdl
 
 SRC=basic.ml preprocessing.ml binarize.ml orientation.ml segmentation.ml ocr.ml
 OCAML=ocamlopt
 OCAMLFLAGS= -I +sdl -I +site-lib/sdl
 OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
  
ocr: ocr.ml
		${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr ${SRC}
		 
clean::
		rm -f *~ *.o *.cm? ocr
		 
		 # FIN
