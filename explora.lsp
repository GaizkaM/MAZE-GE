; -----------------------------------------------------------------------------------------------
; Pràctica Final - Llenguatges de Programació (LISP)
; Curs 2024-25
; Nom: Gaizka Medina Gordo
; Grup: PF3-26 (Grup 301)
; Professor: Antoni Oliver Tomàs
; Convocatòria: Extraordinària
; Data: 26/06/2025
; -----------------------------------------------------------------------------------------------
; ARXIU QUE REALITZA L'EXPLORACIÓ DEL LABERINT PER PART DE L'USUARI
;
; En aquest arxiu trobam totes les funcions encarregades de realitzar l'exploració del laberint
; per part de l'usuari
; -----------------------------------------------------------------------------------------------
; ÚS FUNCIÓ -> carregar l'arxiu a XLISP-PLUS
; cridar a (load"explora.lsp")
;
; Exemple d'Ús de funció:
;
; (explora "laberint.txt")  *El laberint ha d'estar generat (veure genera.lsp)
;
; -----------------------------------------------------------------------------------------------
; Funció principal explora que s'encarrega d'inicialitzar l'exploració d'un laberint per part de
; l'usuari.
;
; El primer que fa el programa es inicialitzar totes les variables que s'han d'emprar per a poder
; dibuixar el laberint. Cada variable s'inicialitza amb la funció corresponent que la genera, i
; es creen totes 
;
; al començament, la casella jugador representa l'entrada del laberint
;
; la casella-actual representa la casella on es realitza el moviment
;
;
; @param nom-fitxer Fitxer a llegir on es troba el laberint a explorar per l'usuari
; @return el dibuix sencer del laberint i després crida a la funció interna recursiva
; -----------------------------------------------------------------------------------------------
(defun explora (nom-fitxer)
    (let* 
        ((amplada-finestra 600)
        (altura-finestra 350)
        ; Inicialitzam el nom del jugador demanant a l'usuari per pantalla
        (nom-jugador (llegir-nom))
        ; Empram el reverse perque al llegir del fitxer,el laberint surt de manera invertida
        (laberint (reverse (llegir nom-fitxer)))
        ; Inicialitzam les caselles jugador i sortida
        (casella-jugador (cercar-casella laberint #\e))
        (casella-sortida (cercar-casella laberint #\s))
        ; Inicialitzam el tamany de les caselles
        (tamany-casella (obtenir-tamany-casella amplada-finestra altura-finestra laberint)))
        ; Esborram el que hi havia a la finestra
        (cls)
        ; Dibuixam el laberint sencer
        (dibuixar-laberint laberint casella-jugador casella-sortida tamany-casella)
        ; Actualitzam recursivament el laberint en funció del patró d'esdeveniments
        (explora-intern nom-fitxer laberint nom-jugador casella-jugador casella-sortida tamany-casella 0)
    )     
)

; -----------------------------------------------------------------------------------------------
; Bucle principal d'exploració. Gestiona el moviment de l'usuari pel laberint i controla
; quan arriba a la meta o decideix sortir. Fa crides recursives a sí mateixa després de cada acció.
;
; @param nom-fitxer Nom del fitxer amb el laberint.
; @param laberint Llista de llistes que representa el laberint.
; @param nom-jugador Nom del jugador (Usuari).
; @param casella-jugador Posició de la casella actual del jugador.
; @param casella-sortida Posició de la casella de sortida.
; @param tamany-casella Mida de cada casella per a la visualització.
; @param passos Número de passos realitzats fins ara.
; @return no retorna res explícitament; controla la lògica d'exploració i la visualització.
; -----------------------------------------------------------------------------------------------
(defun explora-intern (nom-fitxer laberint nom-jugador casella-jugador casella-sortida tamany-casella npasses)
    (let ((moviment-actual (llegir-casella)))
        (cond
            ;CAS BASE -> EL JUGADOR HA ARRIBAT A LA META
            ((equal casella-jugador casella-sortida)
             ; Esborram laberint
             (cls)
             ; Notificam
             (format t "Has arribat a la meta!~%Numero de passes: ~A~%" npasses)
             ; Escribim les dades (Nom, npasses) al fitxer de puntuacions
             (escriure-dades nom-fitxer nom-jugador npasses)
             ; Mostram les puntuacions d'aquest mateix fitxer
             (llegir-dades nom-fitxer)
            )

            ;CAS BASE -> EL JUGADOR HA PRESSIONAT EL BOTÓ DE SORTIR
            ((eq moviment-actual 'surt)
             ; Esborram laberint
             (cls)
             ; Notificam
             (print "Has sortit de l'exploracio.")
            nil)

            ;CAS BASE -> EL JUGADOR ES MOU (A UNA POSICIÓ VÀLIDA)
            ((casella-valida-explora laberint moviment-actual casella-jugador)
                  ; Inicialitzam la nova casella al aque se mou
             (let ((next-casella (moure-jugador moviment-actual casella-jugador)))
                  ; Dibuixam la casella a la que s'ha mogut i repintam la casella d'on venia
                  (dibuixar-casella-actual laberint casella-jugador tamany-casella)
                  (dibuixar-casella-jugador laberint next-casella tamany-casella)
                  ; Cridam recursivament a la funció
                  (explora-intern nom-fitxer laberint nom-jugador next-casella casella-sortida tamany-casella (1+ npasses))
            ))

            ;CAS GENERAL -> QUALSEVOL ALTRE CONDICIÓ (ENS MANTENIM AL MATEIX LLOC)
            (t
             (explora-intern nom-fitxer laberint nom-jugador casella-jugador casella-sortida tamany-casella npasses)
            )
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que llegeix el nom del jugador passat per pantalla
;
; @param Sense paràmetres
; @return nom Representa el nom elegit pel jugador
; -----------------------------------------------------------------------------------------------
(defun llegir-nom ()
    (let ((nom (read-line)))
        (cond 
            ; CAS BASE -> NOM VÀLID (RETORNA NOM)
            ((and nom (not (equal nom ""))) nom)
            ; CAS RECURSIU -> NOM INVÀLID (CRIDAR FINS QUE EL NOM SIGUI VÀLID)
            (t 
              (print "Introdueix el teu nom: ")
              (llegir-nom))
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que llegeix el laberint d'un fitxer
;
; @param nom-fitxer fitxer a llegir
; @return laberint Llista de llistes de valors literals que representa el laberint
; -----------------------------------------------------------------------------------------------
(defun llegir (nom-fitxer)
    (print "llegir")
    (let* ((fp (open nom-fitxer :direction :input))
           (laberint (llegir-intern fp)))
          ;(print laberint)
          (close fp)
          laberint)
)

; FUNCIÓ INTERNA QUE TRANSFORMA LES LÍNIES LLEGIDES A LLISTES
(defun llegir-intern (fp)
    (let ((caracter (read-char fp nil 'eof)))
        (cond
            ;CAS BASE -> si el caracter és el final del fitxer ,retornam nil
            ((eq caracter 'eof) nil)
            ;CAS BASE -> si el caracter és el final de la linia, llegim la seguent linia
            ((eq caracter #\Newline)
             (cons nil (llegir-intern fp))
            )
            ;CAS GENERAL ->  si hi ha caracters, llegim la linia
            (t 
            (let ((linia (llegir-linia fp (list caracter))))
                (cons linia (llegir-intern fp)))
            )
        )
    )
)

;FUNCIÓ INTERNA QUE LLEGEIX TOTA UNA LÍNIA DEL LABERINT
(defun llegir-linia (fp l)
    (let ((caracter (read-char fp nil 'eof)))
        (cond
            ;CAS BASE -> final de fitxer/linia (retornam la llista al revés)
            ((or (eq caracter 'eof) (eq caracter #\Newline)) (reverse l))
            ;CAS GENERAL -> si encara hi ha caracters, cridam a la funció afegint el caracter nou a la llista
            (t (llegir-linia fp (cons caracter l)))))
)

; -----------------------------------------------------------------------------------------------
; Funció que cerca la posició d'una casella dins el laberint
;
; Amb les funcions internes cercar-fila i cercar-columna, recorrem tot el laberint comparant cada
; valor de cada casella fins trobar la casella que correspon al caracter i retornam la seva posició
;
; @param laberint Llista de llistes de valors literals que representa el laberint
; @param caracter Caracter literal que representa la casella a trobar
; @return posicio Lista amb la fila i columna del caràcte (NFila NColumna)
; -----------------------------------------------------------------------------------------------
(defun cercar-casella (laberint caracter)
    (cercar-fila laberint caracter 0)
)

; FUNCIÓ INTERNA QUE CERCA DINS D'UNA FILA EL CARACTER
(defun cercar-fila (laberint caracter nfila)
    (cond
        ;CAS BASE -> el laberint es buid (hem arribat al final)
        ((null laberint) nil)
        (t 
                ; inicialització de la posició del caràcter
            (let ((posicio (cercar-columna (car laberint) caracter nfila 0)))
                (print posicio)
                (cond
                    ; CAS BASE -> LA POSICIÓ NO ÉS NULL (HEM TROBAT EL CARACTER)
                    (posicio posicio) 
                    ;CAS GENERAL -> si la posició és buida, encara no la hem trobat i hem de provar amb la seguent fila
                    (t (cercar-fila (cdr laberint) caracter (+ nfila 1)))
                )
            )
        )
    )
)

; FUNCIÓ INTERNA QUE CERCA DINS D'UNA FILA EL CARACTER
(defun cercar-columna (factual caracter nfila ncolumna)
    (cond
        ;CAS BASE -> no hem trobat el caracter a la fila
        ((null factual) nil)     
        ;CAS BASE -> si el caracter actual és el caracter a trobar, retornam la seva posicio (en forma de llista)
        ((eq (car factual) caracter) 
         (list nfila ncolumna)
        )
        ;CAS GENERAL -> si el caracter actual no és el caracter a trobar, cridam recursivament amb el seguent
        (t (cercar-columna (cdr factual) caracter nfila (+ ncolumna 1)))
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que llegeix les possibles caselles en funció de les tecles
;
; Verifica quina tecla ha estat pitjada i retorna el moviment
;
; @param Sense
; @return Acció 
; -----------------------------------------------------------------------------------------------
(defun llegir-casella ()
    (let ((tecla (get-key)))
        (cond
            ; Tecles A a 🡠
            ((or (= tecla 65) (= tecla 97)  (= tecla 331)) 'mou-esquerra)
            ; Tecles D d 🡢
            ((or (= tecla 68) (= tecla 100) (= tecla 333)) 'mou-dreta)
            ; Tecles W w 🡡
            ((or (= tecla 87) (= tecla 119) (= tecla 328)) 'mou-amunt)
            ; Tecles S s 🡣
            ((or (= tecla 83) (= tecla 115) (= tecla 336)) 'mou-avall)
            ; Tecla ESC
            ((= tecla 27) 'surt)
            (t nil)
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que escriu el nom i el nombre de passes d'un jugador dins el fitxer de puntuacions
;
; Obre el fitxer si existeix (el crea si no) i afegeix el Nom i el Npasses del jugador com una
; llista
;
; @param nom-fitxer Fitxer del laberint explorat
; @param nom-jugador Nom del jugador que ha explorat
; @param npasses Nombre de passes emprades per arribar a la sortida
; @return Notifica si s'executa correctament, un missatge d'error en altre cas
; -----------------------------------------------------------------------------------------------
(defun escriure-dades (nom-fitxer nom-jugador npasses)
    (let*
        ; Cream el fitxer si no existeix
        ((fitxer (format nil "~A-puntuacio.txt" nom-fitxer))
         (fp (open fitxer :direction :output :if-exists :append :if-does-not-exist :create))
        )
        (cond 
            (fp 
                ; Escribim les dades com una llista
                (format fp "~S~%" (list nom-jugador npasses))
                (close fp)
                (print "Dades guardades correctament")
            )
            (t (print "Error al obrir el fitxer"))
        )
    )
)


; -----------------------------------------------------------------------------------------------
; Funció que llegeix totes les dades d'un fitxer de puntuacions
;
; Obre el fitxer i llegeix les 10 millors puntuacions
;
; @param nom-fitxer Fitxer del laberint explorat
; @return Dades dels 10 millors jugadors per pantalla
; -----------------------------------------------------------------------------------------------
(defun llegir-dades (nom-fitxer)
    (let*
        ; Obre fitxer
        ((fitxer (format nil "~A-puntuacio.txt" nom-fitxer))
         (dades (llegir-dades-intern fitxer))
        )
        ; Notifica
        (format t "-----------------------------------------------------~%")
        (format t "- 10 millors puntuacions del fitxer ~A: -~%" nom-fitxer)
        (format t "-----------------------------------------------------~%")
        (llegir-limit dades 10)

    )
)

; FUNCIÓ INTERNA QUE LLEGEIX TOT EL FITXER
(defun llegir-dades-intern (fitxer)
    (let ((fp (open fitxer :direction :input)))
         (cond
            (fp 
                (let ((puntuacions (llegir-files fp)))
                    (close fp)
                    (ordenar puntuacions)
                )
            )
            (t (print "Error al obrir el fitxer"))
        )
    )
)

; FUNCIÓ QUE LLEGEIX LES LÍNIES D'UN FITXER I LES CONVERTEIX A LLISTES
(defun llegir-files (fp)
    (let ((fila (read fp nil)))
        (cond
            ; Es converteixen les files a llistes
            (fila (cons fila (llegir-files fp)))
            (t nil)
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que ordena unes puntuacions donades
;
; Les ordena de menys a més passes
;
; @param puntuacions Fitxer del laberint explorat
; @return puntuacions ordenades
; -----------------------------------------------------------------------------------------------
(defun ordenar (puntuacions)
    (cond
        ((eq puntuacions nil) nil)
        (t (ordenar-intern (car puntuacions) (ordenar (cdr puntuacions))))
    )
)


; FUNCIÓ INTERNA DE ORDENAR QUE VERIFICA LA POSICIÓ DINS LA LLISTA l DE jugador
; jugador representa les dades d'un jugador "(nom-jugador npasses)""
(defun ordenar-intern (jugador l)
    (cond
        ; CAS BASE -> llista buida
        ((eq l nil) (list jugador))
        ; CAS BASE -> si el nombre de passes es menor a l'actual primer, el posam davant
        ((< (cadr jugador) (cadr (car l))) (cons jugador l))
        ; CAS GENERAL -> si no, comprovam amb el seguent fins trobar la seva posició dins la llista
        (t (cons (car l) (ordenar-intern jugador (cdr l))))
    )
)

; FUNCIÓ QUE LLEGEIX UN CONJUNT DE DADES FINS UN LIMIT PASSAT PER PARÀMETRE
(defun llegir-limit (dades limit)
    (cond
        ; CAS BASE -> llista buida o hem arribat al limit
        ((or (eq dades nil) (<= limit 0)) nil)
        ; CAS GENERAL -> encara hi han dades per llegir
        (t (let ((jugador (car dades)))
                (format t "~A - ~A passes. ~%" (car jugador) (cadr jugador))
                (llegir-limit (cdr dades) (- limit 1))    
            )
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció booleana que verifica si la casella a la qual s'ha de moure el jugador és vàlida o no
;
; AND de tots els moviments permesos pel jugador
;
; @param laberint Llista de llistes que representa el laberint
; @moviment-actual que realitza el jugador
; @casella-jugador casella a la que es troba ara
; @return puntuacions ordenades
; -----------------------------------------------------------------------------------------------
(defun casella-valida-explora (laberint moviment-actual casella-jugador)
    (let* ((fila (car casella-jugador))
           (columna (cadr casella-jugador))
           ; Casella nova en funció del moviment del jugador (actualització de la fila i columna)
           (next-casella
                (cond
                    ((eq moviment-actual 'mou-esquerra) (list fila (- columna 1)))
                    ((eq moviment-actual 'mou-dreta) (list fila (1+ columna)))
                    ((eq moviment-actual 'mou-amunt) (list (1+ fila) columna))
                    ((eq moviment-actual 'mou-avall) (list (- fila 1) columna))
                    (t moviment-actual)
                )
           ))
            ; ES VERIFICA QUE COMPLEIX AMB LES ESPECIFICACIONS
            (and
                ; POSICIO DINS LIMITS
                (>= (car next-casella) 0)
                (>= (cadr next-casella) 0)
                (< (car next-casella) (long laberint))
                (< (cadr next-casella) (long (car laberint)))
                ; ES MOVEIX A UNA CASELLA VÀLIDA (NO ÉS PARET)
                (not (eq (obtenir-casella laberint (car next-casella) (cadr next-casella)) #\#))
            )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que actualitza la posició del jugador
;
; En funció del moviment canvia els valors de la seva posició
;
; @moviment-actual que realitza el jugador
; @casella-jugador casella a la que es troba ara
; @return posicio nova (fila columna)
; -----------------------------------------------------------------------------------------------
(defun moure-jugador (moviment-actual casella-jugador)
    (let    
        ((fila (car casella-jugador))
         (columna (cadr casella-jugador)))
        ; Casella nova en funció del moviment del jugador (actualització de la fila i columna)
        (cond
            ((eq moviment-actual 'mou-esquerra) (list fila (- columna 1)))
            ((eq moviment-actual 'mou-dreta) (list fila (1+ columna)))
            ((eq moviment-actual 'mou-amunt) (list (1+ fila ) columna))
            ((eq moviment-actual 'mou-avall) (list (- fila 1) columna))
            (t moviment-actual)
        )
    )
)

; -------------------------------------------------------------
; - FUNCIONS DE DIBUIX
; -------------------------------------------------------------

; Per a les funcions de dibuix, el que farem serà dibuixar inicialment el laberint, i després anar-lo
; actualitzant en funció de l'exploració recursiva (moviments del jugador)

; -----------------------------------------------------------------------------------------------
; Funció que dibuixa el laberint sencer
;
; Dibuixarem recursivament cada fila i per fila dibuixarem recursivament cada casella
;
; @param laberint Llista de llistes que representa el laberint
; @casella-jugador casella a la que es troba ara (també casella-entrada)
; @casella-sortida casella de la sortida
; @tamany-casella tamany de cada una de les caselles
; @return Laberint dibuixar per pantalla
; -----------------------------------------------------------------------------------------------
(defun dibuixar-laberint (laberint casella-jugador casella-sortida tamany-casella)
    (dibuixar-files laberint casella-jugador casella-sortida tamany-casella 0)
)

; FUNCIÓ RECURSIVA INTERNA QUE DIBUIXA CADA UNA DE LES FILES DEL LABERINT
(defun dibuixar-files (laberint casella-jugador casella-sortida tamany-casella nfila)
    (cond
        ; CAS BASE -> NO HI HA MÉS FILES PER DIBUIXAR, FINALITZAM
        ((null laberint) nil)
        ; CAS GENERAL -> ENCARA HI HA FILES PER DIBUIXAR
        (t
            ; PER CADA FILA, DIBUIXAREM ELS SEUS ELEMENTS (COLUMNES)
            (dibuixar-columnes (car laberint) casella-jugador casella-sortida tamany-casella nfila 0)
            ; I CRIDAREM A LA FILA SEGUENT
            (dibuixar-files (cdr laberint) casella-jugador casella-sortida tamany-casella (1+ nfila))
        )
    )
)

; FUNCIÓ RECURSIVA INTERNA QUE DIBUIXA CADA UN DELS ELEMENTS DE LA FILA
(defun dibuixar-columnes (fila casella-jugador casella-sortida tamany-casella nfila ncolumna) 
    (cond
        ; CAS BASE -> NO HI HA MÉS ELEMENTS DINS LA FILA PER DIBUIXAR, FINALITZAM
        ((null fila) nil)

        ; CAS BASE -> L'ELEMENT ES LA CASELLA DEL JUGADOR
        ((eq (list nfila ncolumna) casella-jugador)
         ; Com que la casella jugador no té un literal associat, feim una crida especial pel seu cas amb 'jugador
         (dibuixar-casella (* ncolumna tamany-casella) (* nfila tamany-casella) 'jugador tamany-casella)
         ; CRIDAM RECURSIVAMENT AMB EL SEGUENT ELEMENT
         (dibuixar-columnes (cdr fila) casella-jugador casella-sortida tamany-casella nfila (1+ ncolumna)))

        (t 
            ; A la resta de casos, podem agafar directament el car per dibuixar el literal
            (dibuixar-casella (* ncolumna tamany-casella) (* nfila tamany-casella) (car fila) tamany-casella)
            ; CRIDAM RECURSIVAMENT AMB EL SEGUENT ELEMENT
            (dibuixar-columnes (cdr fila) casella-jugador casella-sortida tamany-casella nfila (1+ ncolumna))
        )
    )
)

; -----------------------------------------------------------------------------------------------
; Funció que dibuixa la casella antigua del jugador
;
; Crida a dibuixar casella amb la posició de casella-jugador
;
; @param laberint Llista de llistes que representa el laberint
; @casella-jugador casella a la que es troba ara (també casella-entrada)
; @tamany-casella tamany de cada una de les caselles
; @return Casella dibuixada per pantalla
; -----------------------------------------------------------------------------------------------
(defun dibuixar-casella-actual (laberint casella-jugador tamany-casella)
    (dibuixar-casella (* (cadr casella-jugador) tamany-casella) 
                      (* (car casella-jugador) tamany-casella) 
                      (obtenir-casella laberint (car casella-jugador) (cadr casella-jugador)) 
                      tamany-casella)
)

; -----------------------------------------------------------------------------------------------
; Funció que dibuixa la casella nova del jugador
;
; Crida a dibuixar casella amb la posició de casella-jugador
;
; @param laberint Llista de llistes que representa el laberint
; @casella-jugador casella a la que es troba ara (també casella-entrada)
; @tamany-casella tamany de cada una de les caselles
; @return Casella dibuixada per pantalla
; -----------------------------------------------------------------------------------------------
(defun dibuixar-casella-jugador (laberint casella-jugador tamany-casella)
    (dibuixar-casella (* (cadr casella-jugador) tamany-casella) 
                      (* (car casella-jugador) tamany-casella)
                      'jugador
                      tamany-casella)
)

; -----------------------------------------------------------------------------------------------
; Funció que dibuixa qualsevol casella
;
; Es dibuixa un quadrat i es defineix el seu color en funció del tipus
;
; @param x posició x del punter
; @param y posició y del punter
; @param tipus que representa el tipus de casella
; @param tamany de la casella
; @return Quadrat dibuixat per pantalla
; -----------------------------------------------------------------------------------------------
(defun dibuixar-casella (x y tipus tamany)
    (cond
        ; ENTRADA -> COLOR BLAU
        ((eq tipus #\e) (color 0 0 255))
        ; SORTIDA -> COLOR VERMELL
        ((eq tipus #\s) (color 255 0 0))
        ; JUGADOR -> COLOR VERD
        ((eq tipus 'jugador) (color 0 255 0))
        ; PARET -> COLOR NEGRE
        ((eq tipus #\#) (color 0 0 0))
        ; CAMI -> COLOR BLANC
        ((eq tipus #\.) (color 255 255 255))
    )
    (move x y)
    (quadrat tamany)
    (color 0 0 0)
)

; FUNCIÓ QUADRAT QUE PINTA UN QUADRAT DE MIDA m
(defun quadrat (m)
    (quadrat-intern m m)
)

(defun quadrat-intern (n m)
    (cond
        ((= n 0) nil)
        (t
            (drawrel m 0)
            (drawrel (- m) 1)
            (quadrat-intern (- n 1) m)
        )
    )
)

; FUNCIÓ QUE CALCULA EL TAMANY DE LES CASELLES EN FUNCIÓ DEL TAMANY DE LA FINESTRA I EL LABERINT
(defun obtenir-tamany-casella (amplada-finestra altura-finestra laberint)
    (let*
        ((nfiles (long laberint))
         (ncolumnes (long (car laberint))))
        ; CALCULAM EL TAMANY MÁXIM POSSIBLE DE LES CASELLES (PER NO SORTIR DE LA FINESTRA) I ENS QUEDAM AMB EL MENOR
        ; ARRODONIM PER AVALL PER TENIR UN NOMBRE SENCER DE PÍXELS
        (floor (min (/ amplada-finestra ncolumnes) (/ altura-finestra nfiles)))
    )
)


