; -----------------------------------------------------------------------------------------------
; Pràctica Final - Llenguatges de Programació (LISP)
; Curs 2024-25
; Nom: Gaizka Medina Gordo
; Grup: PF3-26 (Grup 301)
; Professor: Antoni Oliver Tomàs
; Convocatòria: Extraordinària
; Data: 26/06/2025
; -----------------------------------------------------------------------------------------------
; ARXIU QUE REALITZA LA GENERACIÓ DEL LABERINT 
;
; En aquest arxiu trobam totes les funcions encarregades de generar el laberint dins un fitxer
; de text passat per paràmetre. També hem de passar el nombre de files i columnes
; -----------------------------------------------------------------------------------------------
; ÚS FUNCIÓ -> carregar l'arxiu a XLISP-PLUS
; cridar a (load"genera.lsp")
;
; Exemple d'Ús de funció:
;
; (genera "laberint.txt" 10 10)
;
;
; -----------------------------------------------------------------------------------------------
; Funció principal genera que genera el laberint i l'escriu a un fitxer de text
;
; Crida a la funció interna que genera el laberint i l'escriur al fitxer de text passat per paràmetre 
;
; @param nom-fitxer Fitxer on s'escriurà el laberint
; @para files nombre de files del laberint
; @param columnes nombre de columnes del laberint
; @return retorna el laberint
; --------------------------------------------------------------------------------------------------
(defun genera (nom-fitxer files columnes)
  (let ((laberint (genera-intern files columnes)))
    (escriure nom-fitxer laberint)
    laberint
  )
)

; --------------------------------------------------------------------------------------------------
; Funció interna que genera i retorna el laberint
;
; Va generant per passes el laberint cridant a diferents mètodes que modifiquen la llista de llistes
;
; @param files del laberint
; @param columnes del laberint
; @return retorna el laberint final
; --------------------------------------------------------------------------------------------------
(defun genera-intern (files columnes)
    (let* 
        ; Generam les files internes (sense les vores)
        ((files-intern (- files 2))
        (columnes-intern (- columnes 2))
        ; Generam aleatoriament l'entrada i sortida
        (entrada (genera-casella files-intern columnes-intern))
        (sortida (genera-casella files-intern columnes-intern))
        ; Generam la matriu de parets
        (laberint-intern (genera-matriu-parets files-intern columnes-intern))
        ; Afeigm l'entrada
        (laberint-entrada (insereix-caracter laberint-intern entrada #\e))
        ; Afeigm els camins amb l'algorisme dfs
        (laberint-camins (dfs laberint-entrada entrada files-intern columnes-intern))
        ; Afegim la sortida
        (laberint-sortida (insereix-caracter laberint-camins sortida #\s))
        ; Tornam a afegir l'entrada que s'havia "perdut" despré de generar el laberint
        (laberint-intern-final (insereix-caracter laberint-sortida entrada #\e))
        ; Finalment afegim les vores per tenir el laberint-final
        (laberint-final (afegir-vores laberint-intern-final)))
        laberint-final
    )
    
)

; --------------------------------------------------------------------------------------------------
; Funció que genera una matriu de caràcters paret de tamany files x columnes
;
; Crida a una funció interna que genera la matriu
;
; @param files del laberint
; @param columnes del laberint
; @return retorna la matriu de columnes
; --------------------------------------------------------------------------------------------------
(defun genera-matriu-parets (files columnes)
    (genera-matriu-intern 0 1 files columnes '() '())
)

; FUNCIÓ INTERNA QUE GENERA PER A CADA FILA EL NOMBRE DE COLUMNES DE CARACTERS
(defun genera-matriu-intern (numf numc files columnes fila-actual matriu)
    (cond
        ((and (= numf files) (= numc columnes)) 
            (cons fila-actual matriu))     
        ((= numf files)
            (genera-matriu-intern 0 (1+ numc) files columnes
                                                '()
                                                (cons fila-actual matriu)))
        (t
            (genera-matriu-intern (1+ numf) numc files columnes
                                                (cons #\# fila-actual)
                                                matriu)
        )
    )
)

; GENERA UNA CASELLA ALEATORIA
(defun genera-casella (fila columna)
    (list (random fila) (random columna))
)

; ESTABLEIX UN ELEMENT AL LABERINT A UNA POSICIO
(defun insereix-caracter (laberint pos-caracter caracter)
    (assigna-caracter-posicio laberint 
        (car pos-caracter) 
        (cadr pos-caracter) 
        caracter
    )
)

; --------------------------------------------------------------------------------------------------
; Funció que assigna un caracter a una posició del laberint
;
; Crida a una funcions internes per canviar de fila i columna 
;
; @param laberint Llista de llistes
; @param files del laberint
; @param columnes del laberint
; @return no retorna res, simplement assigna
; --------------------------------------------------------------------------------------------------
(defun assigna-caracter-posicio (laberint fila columna caracter)
    (canvi-fila laberint fila
     (canvi-columna (nth fila laberint) columna caracter))
)
(defun canvi-fila (laberint fila fila-actualitzada)
    (cond 
        ((= fila 0) (cons fila-actualitzada (cdr laberint)))
        (t (cons (car laberint)
                 (canvi-fila (cdr laberint) (1- fila) fila-actualitzada)))
    )
)
(defun canvi-columna (fila columna caracter)
    (cond
        ((= columna 0) (cons caracter (cdr fila)))
        (t (cons (car fila)
                 (canvi-columna (cdr fila) (1- columna) caracter)))
    )
)

; --------------------------------------------------------------------------------------------------
; Funció que genera l'algorisme dfs per a un laberint
;
; Aquesta assigna a l'entrada un cami i, per a cada vei d'aquesta casella, executa la funció interna
; de manera aleatoria
;
; @param laberint Llista de llistes
; @param casella-actual (en primer cas l'entrada)
; @param files del laberint
; @param columnes del laberint
; @return no retorna res, simplement assigna
; --------------------------------------------------------------------------------------------------
(defun dfs (laberint casella-actual files columnes)
    
    (let ((laberint-actual (assigna-caracter-posicio laberint (car casella-actual) (cadr casella-actual) #\.)))
        (let ((veins (mescla '((1 0) (0 1) (-1 0) (0 -1)))))
            (dfs-intern laberint-actual casella-actual veins files columnes))
    )
)

; --------------------------------------------------------------------------------------------------
; Funció interna recursiva per a l'algorisme dfs
;
; Per a una casella, visita un dels seus veins i comprova si el vei és valid per a convertir-se en la
; casella actual. Si ho és, cridarà a dfs per a aquest vei, si no, cridarà al següent. Si una casella
; no te veins válids finalitza.
;
; @param laberint Llista de llistes
; @param casella-actual 
; @param veins de la casella actual
; @param files del laberint
; @param columnes del laberint
; @return no retorna res, simplement assigna
; --------------------------------------------------------------------------------------------------
(defun dfs-intern (laberint casella-actual veins files columnes)
    (cond 
        ((null veins) laberint) ; tots veins visitats
        (t (let ((vei (car veins)))
            (let ((nova-casella (list (+ (car casella-actual) (car vei))
                                      (+ (cadr casella-actual) (cadr vei)))))
                (if 
                    ; CASELLA VALIDA -> L'ASSIGNAM COM ACTUAL
                    (casella-valida-genera laberint nova-casella files columnes)
                     (dfs-intern (dfs laberint nova-casella files columnes)
                                casella-actual
                                (cdr veins)
                                files columnes)
                    ; CASELLA NO VALIDA -> PROVAM AMB EL SEGUENT VEI
                    (dfs-intern laberint casella-actual (cdr veins) files columnes)
                )
            
            ))
        )
    )
)

; --------------------------------------------------------------------------------------------------
; Funció que comprova si una casella és vàlida
;
; És vàlida si encara és paret i de les seves caselles veïnes l'unic camí és l'actual, a part de que
; ha de trobar-se dins els límits
;
; @param laberint Llista de llistes
; @param casella a verificar 
; @param files del laberint
; @param columnes del laberint
; @return no retorna res, simplement assigna
; --------------------------------------------------------------------------------------------------
(defun casella-valida-genera (laberint casella files columnes)
    (and (casella-dins-limit casella files columnes) 
         (eq (obtenir-casella laberint (car casella) (cadr casella)) #\#)
         (= (num-veins laberint casella files columnes) 1)    
    )
)

; FUNCIÓ QUE OBTÉ EL VALOR DE LA CASELLA A LA POSICIÓ DESITJADA
(defun obtenir-casella (laberint fila columna)
    (let ((fila-desitjada (bucle-fins fila laberint)))
         (bucle-fins columna fila-desitjada)
    )
)
(defun bucle-fins (n l)
    (cond ((= n 0) (car l))
          (t (bucle-fins (- n 1) (cdr l)))
    )
)

; AGAFA ELS VEINS D'UNA CASELLA (CASELLES ADJACENTS)
(defun num-veins (laberint casella files columnes)
    (let ((veins (list (list (- (car casella) 1) (cadr casella))
                       (list (1+ (car casella)) (cadr casella))
                       (list (car casella) (- (cadr casella) 1))
                       (list (car casella) (1+ (cadr casella))))))
            (num-veins-intern laberint veins files columnes 0)
    )
)

(defun num-veins-intern (laberint veins files columnes num-camins)
    (cond
        ((null veins) num-camins)
        ((and (casella-dins-limit (car veins) files columnes)
              (eq (obtenir-casella laberint (car (car veins)) (cadr (car veins))) #\.))
         (num-veins-intern laberint (cdr veins) files columnes (1+ num-camins)))
        (t
         (num-veins-intern laberint (cdr veins) files columnes num-camins))
    )
)

; VERIFICA QUE UNA CASELLA NO SURT DELS LIMITS DEL LABERINT
(defun casella-dins-limit (casella files columnes)
    (and
        (>= (car casella) 0)
        (< (car casella) files)
        (>= (cadr casella) 0)
        (< (cadr casella) columnes)
    )
)
; MESCLA UNA LLISTA
(defun mescla (l)
    (cond
        ((null l) nil)
        (t (let ((i (random (long l))))
            (let ((element (obtenir-element l i))
                  (reste (elimina i l)))
                 (cons element (mescla reste)))))
    )
)
(defun long (l)
    (cond ((null l) 0)
          (t (+ 1 (long (cdr l))))
    )
)

; OBTÉ L'ELEMENT D'UNA LLISTA llista A LA POSICIÓ index
(defun obtenir-element (llista index)
    (cond ((null llista) nil)
        ((= index 0) (car llista))
        (t (obtenir-element (cdr llista) (- index 1)))
    )
)
; ELIMINA L'ELEMENT A LA POSICIÓ i D'UNA LLISTA l 
(defun elimina (i l)
    (cond 
        ((null l) nil)
        ((= i 0) (cdr l))
        (t (cons (car l) (elimina (- i 1) (cdr l))))
    )
)


; AFEGEIX UNA FILA AL PRINCIPI I AL FINAL I UN ELEMENT AL 
; PRINCIPI I AL FINAL DE CADA FILA
(defun afegir-vores (laberint)
    (let* ((amplada (+ 2 (long (car laberint))))
           (vora-fila (genera-fila amplada)))
     (cons vora-fila
        (append (mapcar (lambda (fila)
                            (cons #\# (append fila '(#\#))))
                             laberint)
                (list vora-fila))))
)

(defun genera-fila (n)
    (cond  
        ((= n 0) nil)
        (t (cons #\# (genera-fila (- n 1))))
    )
)


; --------------------------------------------------------------------------------------------------
; Funció que escriu el laberint dins el fitxer
;
; Escriu fila a fila i element a element cridant a funcions internes recursives
;
; @param nom-fitxer on s'escriurà
; @param laberint a escriure
; @return no retorna res, simplement assigna
; --------------------------------------------------------------------------------------------------
(defun escriure (nom-fitxer laberint)
    (let
        ; Si existeix la sobreescriu i sino la crea
        ((fp (open nom-fitxer :direction :output :if-exists :supersede :if-does-not-exist :create)))
        (cond
            (fp (escriure-intern laberint fp)
                (close fp))
            (t (format t "Error a obrir l'arxiu ~A~%" nom-fitxer))
        )
    )
)

(defun escriure-intern (laberint fp)
    (cond 
        ((null laberint) nil)
        (t (escriure-fila (car laberint) fp)
            (write-char #\Newline fp)
            (escriure-intern (cdr laberint) fp)
        )
    )
)

(defun escriure-fila (fila fp)
    (cond 
        ((null fila) nil)
        (t (write-char (car fila) fp)
            (escriure-fila (cdr fila) fp)
        )
    )
)
