#lang racket
; FUNCION BUENA

;SOLUCIÓN

; funciones para rodear con formato html para resaltado
(define (surround1 s1 c)
  (string-append "<" c ">" s1))

(define (surround2 s1 c)
  (string-append "<" c ">" s1 "</" c ">"))

(define (surround3 s1 c)
  (string-append s1 "</" c ">"))

; funcion que separa un string
(define (sep str)(regexp-split #rx"(?<=\\]|\\[|(\r\n)|[*+-/()<>{}=;: ])|(?=\\]|\\[|(\r\n)|[*+-/()<>{}=;: ])" str))

(define (surroundRegexp s1)
  (cond
    ;darles color con formato html
    [(regexp-match #rx"^[0-9]+$" s1) (set! s1 (surround2 s1 "num"))]
    [(regexp-match #rx"^\\.$" s1) (set! s1 (surround2 s1 "num"))]
    [(regexp-match #rx"^[><*=/+-]$" s1) (set! s1 (surround2 s1 "op"))]
    [(regexp-match #rx"^else$|^if$" s1) (set! s1 (surround2 s1 "cond"))]
    [(regexp-match #rx"^for$|^while$" s1) (set! s1 (surround2 s1 "loop"))]
    [(regexp-match #rx"\"" s1) (set! s1 (surround3 s1 "string"))]
    [(regexp-match #rx"'.*'" s1) (set! s1 (surround2 s1 "string"))]
    [(regexp-match #rx"^\\]$|^\\[$|^[(]$|^[)]$|^[{]$|^[}]$" s1) (set! s1 (surround2 s1 "par"))]
    [(regexp-match #rx"^[,]$" s1) (set! s1 (surround2 s1 "punct"))]
    ;formato html de espacios
    [(regexp-match #rx" " s1) (set! s1 "&nbsp ")]
    [(regexp-match #rx"(\r\n)" s1) (set! s1 "<br>")]
    )s1)

; funcion que separa listas en partes para el trabajo paralelo
(define (split lst n)
  (let ([len (length lst)])
    (if (= len 0)
        '()
        (let ([k (ceiling (/ len n))])
          (cons (take lst k) (split (drop lst k) (- n 1)))))))

; funcion principal que resalta
(define (resaltar2 x)
  (set! x (sep x))
  (set! x (map (lambda (lst) (surroundRegexp lst)) x))
  (set! x (string-join x ""))
  (set! x (string-append " <html> <head> <meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"estilo.css\"> </head> <body> <p id=\"texto\"> " x " </p> </body> </html>   <script>function resaltarTextoEntreComillas() { var elemento = document.getElementById(\"texto\"); var texto = elemento.innerHTML; var regex = /\\\"(.*?)\\\"|'(.*?)'/g; var nuevoTexto = texto.replace(regex, function(match, p1, p2) { var contenido = p1 ? \"\\\"\" + p1 + \"\\\"\" : \"'\" + p2 + \"'\"; return \"<span class='string'>\" + contenido + \"</span>\"; }); elemento.innerHTML = nuevoTexto; } window.onload = resaltarTextoEntreComillas; </script>"))
  x)

; funcion auxiliar para resaltar de manera parelela
(define (resaltarAuxP x)
  (set! x (map (lambda (lst) (surroundRegexp lst)) x)) x)

; funcion para resaltar de manera parelela (4 futures)
(define (resaltarP x)
  (set! x (sep x))
  (cond [(< (length x) 4) (set! x (resaltarAuxP x))]
        [(>= (length x) 4)
  (set! x (let ([sublists (split x 4)])
    (let ([f1 (future (lambda () (resaltarAuxP (first sublists))))]
          [f2 (future (lambda () (resaltarAuxP (second sublists))))]
          [f3 (future (lambda () (resaltarAuxP (third sublists))))]
          [f4 (future (lambda () (resaltarAuxP (fourth sublists))))]
          )
      (append (append (append (touch f1) (touch f2)) (touch f3)) (touch f4)))))])
  (set! x (string-join x ""))
  (set! x (string-append " <html> <head> <meta charset=\"utf-8\"> <link rel=\"stylesheet\" href=\"estilo.css\"> </head> <body> <p id=\"texto\"> " x " </p> </body> </html>   <script>function resaltarTextoEntreComillas() { var elemento = document.getElementById(\"texto\"); var texto = elemento.innerHTML; var regex = /\\\"(.*?)\\\"|'(.*?)'/g; var nuevoTexto = texto.replace(regex, function(match, p1, p2) { var contenido = p1 ? \"\\\"\" + p1 + \"\\\"\" : \"'\" + p2 + \"'\"; return \"<span class='string'>\" + contenido + \"</span>\"; }); elemento.innerHTML = nuevoTexto; } window.onload = resaltarTextoEntreComillas; </script>"))
  x)

;SOLUCIÓN
;RESULTADO FINAL

; escribir string en un archivo
(define (res ruta string)
  (call-with-output-file (string-append "html/" ruta ".html")
    (lambda (port)
      (display string port))
    #:exists 'replace))

; funcion para 1 solo archivo
(define (resultado x ruta)
  (res ruta (resaltar2 (file->string (string-append "lectura/" x)))) "Correcto")

; funcion para 1 archivo paralela (4 futures)
(define (resultadoP x ruta)
  (res ruta (resaltarP (file->string (string-append "lectura/" x)))) "Correcto")

; funcion para una lista de archivos
(define (funcion x y) (map resultado x y))

; funciones para una lista de archivos paralela (4 futures)
(define (funcionAuxP x y) (map resultadoP x y))
(define (funcionP x y)
  (cond
    [(> 4 (length x)) (funcionAuxP x y)]
    [(<= 4 (length x))
     (let ([subX (split x 4)] [subY (split y 4)])
       (let ([f1 (future (lambda () (funcionAuxP (first subX) (first subY))))]
             [f2 (future (lambda () (funcionAuxP (second subX) (second subY))))]
             [f3 (future (lambda () (funcionAuxP (third subX) (third subY))))]
             [f4 (future (lambda () (funcionAuxP (fourth subX) (fourth subY))))]
             )
         (append (append (append (touch f1) (touch f2)) (touch f3)) (touch f4))))]))

; CONSTRUCCIÓN DE LISTA

(define leer '("mix.txt" "prueba.txt"))

; funcion secuencial
(time (funcion leer leer))
; funcion paralela 4 hilos 
(time (funcionP leer leer))