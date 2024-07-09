(defun ejecutar-pruebas ()
  ;; Ejecuta una serie de pruebas sobre el intérprete OZ.

    ;; Prueba Imprime un string
    (format t "Test 1 - Imprime un string~%")
    (format t "~%")
    (ejecutar-oz '((Browse "Hola mundo")))
    (format-test)

    ;; Prueba asignación e impresión de variable global
    (format t "Test 2 - Variable global~%")
    (format t "~%")
    (ejecutar-oz '((declare X)
                   (X = 50)
                   (Browse X)))
    (format-test)  

  ;; Prueba asignación e impresión de muchas variable globales
  (format t "Test 3 - Multiples Variables globales~%")
  (format t "~%")
  (ejecutar-oz '((declare X M N)
                 (X = 50)
                 (M = 40)
                 (N = 30)
                 (Browse X)
                 (Browse M)
                 (Browse N)))
  (format-test)  

    ;; Prueba lista con una variable global
    (format t "Test 4 - Lista~%")
    (format t "~%")
    (ejecutar-oz '((declare Y)
                   (Y = '(1 2 3 4))
                   (Browse Y)))
    (format-test) ; Deja un espacio en blanco en la consola

    ;; Prueba variable local
    (format t "Test 5 - Variable local~%")
    (format t "~%")
    (ejecutar-oz '((local ((Z ))
                          (in
                            (Z = 34)
                            (Browse Z)
                          end))))
    (format-test)


   ;; Prueba Multiples Variables locales
   (format t "Test 6 - Multiples Variables locales~%")
   (format t "~%")
   (ejecutar-oz '((local ((Z W H))
                         (in
                           (Z = 30)
                           (W = 20)
                           (H = 10)
                           (Browse Z)
                           (Browse W)
                           (Browse H)
                         end))))
   (format-test)
    
  ;; Prueba variable local fuera de ámbito
    (format t "Test 7 - Variable local fuera de ámbito~%")
    (format t "~%")
   (ejecutar-oz '((local ((A))
                         (in
                           (A = 34)
                           (Browse A)
                         end))))
    
    (handler-case
        (ejecutar-oz '((Browse A)))
        ;; Intenta acceder a la variable local 'a' fuera de su ámbito
      (error (c)
        (format t "Error: variable local fuera de ámbito.~%")))
        ;; Maneja el error y emite un mensaje si se intenta acceder a la variable local fuera de su ámbito
    (format-test)

  ;; Prueba de records
  (format t "Test 8 - records~%")
  (format t "~%")
  (ejecutar-oz '( (Declare Alumno E)
                    (E = 25)
                    (Alumno = (record (nombre "Roberto")
                                      (app "Sanchez")
                                      (edad E)))
                          (Browse (field Alumno nombre))
                          (Browse (field Alumno app))
                         (Browse (field Alumno edad))
                        ))

  (format-test)
  ;; Prueba de operación Suma
  (format t "Test 9 - Operación de suma: 25~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 24 + 1)
                          (Browse A)
                        end))))
  (format-test)

  ;; Prueba de operación Resta
  (format t "Test 10 - Operación de resta: 33~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 34 - 1)
                          (Browse A)
                        end))))
  (format-test)

  ;; Prueba de operación Multiplicación
  (format t "Test 11 - Operación de multiplicación: 34~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 2 * 17)
                          (Browse A)
                        end))))
  (format-test)

  ;; Prueba de operación División
  (format t "Test 12 - Operación de división: 7~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 56 / 8)
                          (Browse A)
                        end))))
  (format-test)

  ;; Prueba de operación múltiple
  (format t "Test 13 - Operación de suma dos veces: 18~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 10 + 4 + 4)
                          (Browse A)
                        end))))
  (format-test)

  ;Prueba de operaciones combinadas: suma y multiplicación
  (format t "Test 14 - Operación de suma y multiplicacion: 22~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 10 + 4 * 3)
                          (Browse A)
                        end))))
  (format-test)

  ;Prueba de operaciones combinadas: resta y división
  (format t "Test 15 - Operación de resta y división: 20~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 40 - 80 / 4)
                          (Browse A)
                        end))))
  (format-test)

  ;Prueba de operaciones prioritarias: multiplicación y suma
  (format t "Test 16 - Operación de multiplicación y suma: 25~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 10 * 2 + 5)
                          (Browse A)
                        end))))
  (format-test)



  ;Prueba de operaciones cruzadas: resta y suma
  (format t "Test 17 - Operación de resta y suma: 4~%")
  (format t "~%")
  (ejecutar-oz '((local ((A))
                        (in
                          (A = 4 + 8 + 2 - 10 )
                          (Browse A)
                        end))))
  (format-test)
 

   ;; Prueba de variable global en asignación
    (format t "Test 18 - Variable Global como asignación~%")
    (format t "~%")
        (ejecutar-oz '((declare R T)
                       (R = 5)
                       (T = R)
                       (Browse T)
                       ))
    (format-test)

   ;; Prueba de variable local en asignación
    (format t "Test 19 - Variable local como asignación~%")
    (format t "~%")
  (ejecutar-oz '((local ((A B))
    (in
      (A = 4)
      (B = A)
      (Browse B)
    end))))
    (format-test)

  
  
  ;; Prueba de variable global de simple asignacion (inmutable)
  (format t "Test 20 - Variable Global simple asignacion(inmutable)~%")
  (format t "~%")
  (handler-case
      (ejecutar-oz '((declare R)
                     (R = 5)
                     (Browse "El valor inicial de la variable es: ")
                     (Browse R)
                     (R = 3)
                     ))
    (error (c)
      (format t "Error: Las variables son inmutables, no se puede cambiar el valor de R~%")))
  (format-test)

  ;; Prueba variable local de simple asignación
  (format t "Test 21 - Variable local simple asignacion(inmutable)~%")
  (format t "~%")
    (handler-case
        (ejecutar-oz '((local ((Z))
                              (in
                                (Z = 34)
                                (Browse "El valor inicial de la variable es: ")
                                (Browse Z)
                                (Z = 30)
                              end))))
      (error (c)
        (format t "Error: Las variables son inmutables, no se puede cambiar el valor de Z~%")))
    (format-test)
  
 )

(defun format-test ()
  (format t "~%")
  (format t "########################################")
  (format t "~%")
)