(defvar *global-env* (make-hash-table))

;; Declara una variable en el entorno dado.
(defun declare-var (name env)
  (setf (gethash name env) nil)
)

;; Declara una función en el entorno dado.
(defun declare-fun (name params body env)
  (setf (gethash name env)
        (lambda (&rest args)
          ;; Define una función lambda con los parámetros y el cuerpo.
          (let ((local-env (copy-hash-table env)))
            ;; Crea una copia del entorno para la ejecución de la función.
            (loop for param in params
                  for arg in args
                  do (setf (gethash param local-env) arg))
                  ;; Asigna los argumentos a los parámetros en el entorno local.
            (eval-expr body local-env))))
  ;; Evalúa el cuerpo de la función en el entorno local.
)

;; Asigna un record a una variable.
(defun assign-record (var fields env)
  (setf (gethash var env) (eval-record fields env)) 
  ;; Evalúa la definición del record y la almacena en la tabla hash del entorno.
)


;; Esta función ejecuta la igualdad de oz
;; Puede ser:
;;   - Asignacion (si la variable aun no tiene valor)
;;   - Igualdad (si la variable ya tiene valor)
;; Tiene en cuenta que el valor que se pasa como parametro puede ser una expresion de oz
;; por eso la evalua antes de asignar.
(defun equality (variable valor entorno)
  (let ((current-value (gethash variable entorno)))
    (if current-value
        (if (= current-value (eval-expr valor entorno))
            (nil)
            (error "Error: La variable ~a ya ha sido asignada y no puede ser reasignada." variable))
        (setf (gethash variable entorno) (eval-expr valor entorno)))
  )
)

;; Esta función busca el valor de una variable en el entorno.
(defun buscar-variable (entorno variable)
  (gethash variable entorno)
)


(defun get-var (name)
  ;; Obtiene el valor de una variable desde la tabla hash global.
  (gethash name *global-env*)
)
; la función gethash se usa para buscar el valor asociado con name en *global-env*. Si la variable existe, devuelve su valor
; de lo contrario, devuelve nil.

(defun set-var (name value)
  ;; Establece el valor de una variable en la tabla hash global.
  (equality name value *global-env*)
)


(defun copy-hash-table (table)
  ;; Crea una copia de una tabla hash.
  (let ((new-table (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               ;; Copia cada clave y valor de la tabla original a la nueva tabla.
               (setf (gethash key new-table) value))
             table)
    new-table))
