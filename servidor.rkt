#lang racket

(require racket/tcp)
(require racket/udp)

(define TCP-PORT 8080)
(define UDP-PORT 9090)
(define UDP-BUFFER-SIZE 60000)

; ==========================================
; LOGICA TCP (Texto - Parte 1)
; ==========================================
(define (manejar-cliente-tcp in out)
  (displayln "[TCP] Cliente conectado. Recibiendo datos...")
  
  ; Generamos nombre único: recibido_texto_TIMESTAMP.txt
  (define nombre-archivo (format "recibido_texto_~a.txt" (current-seconds)))
  
  (with-output-to-file nombre-archivo
    (lambda ()
      (copy-port in (current-output-port)))
    #:exists 'replace)
  
  (displayln (format "[TCP] Archivo guardado correctamente: ~a" nombre-archivo))
  (close-input-port in)
  (close-output-port out))

(define (iniciar-tcp)
  (define listener (tcp-listen TCP-PORT 4 #t))
  (displayln (format ">>> Servidor TCP escuchando en puerto ~a (Modo Texto) <<<" TCP-PORT))
  (displayln "Esperando conexiones...")
  
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    ; Creamos un hilo para atender a este cliente sin bloquear a los demás
    (thread (lambda () (manejar-cliente-tcp in out)))
    (loop)))

; ==========================================
; LOGICA UDP (Multimedia)
; ==========================================
(define (iniciar-udp)
  (define socket (udp-open-socket))
  (udp-bind! socket #f UDP-PORT)
  
  (displayln (format ">>> Servidor UDP escuchando en puerto ~a <<<" UDP-PORT))
  (displayln "Esperando metadata del archivo...")
  
  (define buffer (make-bytes UDP-BUFFER-SIZE))
  (define output-port #f) ; Variable para guardar el puntero al archivo abierto
  
  (let loop ()
    (define-values (bytes-leidos host port) (udp-receive! socket buffer))
    
    ; Convertimos los primeros bytes a string para ver si es un comando
    (define mensaje (bytes->string/utf-8 (subbytes buffer 0 bytes-leidos) #\?))
    
    (cond
      ; CASO 1: Recibimos la cabecera con el nombre
      [(string-prefix? mensaje "INIT:")
       (define nombre-archivo (substring mensaje 5)) ; Quitamos "INIT:"
       (displayln (format "[UDP] Nueva transmisión detectada: ~a" nombre-archivo))
       
       ; Si ya había un archivo abierto, lo cerramos
       (when output-port (close-output-port output-port))
       
       ; Abrimos el nuevo archivo con el nombre correcto
       (set! output-port (open-output-file nombre-archivo #:exists 'replace #:mode 'binary))
       (displayln "[UDP] Archivo creado. Recibiendo datos...")]
      
      ; CASO 2: Son datos del archivo
      [else
       (if output-port
           (begin
             (write-bytes (subbytes buffer 0 bytes-leidos) output-port)
             (flush-output output-port))
           (displayln "[UDP Error] Recibidos datos sin cabecera INIT previa."))])
    
    (loop)))

; ==========================================
; MENU PRINCIPAL
; ==========================================
(define (main)
  (displayln "========================================")
  (displayln "   SERVIDOR DE PROCESOS DISTRIBUIDOS    ")
  (displayln "========================================")
  (displayln "Seleccione el modo de operación:")
  (displayln "1. TCP (Transferencia de Archivos de Texto)")
  (displayln "2. UDP (Transferencia Multimedia - Imagen/Video)")
  (displayln "3. Salir")
  (display "> Ingrese una opción (1 o 2): ")
  
  (define opcion (read))
  
  (cond
    [(equal? opcion 1) (iniciar-tcp)]
    [(equal? opcion 2) (iniciar-udp)]
    [(equal? opcion 3) (displayln "Saliendo...")]
    [else 
     (displayln "Opción no válida. Intente de nuevo.\n")
     (main)]))


(main)