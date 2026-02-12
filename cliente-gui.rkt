#lang racket/gui
(require racket/tcp)
(require racket/udp)

; --- Variables Globales ---
(define archivo-tcp #f)      ; Ruta del archivo de texto seleccionado
(define archivo-udp #f)      ; Ruta del archivo multimedia seleccionado
(define UDP-PORT 9090)
(define TCP-PORT 8080)
(define CHUNK-SIZE 60000)

; --- Lógica de Envío TCP (Texto) ---
(define (enviar-texto ip path)
  (define-values (base name dir) (split-path path))
  (send msg-status set-label "Estado: Conectando TCP...")
  
  ; Usamos un hilo para no congelar la GUI mientras envía
  (thread
   (lambda ()
     (with-handlers ([exn:fail:network? (lambda (e) (send msg-status set-label "Error: No se pudo conectar."))])
       (define-values (in out) (tcp-connect ip TCP-PORT))
       (send msg-status set-label "Estado: Enviando texto...")
       
       (call-with-input-file path
         (lambda (file-in)
           (copy-port file-in out)))
       
       (close-output-port out)
       (close-input-port in)
       (send msg-status set-label "Estado: ¡Texto enviado con éxito!")))))

; --- Lógica de Envío UDP (Multimedia) ---
(define (enviar-multimedia ip path)
  (send msg-status set-label "Estado: Iniciando protocolo UDP...")
  
  (thread
   (lambda ()
     (define socket (udp-open-socket))
     
     ; 1. PREPARAR Y ENVIAR EL NOMBRE DEL ARCHIVO (METADATA)
     (define nombre-archivo (path->string (file-name-from-path path)))
     ; Creamos un mensaje tipo "INIT:nombre.ext"
     (define header (string->bytes/utf-8 (string-append "INIT:" nombre-archivo)))
     
     (send msg-status set-label (format "Estado: Enviando cabecera de ~a..." nombre-archivo))
     (udp-send-to socket ip UDP-PORT header)
     
     ; IMPORTANTE: Dormimos el hilo un momento (50ms) para asegurar 
     ; que el servidor procese el nombre antes de recibir los datos pesados.
     (sleep 0.05) 
     
     ; 2. ENVIAR EL CONTENIDO DEL ARCHIVO
     (send msg-status set-label "Estado: Transmitiendo datos...")
     (call-with-input-file path
       (lambda (in)
         (let loop ()
           (define buffer (read-bytes CHUNK-SIZE in))
           (unless (eof-object? buffer)
             (udp-send-to socket ip UDP-PORT buffer)
             (sleep 0.002) ; Control de flujo para no saturar
             (loop))))
       #:mode 'binary)
     
     (udp-close socket)
     (send msg-status set-label "Estado: ¡Multimedia enviada!"))))


; --- GUI ---

; 1. Ventana Principal
(define frame (new frame% 
                   [label "Cliente Distribuido Racket"]
                   [width 400]
                   [height 350]))

; 2. Panel para la IP
(define panel-ip (new vertical-panel% [parent frame] [border 10] [alignment '(center top)]))
(define field-ip (new text-field% 
                      [parent panel-ip] 
                      [label "IP del Servidor:"] 
                      [init-value "127.0.0.1"]))

; 3. Panel TCP
(define group-tcp (new group-box-panel% [parent frame] [label "Parte 1: Archivos de Texto (TCP)"] [border 10]))
(define btn-sel-tcp (new button% 
                         [parent group-tcp] 
                         [label "Seleccionar Archivo de Texto..."]
                         [callback (lambda (b e)
                                     (define f (get-file "Selecciona texto"))
                                     (when f 
                                       (set! archivo-tcp f)
                                       (send lbl-file-tcp set-label (path->string (file-name-from-path f)))))]))

(define lbl-file-tcp (new message% [parent group-tcp] [label "Ningún archivo seleccionado"] [auto-resize #t]))

(define btn-send-tcp (new button% 
                          [parent group-tcp] 
                          [label "Enviar por TCP"]
                          [callback (lambda (b e)
                                      (if archivo-tcp
                                          (enviar-texto (send field-ip get-value) archivo-tcp)
                                          (message-box "Error" "Selecciona un archivo de texto primero." frame)))]))

; 4. Panel UDP
(define group-udp (new group-box-panel% [parent frame] [label "Parte 2: Multimedia (UDP)"] [border 10]))
(define btn-sel-udp (new button% 
                         [parent group-udp] 
                         [label "Seleccionar Imagen/Video..."]
                         [callback (lambda (b e)
                                     (define f (get-file "Selecciona multimedia"))
                                     (when f 
                                       (set! archivo-udp f)
                                       (send lbl-file-udp set-label (path->string (file-name-from-path f)))))]))

(define lbl-file-udp (new message% [parent group-udp] [label "Ningún archivo seleccionado"] [auto-resize #t]))

(define btn-send-udp (new button% 
                          [parent group-udp] 
                          [label "Enviar por UDP"]
                          [callback (lambda (b e)
                                      (if archivo-udp
                                          (enviar-multimedia (send field-ip get-value) archivo-udp)
                                          (message-box "Error" "Selecciona un archivo multimedia primero." frame)))]))

; 5. Barra de Estado Inferior
(define msg-status (new message% [parent frame] [label "Estado: Esperando acción..."] [auto-resize #t]))

; Mostrar la ventana
(send frame show #t)