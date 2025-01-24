#lang racket

(struct navegador (ambientes ambiente-atual))
(struct ambiente (nome extra conexoes))

(define (localizacao-inicial)
   (navegador
      (list
         (ambiente
            "Sala dos Pilares"
            (λ () (display "Pilares!"))
            (list "Galeria" "A")
         )
         (ambiente
            "Galeria"
            (λ () (display "Fotos!"))
            (list "Sala Principal")
         )
         (ambiente
            "A"
            (λ () (display "A!"))
            (list "Sala dos Pilares" "B" "C")
         )
         (ambiente
            "B"
            (λ () (display "B!"))
            (list "A" "C")
         )
         (ambiente
            "C"
            (λ () (display "C!"))
            (list "A" "B")
         )
      )
      "Sala dos Pilares"
   )
)

(define (visitar localizacao nome-ambiente)
   (define conexao-encontrada
      (encontra-conexao
         (first
            (retorna-ambiente-atual localizacao)
         )
         nome-ambiente
      )
   )
   (cond
      [(empty? conexao-encontrada) empty]
      [else
         (struct-copy
            navegador
            localizacao
            [ambiente-atual nome-ambiente]
         )
      ]
   )
)

(define (retorna-ambiente-atual localizacao)
   (filter
      (λ (ambiente)
         (equal?
            (navegador-ambiente-atual localizacao)
            (ambiente-nome ambiente)
         )
      )
      (navegador-ambientes localizacao)
   )
)

(define (encontra-conexao local nome-conexao)
   (filter
      (curry equal? (ambiente-nome local))
      (ambiente-conexoes local)
   )
)

(define (teste-inicializador)
   (teste-principal
      (localizacao-inicial)
      (list
         "Galeria"
         "Sala dos Pilares"
         "A"
         "B"
         "C"
         "A"
         "C"
      )
   )
)

(define (teste-principal localizacao lista-visitar)
   (cond
      [(not (empty? localizacao))
         (ambiente-extra
            (first
               (retorna-ambiente-atual localizacao)
            )
         )
         (teste-principal
            (visitar localizacao (first lista-visitar))
            (rest lista-visitar)
         )
      ]
   )
)

(teste-inicializador)