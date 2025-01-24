#lang racket

(struct navegador (ambientes ambiente-atual)#:transparent)
(struct ambiente (nome extra conexoes)#:transparent)

(define (localizacao-inicial)
   (navegador
      (list
         (ambiente
            "Sala dos Pilares"
            "Pilares!"
            (list "Galeria" "A")
         )
         (ambiente
            "Galeria"
            "Fotos!"
            (list "Sala dos Pilares")
         )
         (ambiente
            "A"
            "A!"
            (list "Sala dos Pilares" "B" "C")
         )
         (ambiente
            "B"
            "B!"
            (list "A" "C")
         )
         (ambiente
            "C"
            "C!"
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
      (λ (ambiente-da-lista)
         (equal?
            (navegador-ambiente-atual localizacao)
            (ambiente-nome ambiente-da-lista)
         )
      )
      (navegador-ambientes localizacao)
   )
)

(define (encontra-conexao local nome-conexao)
   (filter
      (λ (conexao-da-lista) (equal? conexao-da-lista nome-conexao))
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
         "B"
         "C"
         "A"
         "Sala dos Pilares"
         "Galeria"
         "Sala dos Pilares"
      )
   )
)

(define (teste-principal localizacao lista-visitar)
   (cond
      [(and
          (not (empty? localizacao))
          (not (empty? lista-visitar))
       )
         (displayln
            (ambiente-extra
               (first
                  (retorna-ambiente-atual localizacao)
               )
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