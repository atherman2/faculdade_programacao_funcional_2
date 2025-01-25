#lang racket

(require "definicoes.rkt")
(require "principal.rkt")

(struct navegador (ambientes ambiente-atual))
(struct ambiente (nome descricao mobilias conexoes))
(struct puzzle (nome descricao funcao))
(struct mobilia (nome descricao objetos puzzles))
(struct objeto ( nome descricao utilizacao situacao-uso )#:transparent)

(define o1
    (objeto
        "o1"
        "objeto o1"
        empty
        "A"
    )
)

(define p1
    (puzzle
        "a1"
        "ação a1"
        (λ (inventario navegador)
            (displayln "a1!")
            (loop-principal inventario navegador)
        )
    )
)

(define m1
    (mobilia
        "m1"
        "mobília m1"
        (list o1)
        (list p1)
    )
)





(define A
    (ambiente
    )
)

(define navegador0
    (navegador
        (list
        )
        "A"
    )
)