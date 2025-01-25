#lang racket

(require "definicoes.rkt")
(require "principal.rkt")

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
        (λ (inventario navegador mobilia)
            (displayln "a1!")
            (displayln "Pressione Enter para continuar:")
            (read-line)
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
        "A"
        "ambiente A"
        (list m1)
        (list "B")
    )
)

(define B
    (ambiente
        "B"
        "ambiente B"
        (list m1)
        (list "A")
    )
)

(define navegador0
    (navegador
        (list
            A
            B
        )
        "A"
    )
)

(loop-principal empty navegador0)