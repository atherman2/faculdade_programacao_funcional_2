#lang racket

(struct navegador (ambientes ambiente-atual))
(struct ambiente (nome descricao mobilias conexoes))
(struct mobilia (nome descricao objetos puzzle))
(struct puzzle (nome descricao funcao))

(define (visitar navegador nome-destino)
    (define conexao-encontrada
        (encontra-conexao
            (first
                (retorna-ambiente-atual navegador)
            )
            nome-destino
        )
    )
    (cond
        [(empty? conexao-encontrada) empty]
        [else
            (struct-copy
                navegador
                navegador
                [ambiente-atual nome-destino]
            )
        ]
    )
)

(define (retorna-ambiente-atual navegador)
    (filter
        (λ (ambiente-da-lista)
            (equal?
                (navegador-ambiente-atual navegador)
                (ambiente-nome ambiente-da-lista)
            )
        )
        (navegador-ambientes navegador)
    )
)

(define (encontra-conexao ambiente nome-conexao)
    (filter
        (λ (conexao-da-lista) (equal? conexao-da-lista nome-conexao))
        (ambiente-conexoes ambiente)
    )
)