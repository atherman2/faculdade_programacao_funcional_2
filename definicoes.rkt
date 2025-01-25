#lang racket

(provide
    navegador
    ambiente
    mobilia
    puzzle
    novo-navegador-visitar
    retorna-ambiente-atual
    encontra-conexao
    encontra-mobilia
    encontra-puzzle
)

(struct navegador (ambientes ambiente-atual))
(struct ambiente (nome descricao mobilias conexoes))
(struct mobilia (nome descricao objetos puzzles))
(struct puzzle (nome descricao funcao))

(define (novo-navegador-visitar navegador0 nome-destino)
    (define conexao-encontrada
        (encontra-conexao
            (first
                (retorna-ambiente-atual navegador0)
            )
            nome-destino
        )
    )
    (cond
        [(empty? conexao-encontrada) empty]
        [else
            (struct-copy
                navegador
                navegador0
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
        (λ (conexao-da-lista) (equal? (string-upcase conexao-da-lista) nome-conexao))
        (ambiente-conexoes ambiente)
    )
)

(define (encontra-mobilia ambiente nome-mobilia)
    (filter
        (λ (mobilia-da-lista) (equal? nome-mobilia (string-upcase (mobilia-nome mobilia-da-lista))))
        (ambiente-mobilias)
    )
)

(define (encontra-puzzle mobilia nome-puzzle)
    (filter
        (λ (puzzle-da-lista) (equal? nome-puzzle (string-upcase (puzzle-nome puzzle-da-lista))))
        (mobilia-puzzles mobilia)
    )
)