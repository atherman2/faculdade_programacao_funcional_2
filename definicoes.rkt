#lang racket

(provide
    (all-defined-out)
)

(struct navegador (ambientes ambiente-atual))
(struct ambiente (nome descricao mobilias conexoes))
(struct mobilia (nome descricao objetos puzzles))
(struct puzzle (nome descricao funcao))
(struct objeto ( nome descricao utilizacao situacao-uso )#:transparent)
(struct inv (trivia anagrama))

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
                (string-upcase (navegador-ambiente-atual navegador))
                (string-upcase (ambiente-nome ambiente-da-lista))
            )
        )
        (navegador-ambientes navegador)
    )
)

(define (encontra-ambiente navegador nome-ambiente)
    (filter
        (λ (ambiente-da-lista) (equal? (ambiente-nome ambiente-da-lista) nome-ambiente))
        (navegador-ambientes navegador)
    )
)

(define (remove-ambiente-da-lista ambientes nome-ambiente)
    (filter
        (λ (ambiente-da-lista) 
            (not
                (equal? nome-ambiente
                    (string-upcase (ambiente-nome ambiente-da-lista))
                )
            )
        )
        (ambientes)
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
        (λ (mobilia-da-lista)
            (equal? nome-mobilia 
                (string-upcase (mobilia-nome mobilia-da-lista))
            )
        )
        (ambiente-mobilias ambiente)
    )
)

(define (remove-mobilia ambiente nome-mobilia)
    (filter
        (λ (mobilia-da-lista) 
            (not
                (equal? nome-mobilia
                    (string-upcase (mobilia-nome mobilia-da-lista))
                )
            )
        )
        (ambiente-mobilias ambiente)
    )
)

(define (remove-mobilia-da-lista mobilias nome-mobilia)
    (filter
        (λ (mobilia-da-lista) 
            (not
                (equal? nome-mobilia
                    (string-upcase (mobilia-nome mobilia-da-lista))
                )
            )
        )
        (mobilias)
    )
)

(define (atualiza-mobilia navegador nova-mobilia nome-ambiente)
    (define ambiente-da-mobilia (first (encontra-ambiente navegador nome-ambiente)))
    (define novo-ambiente
        (struct-copy
            ambiente
            ambiente-da-mobilia
            [mobilias
                (cons nova-mobilia
                    (remove-mobilia-da-lista
                        (ambiente-mobilias ambiente-da-mobilia)
                        (mobilia-nome nova-mobilia)
                    )
                )
            ]
        )
    )
   (atualiza-ambiente navegador novo-ambiente)
)

(define (atualiza-ambiente navegador0 novo-ambiente)
     (struct-copy
         navegador
         navegador0
         [ambientes
             (cons novo-ambiente
                 (remove-ambiente-da-lista
                     (navegador-ambientes navegador0)
                     (ambiente-nome novo-ambiente)
                 )
             )
         ]
     )
)

(define (encontra-puzzle mobilia nome-puzzle)
    (filter
        (λ (puzzle-da-lista) (equal? nome-puzzle (string-upcase (puzzle-nome puzzle-da-lista))))
        (mobilia-puzzles mobilia)
    )
)