#lang racket

(require "definicoes.rkt")

(provide
    (all-defined-out)
)

(define (loop-principal inventario navegador)
    (exibir-ambiente-atual navegador)
    (define escolha (menu-acoes-loop-principal))
    (cond
        [(equal? escolha "INSP")
            (inspecionar inventario navegador)
        ]
        [(equal? escolha "USAR")]
        [(equal? escolha "VISI")
            (visitar inventario navegador)
        ]
        [(equal? escolha "SAIR")
            (confirmar-sair inventario navegador)
        ]
        [else (opcao-loop-principal-invalida inventario navegador)]
    )
)

(define (menu-acoes-loop-principal)
    (display "O que você desejá fazer?")(newline)(newline)
    (display "INSP - Inspecionar uma mobília")(newline)
    (display "USAR - Usar um item")(newline)   
    (display "VISI - Visitar uma sala")(newline)
    (display "SAIR - Encerrar o escape room")(newline)
    (string-upcase (read-line))
)

(define (confirmar-sair inventario navegador)
    (display "Você deseja realmente sair do escape room?")(newline)
    (display "Todo progresso será perdido")(newline)
    (display "S - Confirmar")(newline)
    (display "N - Cancelar")(newline)
    (define escolha (string-upcase (read-line)))
    (cond
        [(equal? escolha "N")
            (newline)(loop-principal inventario navegador)
        ]
    )
)

(define (opcao-loop-principal-invalida inventario navegador)
    (display "Você digitou uma opção inválida!")(newline)(newline)
    (loop-principal inventario navegador)
)

(define (visitar inventario navegador)
    (define escolha (menu-visitar navegador))
    (define novo-navegador (novo-navegador-visitar navegador escolha))
    (cond
        [(equal? escolha "VOLT") (loop-principal inventario navegador)]
        [(empty? novo-navegador)
            (display "Você digitou uma opção inválida!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (newline)
            (visitar inventario navegador)
        ]
        [else
            (loop-principal inventario novo-navegador)
        ]
    )
)

(define (menu-visitar navegador)
    (display "Qual sala você deseja visitar?")(newline)
    (exibir-ambiente-atual navegador)
    (display "Ou insira VOLT para voltar")(newline)(newline)
    (string-upcase (read-line))
)

(define (inspecionar inventario navegador)
    (define ambiente-atual (first (retorna-ambiente-atual navegador)))
    (define mobilia-selecionada (menu-inspecionar ambiente-atual))
    (define mobilia-encontrada (encontra-mobilia ambiente-atual mobilia-selecionada))
    (cond
        [(equal? mobilia-selecionada "VOLT") (loop-principal inventario navegador)]
        [(empty? mobilia-encontrada)
            (display "Você digitou uma opção inválida!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (newline)
            (inspecionar inventario navegador)
        ]
        [else (inspecionar-mobilia inventario navegador (first mobilia-encontrada))]
    )
)

(define (menu-inspecionar ambiente-atual)
    (display "Qual mobília você deseja inspecionar?")(newline)
    (map
        (λ (mobilia-da-lista)
            (displayln
                (string-append
                    (mobilia-nome mobilia-da-lista)
                    ": "
                    (mobilia-descricao mobilia-da-lista)
                )
            )
        )
        (ambiente-mobilias ambiente-atual)
    )
    (display "Ou insira VOLT para voltar")(newline)
    (string-upcase (read-line))
)

(define (inspecionar-mobilia inventario navegador mobilia)
    (cond
        [(and
            (> (inv-trivia inventario) 0)
            (> (inv-anagrama inventario) 0)
        )
            (newline)
            "Parabéns! Você escapou da realidade virtual travada!"
        ]
        [else
            (exibir_mobilia1 mobilia)
            (define escolha (string-upcase (menu-inspecionar-mobilia)))
            (cond
                [(equal? escolha "USAR")]
                [(equal? escolha "PEGA")]
                [(equal? escolha "ACAO")
                    (seleciona-puzzle inventario navegador mobilia)
                ]
                [(equal? escolha "SELE") (inspecionar inventario navegador)]
                [(equal? escolha "VOLT") (loop-principal inventario navegador)]
            )
        ]
    )
)

(define (menu-inspecionar-mobilia)
    (display "O que você desejá fazer?")(newline)(newline)
    (display "USAR - Usar um objeto")(newline)   
    (display "PEGA - Pegar os objetos da mobília")(newline)
    (display "ACAO - Fazer uma acao da mobília")(newline)
    (display "SELE - Voltar para a seleção de mobília")(newline)
    (display "VOLT - Sair da inspeção de mobílias")(newline)
    (string-upcase (read-line))
)

(define (pega-objetos inventario navegador mobilia0)
    (define objetos (mobilia-objetos mobilia0))
    (cond
        [(empty? objetos)
            (displayln "Não há itens a serem pegos na mobília!")
            (inspecionar-mobilia inventario navegador mobilia0)
        ]
        [else
            (define nova-mobilia
                (struct-copy
                    mobilia
                    mobilia0
                    [objetos empty]
                )
            )
            (define ambiente-atual (first (retorna-ambiente-atual navegador)))
            (define mobilias (ambiente-mobilias ambiente-atual))
            (define novo-mobilias
                (cons nova-mobilia
                    (remove-mobilia-da-lista
                        mobilias
                        (mobilia-nome mobilia0)
                    )
                )
            )
            (define novo-ambiente
                (struct-copy
                    ambiente
                    ambiente-atual
                    [mobilias novo-mobilias]
                )
            )
            empty
        ]
    )
)

(define (seleciona-puzzle inventario navegador mobilia)
    (define escolha (menu-seleciona-puzzle mobilia))
    (define puzzle-encontrado (encontra-puzzle mobilia escolha))
    (cond
        [(equal? escolha "VOLT") (inspecionar-mobilia inventario navegador mobilia)]
        [(empty? puzzle-encontrado)
            (display "Você digitou uma opção inválida!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (newline)
            (seleciona-puzzle inventario navegador mobilia)
        ]
        [else ((puzzle-funcao (first puzzle-encontrado)) inventario navegador mobilia)]
    )
)

(define (menu-seleciona-puzzle mobilia)
    (display "Qual ação você deseja realizar?")(newline)
    (map 
        (λ (puzzle-da-lista)
            (displayln
                (string-append
                    (puzzle-nome puzzle-da-lista)
                    ": "
                    (puzzle-descricao puzzle-da-lista)
                )
            )
        )
        (mobilia-puzzles mobilia)
    )
    (display "Ou insira VOLT para voltar para a mobilia")(newline)(newline)
    (string-upcase (read-line))
)

(define (exibir-ambiente ambiente)
    (display "Você está em ")(display (ambiente-nome ambiente))(newline)
    (display (ambiente-descricao ambiente))(newline)(newline)
    (display "Nesta sala se encontram as seguintes mobílias:")(newline)
    (map (λ (mobilia) (displayln (mobilia-nome mobilia))) (ambiente-mobilias ambiente))
    (newline)
    (display "É possível ir para as seguintes salas:")(newline)
    (map displayln (ambiente-conexoes ambiente))
    (newline)
)

(define (exibir-ambiente-atual navegador)
    (exibir-ambiente
        (first
            (retorna-ambiente-atual navegador)
        )
    )
)


(define (exibir_mobilia1 x) 
  (cond
    ;; Caso onde tem objetos mas não há puzzles
    [(and (not (empty? (mobilia-objetos x))) (empty? (mobilia-puzzles x)))      
     (display
      (string-append "Nome: " (mobilia-nome x) "\n"
                     "Descrição: " (mobilia-descricao x) "\n"
                     "Objetos na Mobilia: " 
                     (string-join (map objeto-nome (mobilia-objetos x)) ", ") "\n\n"))]

    ;; Caso onde tem puzzles mas não há objetos
    [(and (empty? (mobilia-objetos x)) (not (empty? (mobilia-puzzles x))))      
     (display
      (string-append "Nome: " (mobilia-nome x) "\n"
                     "Descrição: " (mobilia-descricao x) "\n"
                     "Ações: " 
                     (string-join (map puzzle-nome (mobilia-puzzles x)) ", ") "\n\n"))]

    ;; Caso onde não tem nem objetos nem puzzles
    [(and (empty? (mobilia-objetos x)) (empty? (mobilia-puzzles x)))
     (display
      (string-append "Nome: " (mobilia-nome x) "\n"
                     "Descrição: " (mobilia-descricao x) "\n\n"))]

    ;; Caso padrão, em que há objetos e puzzles
    [else
     (display
      (string-append "Nome: " (mobilia-nome x) "\n"
                     "Descrição: " (mobilia-descricao x) "\n"
                     "Objetos na Mobilia: " 
                     (string-join (map objeto-nome (mobilia-objetos x)) ", ") "\n"
                     "Ações: " 
                     (string-join (map puzzle-nome (mobilia-puzzles x)) ", ") "\n\n"))]))

