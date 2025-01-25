#lang racket

(require "definicoes.rkt")

(struct puzzle (nome descricao funcao))

(define (loop-principal inventario navegador)
    (exibir-ambiente-atual navegador)
    (define escolha (menu-acoes-loop-principal))
    (cond
        [(equal? escolha "INSP")]
        [(equal? escolha "USAR")]
        [(equal? escolha "VISI")]
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
            (display "Você digitou uma opção inválida!")(newline)(newline)
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
    (string-string-upcase (read-line))
)

(define (inspecionar inventario navegador)
    (define ambiente-atual (retorna-ambiente-atual navegador))
    (define mobilia-selecionada (menu-inspecionar ambiente-atual))
    (define mobilia-encontrada (encontra-mobilia ambiente-atual mobilia-selecionada))
    (cond
        [(equal? mobilia-selecionada "VOLT") (loop-principal inventario navegador)]
        [(empty? mobilia-encontrada)
            (display "Você digitou uma opção inválida!")(newline)(newline)
            (menu-inspecionar inventario navegador)
        ]
        [else (inspecionar-mobilia inventario navegador (first mobilia-encontrada))]
    )
)

(define (menu-inspecionar ambiente-atual)
    (display "Qual mobília você deseja inspecionar?")(newline)
    (exibir-ambiente ambiente-atual)
    (display "Ou insira VOLT para voltar")(newline)
    (string-upcase (read-line))
)

(define (inspecionar-mobilia inventario navegador mobilia)
    (exibir_mobilia mobilia)
    (define escolha (string-upcase (menu-inspecionar-mobilia)))
    (cond
        [(equal? escolha "USAR")]
        [(equal? escolha "PEGA")]
        [(equal? escolha "ACAO")]
        [(equal? escolha "SELE") (inspecionar inventario navegador)]
        [(equal? escolha "VOLT") (loop-principal inventario navegador)]
    )
)

(define (menu-inspecionar-mobilia)
    (display "O que você desejá fazer?")(newline)(newline)
    (display "USAR - Usar um item")(newline)   
    (display "PEGA - Pegar um item da mobília")(newline)
    (display "ACAO - Fazer uma acao da mobília")(newline)
    (display "SELE - Voltar para a seleção de mobília")(newline)
    (display "VOLT - Sair da inspeção de mobílias")(newline)
    (string-upcase (read-line))
)

(define (seleciona-puzzle inventario navegador mobilia)
    (define escolha (menu-seleciona-puzzle mobilia))
    (define puzzle-encontrado (encontra-puzzle mobilia escolha))
    (cond
        [(equal? escolha "VOLT") (inspecionar-mobilia inventario navegador mobilia)]
        [(empty? puzzle-encontrado)
            (display "Você digitou uma opção inválida!")(newline)(newline)
            (seleciona-puzzle inventario navegador mobilia)
        ]
        [else ((puzzle-funcao puzzle-encontrado) inventario navegador mobilia)]
    )
)

(define (menu-seleciona-puzzle mobilia)
    (display "Qual ação você deseja realizar?")(newline)
    (map exibir-puzzle (mobilia-puzzles mobilia))
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
    (map displayln ambiente-conexoes)
    (newline)
)

(define (exibir-ambiente-atual navegador)
    (exibir-ambiente
        (retorna-ambiente-atual navegador)
    )
)

(struct mobilia (nome descricao objetos puzzles))

(define (exibir_mobilia mobilia)
  (for-each
   (λ (x)
     (cond
       ;; Caso onde tem objetos mas não há puzzles
       [(and (not (empty? (mobilia-objetos x))) (empty? (mobilia-puzzles x)))      
        (display (string-append "Nome: " (mobilia-nome x) "\n"
                               "Descrição: " (mobilia-descricao x) "\n"
                               "Objetos na Mobilia: " (string-join (mobilia-objetos x) ", ") "\n\n"))]

       ;; Caso onde tem puzzles mas não há objetos
       [(and (empty? (mobilia-objetos x)) (not (empty? (mobilia-puzzles x))))      
        (display (string-append "Nome: " (mobilia-nome x) "\n"
                               "Descrição: " (mobilia-descricao x) "\n"
                               "Puzzle: " (string-join (mobilia-puzzles x) ", ") "\n\n"))]

       ;; Caso onde não tem nem objetos nem puzzles
       [(and (empty? (mobilia-objetos x)) (empty? (mobilia-puzzles x)))
        (display (string-append "Nome: " (mobilia-nome x) "\n"
                               "Descrição: " (mobilia-descricao x) "\n\n"))]

       ;; Caso padrão, em que há objetos e puzzles
       [else
        (display (string-append "Nome: " (mobilia-nome x) "\n"
                               "Descrição: " (mobilia-descricao x) "\n"
                               "Objetos na Mobilia: " (string-join (mobilia-objetos x) ", ") "\n"
                               "Puzzle: " (string-join (mobilia-puzzles x) ", ") "\n\n"))]))
   mobilia))
