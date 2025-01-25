#lang racket

(define (loop-principal inventario navegador)
    (exibir-ambiente-atual navegador)
    (define escolha (menu-acoes-loop-principal))
    (cond
        [(equal? escolha "INSP")]
        [(equal? escolha "USAR")]
        [(equal? escolha "VISI")]
        [(equal? escolha "SAIR")
            (menu-confirmar-sair inventario navegador)
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

(define (menu-confirmar-sair inventario navegador)
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

(define (menu-inspecionar inventario navegador)
    (define ambiente-atual (retorna-ambiente-atual navegador))
    (display "Qual mobília você deseja inspecionar?")(newline)
    (exibir-ambiente ambiente-atual)
    (display "Ou insira VOLT para voltar")(newline)
    (define mobilia-selecionada (string-upcase (read-line)))
    (define mobilia-encontrada (encontra-mobilia ambiente-atual mobilia-selecionada))
    (cond
        [(equal? mobilia-selecionada "VOLT") (loop-principal inventario navegador)]
        [(empty? mobilia-encontrada)
            (display "Você digitou uma opção inválida!")(newline)(newline)
            (menu-inspecionar inventario navegador)
        ]
        [else (inspecionar inventario navegador (first mobilia-encontrada))]
    )
)

(define (inspecionar inventario navegador mobilia))