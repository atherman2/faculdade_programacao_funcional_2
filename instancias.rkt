#lang racket

(require "definicoes.rkt" "principal.rkt")
(provide
    (all-defined-out)
)

(define (codigo-secreto inventario navegador mobilia)
  (pular-linhas)
  (display "O Livro está aberto e está escrito: ")(newline)(newline)
  (display "CHARADA:")(newline)(newline)
  (display "Códigos ocultos, desafios insistentes,")(newline)
  (display "Geram oportunidades. ")(newline)
  (display "Sistemas engenhosos, criam rumos,")(newline)
  (display "Enigmas tecem ofícios.")(newline)(newline)
  (display "Resp - Para responder")(newline) 
  (display "Voltar - Para sair do livro")(newline)                       
  (display "Dica - Recebe dica")(newline)
  (define escolha (string-upcase (read-line)))
  (cond
    [(equal? escolha "RESP")
        (Colocar-codigo inventario navegador mobilia)]
    [(equal? escolha "VOLTAR")
        (inspecionar-mobilia inventario navegador mobilia)
    ]
    [(equal? escolha "DICA") (dica inventario navegador mobilia)]
    [else
        (display "Você inseriu uma opção inválida!")(newline)
        (display "Pressione Enter para continuar:")(newline)
        (read-line)
        (codigo-secreto inventario navegador mobilia)
    ]))
    
(define (dica inventario navegador mobilia)
(pular-linhas)  
(display "Dica: Olhe para os começos!")(newline)(newline)
(display "Voltar - Volta para Charada")(newline)
(define escolha (string-upcase (read-line)))
(cond
    [(equal? escolha "VOLTAR")
        (codigo-secreto inventario navegador mobilia)
    ]
    [else (dica)]))
        
    
(define (Colocar-codigo inventario navegador mobilia)
  (pular-linhas)
  (display "Escreva aqui a resposta:")(newline)(newline)
  (define resposta (string-upcase (read-line)))
  (cond [(equal? resposta "CODIGOSECRETO")
            (display "Resposta Correta!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (inspecionar-mobilia inventario navegador mobilia)
        ]
        [(equal? resposta "CODIGO SECRETO")
            (display "Resposta Correta!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (inspecionar-mobilia inventario navegador mobilia)
        ]
        [else
            (display "Resposta Errada!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (Colocar-codigo inventario navegador mobilia)
        ]))
           
;;LORE DO MAPA:  Você está em um laboratório misterioso, cercado por computadores antigos, monitores piscando e pilhas de livros de programação espalhados pela mesa. O único som é o suave zumbido do sistema operacional sendo carregado em um dos computadores.
(define (trivia inventario navegador mobilia)
  (pular-linhas)
  (display "Na frente, há uma tela de terminal com uma mensagem:")(newline)
  (display "Bem-vindo ao Laboratório do Programador.")(newline)
  (display "Para escapar, você precisa provar suas habilidades em Rachet.")(newline)
  (display "Responda corretamente às perguntas de trivia sobre Rachet finalizar a sala do laboratório!")(newline)
  (display "Para passar você terá que acertar 3 questões de 5!")(newline)(newline)
  (display "A tela então apresenta a primeira pergunta:")(newline)
  (display "Em Rachet, qual comando é utilizado para definir uma função?")(newline)
  (display "A) (define (function))")(newline)
  (display "B) (function create)")(newline)
  (display "C) var")(newline)
  (display "D) def")(newline)
  (define resposta1 (string-upcase (read-line)))
  (newline)(display "Segunda pergunta:")(newline)
  (display "Em Rachet, há funções de alta ordem. Qual das opções não é uma delas?")(newline)
  (display "A) (map ")(newline)
  (display "B) (foldr ")(newline)
  (display "C) (filter ")(newline)
  (display "D) (cond ")(newline)
  (define resposta2 (string-upcase (read-line)))
  (newline)(display "Terceira Pergunta:")(newline)
  (display "Qual das opções abaixo é usada para definir uma variável em Racket?")(newline)
  (display "A) (define x 10)")(newline)
  (display "B) let x = 10")(newline)
  (display "C) var x = 10")(newline)
  (display "D) const x = 10")(newline)
  (define resposta3 (string-upcase (read-line)))
  (newline)(display "Quarta Pergunta:")(newline)
  (display "Qual das opções abaixo é usada para criar uma lista em Racket?")(newline)
  (display "A) {1, 2, 3}")(newline)
  (display "B) [1, 2, 3]")(newline)
  (display "C) (list 1 2 3)")(newline)
  (display "D) (array 1 2 3)")(newline)
  (define resposta4 (string-upcase (read-line)))
  (newline)(display "Quinta Pergunta:")(newline)
  (display "Pergunta: Qual das funções abaixo é usada para aplicar uma função a cada item de uma lista em Racket?")(newline)
  (display "A) cons")(newline)
  (display "B) map")(newline)
  (display "C) apply")(newline)
  (display "D) filter")(newline)
  (define resposta5 (string-upcase (read-line)))
  (newline)(display "Então o Resultado é: ")

  (cond [(> (soma (list(correto resposta1 "A") (correto resposta2 "D") (correto resposta3 "A") (correto resposta4 "C") (correto resposta5 "B"))) 2 )
      (display "Você passou no teste, parabéns!")(newline)
      (display "Pressione Enter para continuar:")(newline)
      (read-line)(newline)
      (define novo-inventario
        (struct-copy
            inv
            inventario
            [trivia (add1 (inv-trivia inventario))]
        )
      )
      (inspecionar-mobilia novo-inventario navegador mobilia)
      ]
      [else (display "Você é a vergonha da profissão, tente novamente")(newline)
      (display "Pressione Enter para continuar:")(newline)
      (read-line)(newline)
      (inspecionar-mobilia inventario navegador mobilia)
      ]))


(define (correto resposta gabarito)
     (cond
       [(equal? resposta gabarito) 1]
       [else 0]))
  
(define (soma resposta)
  (foldr + 0 resposta))

;LORE da sala: Você entra em uma sala vazia, com paredes brilhantes como telas de computador. Uma voz artificial ecoa
(define (anagrama inventario navegador mobilia)
(pular-linhas)  
(display "As engrenagens começam a girar....")(newline)
(display "Quando você olha para tela está escrito: 'TACKER'.")(newline)(newline)
(display "Resp - Responder o desafio")(newline)
(display "Voltar - Sai do desafio" )(newline)                       
(display "Dica - Recebe dica")(newline)
(define escolha (string-upcase (read-line)))
(cond
    [(equal? escolha "RESP")
        (Colocar-codigo2 inventario navegador mobilia)
    ]
    [(equal? escolha "VOLTAR")
        (inspecionar-mobilia inventario navegador mobilia)
    ]
    [(equal? escolha "DICA")
        (display "Dica: ANA + GRAMA")(newline)
        (display "Pressione Enter para continuar:")(newline)
        (read-line)
        (anagrama inventario navegador mobilia)
    ]
    [else 
        (display "Você inseriu uma opção inválida!")(newline)
        (anagrama inventario navegador mobilia)
    ]))

(define (Colocar-codigo2 inventario navegador mobilia)
  (pular-linhas)
  (display "-----Insira a Resposta.------")(newline)(newline)
  (define resposta (string-upcase (read-line)))
  (cond [(equal? resposta "RACKET")
            (display "Resposta Correta!")(newline)
            (display "Pressione Enter para continuar:")(newline)
            (read-line)
            (define novo-inventario
                (struct-copy
                    inv
                    inventario
                    [anagrama (add1 (inv-anagrama inventario))]
                )
            )
            (inspecionar-mobilia novo-inventario navegador mobilia)
        ]
        [else
          (display "Resposta Errada")(newline)
          (display "Pressione Enter para continuar:")(newline)
          (read-line)
          (anagrama inventario navegador mobilia)
        ]))

(define (pular-linhas) (newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline)(newline))

(define puzzle-exibir-codigo-secreto
    (puzzle
        "Charada no computador"
        "Interagir com o computador - há um texto na tela"
        codigo-secreto
    )
)

(define puzzle-inserir-codigo-secreto
    (puzzle
        "Inserir código"
        "Misteriosamente o computador pede para inserir um código"
        Colocar-codigo
    )
)

(define puzzle-poema
  (puzzle
      "Ler livro"
      ""
      codigo-secreto
    )
)



(define puzzle-trivia
    (puzzle
        "Trivia.exe"
        "No canto da tela há um executável com um nome auto-explicativo"
        trivia
    )
)

(define puzzle-anagrama
    (puzzle
        "Desafio da engrenagem"
        "Tente descobrir o desafio para provar seu conhecimento"
        anagrama
    )
)

(define maquina-engrenagem
  (mobilia
      "Maquina"
      "Uma maquina ligada a uma tela"
      empty
      (list puzzle-anagrama)
    )
)

(define computador
    (mobilia
        "Computador"
        "Um velho computador velho no canto da sala"
        empty
        (list puzzle-trivia)
    )
)

(define livro
  (mobilia
   "Livro Mágico"
   "Livro antigo que voa no meio da sala"
   empty
   (list puzzle-poema)
   )
)

(define sala-pilares
    (ambiente
        "Sala dos Pilares"
        "Uma sala ampla, iluminada por uma luz azul-neon que emana de pilares flutuantes. Cada pilar apresenta um símbolo diferente: uma engrenagem, um pergaminho, e um computador. "
        empty
        (list "Sala da Engrenagem" "Sala do Pergaminho" "Sala do Computador")
    )
)

(define sala-engrenagem
    (ambiente
        "Sala da Engrenagem"
        "A sala é repleta de engrenagens gigantescas que giram sem parar. No centro, há uma máquina com uma tela piscando."
        (list maquina-engrenagem)
        (list "Sala dos Pilares")
    )
)

(define sala-pergaminho
    (ambiente
        "Sala do Pergaminho"
        "Uma sala com paredes de pergaminho, onde palavras aparecem magicamente. No centro, há um livro aberto, exibindo um poema enigmático."
        (list livro)
        (list "Sala dos Pilares")
    )
)

(define sala-computador
    (ambiente
        "Sala do Computador"
        "Você está em uma sala, cercado por computadores antigos, monitores piscando e pilhas de livros de programação espalhados pela mesa."
        (list computador)
        (list "Sala dos Pilares" )
    )
)

(define sala-escura
    (ambiente
        "Sala Escura"
        "Uma sala em que poucas é possível enxergar poucas coisas devido à escuridão"
        empty
        (list "Sala do Baú" "Sala do Computador")
    )
)

(define navegador0
    (navegador
        (list
            sala-pilares
            sala-engrenagem
            sala-pergaminho
            sala-computador
            sala-escura
        )
        "Sala dos Pilares"
    )
)
(define inv0
    (inv
        0
        0
    )
)
