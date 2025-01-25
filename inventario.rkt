#lang racket



(struct objeto ( nome descricao utilizacao situacao-uso )#:transparent)

(define (pegar_objeto objeto inventario) 
  (cons objeto inventario))

(define (remove_objeto_inventario objeto inventario)
  (filter (λ (x) (not (string=? (objeto-nome objeto) (objeto-nome x)))) inventario))

(define (exibir_inventario inventario)
  (for-each
   (λ (x) (display (string-append "Nome: " (objeto-nome x) "\n" "Descrição: " (objeto-descricao x) "\n\n"))) inventario))
