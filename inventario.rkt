#lang racket



(struct item ( nome descricao utilizacao )#:transparent)

(define (pegar_item item inventario) 
  (cons item inventario))

(define (remove_objeto_inventario item inventario)
  (filter (λ (x) (not (string=? (item-nome item) (item-nome x)))) inventario))

(define (exibir-inventario inventario)
  (for-each
   (λ (x) (display (string-append "Nome: " (item-nome x) "\n" "Descrição: " (item-descricao x) "\n\n"))) inventario))

(define inventario
  (list
    (item "espada" "Uma espada brilhante" "cortar")
    (item "escudo" "Um escudo resistente" "proteger")
    (item "poção" "Uma poção de cura" "curar")))


(remove_objeto_inventario (item "espada" "Uma espada brilhante" "cortar")  inventario)

(pegar_item (item "Agua" "Uma poção de cura" "curar") inventario)

(exibir-inventario inventario)
