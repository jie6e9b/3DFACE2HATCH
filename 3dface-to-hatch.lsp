;; =========================================================
;; Скрипт для преобразования 3DFACE в полилинии с штриховками
;; Конвертирует поля армирования из Лира САПР в подложку для AutoCAD
;; Версия 3.0 - использует entmake для совместимости
;; =========================================================

;; Вспомогательная функция для извлечения X и Y из координаты
(defun get-xy (coord-code entdata coord-y-code / pt-data)
  (setq pt-data (cdr (assoc coord-code entdata)))
  (if (listp pt-data)
    ;; Если это список (X Y Z), берем первые два элемента
    (list (car pt-data) (cadr pt-data))
    ;; Если это число, берем X из одного кода и Y из другого
    (list pt-data (cdr (assoc coord-y-code entdata)))
  )
)

;; Вспомогательная функция для создания замкнутой полилинии
(defun make-lwpolyline (ptlist layer color / ent n)
  (setq ent (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    '(100 . "AcDbPolyline")
    (cons 90 (length ptlist))
    '(70 . 1) ;; Флаг замкнутости
  ))

  ;; Добавление слоя
  (if layer
    (setq ent (append ent (list (cons 8 layer))))
  )

  ;; Добавление цвета
  (if color
    (if (numberp color)
      (if (> color 256)
        (setq ent (append ent (list (cons 420 color))))
        (setq ent (append ent (list (cons 62 color))))
      )
    )
  )

  ;; Добавление вершин
  (setq n 0)
  (foreach pt ptlist
    (setq ent (append ent (list (cons 10 pt))))
    (setq n (1+ n))
  )

  (entmake ent)
  (entlast)
)

;; Вспомогательная функция для создания штриховки
(defun make-hatch (boundary layer color trans / ent boundarydata)
  ;; Получаем данные границы
  (setq boundarydata (entget boundary))

  ;; Создаем штриховку
  (setq ent (list
    '(0 . "HATCH")
    '(100 . "AcDbEntity")
    (cons 8 (if layer layer "0"))
    '(100 . "AcDbHatch")
    '(10 0.0 0.0 0.0)
    '(210 0.0 0.0 1.0)
    '(2 . "SOLID")
    '(70 . 1)
    '(71 . 0)
    '(91 . 1)
    '(92 . 7) ;; Тип границы: полилиния
    '(72 . 0)
    '(73 . 1)
    '(93 . 0)
    (cons 330 boundary)
    '(97 . 0)
    '(75 . 1)
    '(76 . 1)
    '(98 . 1)
    '(10 0.0 0.0 0.0)
  ))

  ;; Добавление цвета
  (if color
    (if (numberp color)
      (if (> color 256)
        (setq ent (append ent (list (cons 420 color))))
        (setq ent (append ent (list (cons 62 color))))
      )
    )
  )

  ;; Добавление прозрачности
  (if trans
    (setq ent (append ent (list (cons 440 (fix (* trans 655.36))))))
  )

  (entmake ent)
  (entlast)
)

;; Вспомогательная функция для установки прозрачности
(defun set-transparency (ent trans-val / entdata old-trans)
  (setq entdata (entget ent))
  (setq old-trans (assoc 440 entdata))
  (setq trans-code (cons 440 (fix (* trans-val 655.36))))
  (if old-trans
    (setq entdata (subst trans-code old-trans entdata))
    (setq entdata (append entdata (list trans-code)))
  )
  (entmod entdata)
)

;; =========================================================
;; Быстрая версия - штриховки с прозрачностью 70%
;; =========================================================

(defun C:3DFACE2HATCH (/ ss i ent entdata pt1 pt2 pt3 pt4 layer color
                         pline hatch ptlist oldecho)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\nПреобразование 3D-граней в штриховки...")

  ;; Выбор всех 3DFACE объектов
  (setq ss (ssget "X" '((0 . "3DFACE"))))

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))

        ;; Извлечение координат вершин (только X и Y)
        (setq pt1 (get-xy 10 entdata 20))
        (setq pt2 (get-xy 11 entdata 21))
        (setq pt3 (get-xy 12 entdata 22))
        (setq pt4 (get-xy 13 entdata 23))

        ;; Получение слоя и цвета
        (setq layer (cdr (assoc 8 entdata)))
        (setq color (cdr (assoc 420 entdata)))
        (if (not color) (setq color (cdr (assoc 62 entdata))))

        ;; Создание списка точек
        (setq ptlist (list pt1 pt2 pt3))
        (if (and pt4 (not (equal pt3 pt4 0.0001)))
          (setq ptlist (append ptlist (list pt4)))
        )

        ;; Создание полилинии
        (if (>= (length ptlist) 3)
          (progn
            (setq pline (make-lwpolyline ptlist
                                         (if layer (strcat layer "_contour") "0")
                                         color))

            ;; Создание штриховки через команду -BHATCH
            (if pline
              (progn
                (command "_.-BHATCH" "_P" "SOLID" "_S" pline "" "")
                (setq hatch (entlast))

                ;; Проверка что штриховка создана
                (if (and hatch (not (equal hatch pline)))
                  (progn
                    ;; Установка свойств штриховки через entmod
                    (setq hatchdata (entget hatch))

                    ;; Установка слоя
                    (if layer
                      (progn
                        (setq hatchdata (subst (cons 8 (strcat layer "_hatch"))
                                               (assoc 8 hatchdata)
                                               hatchdata))
                      )
                    )

                    ;; Установка цвета
                    (if color
                      (if (numberp color)
                        (if (> color 256)
                          (setq hatchdata (append hatchdata (list (cons 420 color))))
                          (setq hatchdata (append hatchdata (list (cons 62 color))))
                        )
                      )
                    )

                    ;; Установка прозрачности
                    (setq hatchdata (append hatchdata (list (cons 440 (fix (* 70 655.36))))))

                    ;; Применение изменений
                    (entmod hatchdata)
                  )
                )
              )
            )
          )
        )

        (setq i (1+ i))
        (if (= (rem i 100) 0)
          (princ (strcat "\rОбработано: " (itoa i) " из " (itoa (sslength ss))))
        )
      )
      (princ (strcat "\n\nГотово! Обработано объектов: " (itoa (sslength ss))))
      (princ "\nСозданы полилинии и штриховки на новых слоях.")
    )
    (princ "\nНе найдено объектов 3DFACE!")
  )

  (setvar "CMDECHO" oldecho)
  (princ)
)

;; =========================================================
;; Только контуры без штриховок
;; =========================================================

(defun C:3DFACE2PLINE (/ ss i ent entdata pt1 pt2 pt3 pt4 layer color
                         pline ptlist oldecho)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\nПреобразование 3D-граней в полилинии...")

  (setq ss (ssget "X" '((0 . "3DFACE"))))

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))

        (setq pt1 (get-xy 10 entdata 20))
        (setq pt2 (get-xy 11 entdata 21))
        (setq pt3 (get-xy 12 entdata 22))
        (setq pt4 (get-xy 13 entdata 23))

        (setq layer (cdr (assoc 8 entdata)))
        (setq color (cdr (assoc 420 entdata)))
        (if (not color) (setq color (cdr (assoc 62 entdata))))

        ;; Создание списка точек
        (setq ptlist (list pt1 pt2 pt3))
        (if (and pt4 (not (equal pt3 pt4 0.0001)))
          (setq ptlist (append ptlist (list pt4)))
        )

        ;; Создание полилинии
        (if (>= (length ptlist) 3)
          (setq pline (make-lwpolyline ptlist
                                       (if layer (strcat layer "_contour") "0")
                                       color))
        )

        (setq i (1+ i))
        (if (= (rem i 100) 0)
          (princ (strcat "\rОбработано: " (itoa i) " из " (itoa (sslength ss))))
        )
      )
      (princ (strcat "\n\nГотово! Создано полилиний: " (itoa (sslength ss))))
    )
    (princ "\nНе найдено объектов 3DFACE!")
  )

  (setvar "CMDECHO" oldecho)
  (princ)
)

;; =========================================================
;; Полная версия с опциями пользователя
;; =========================================================

(defun C:LIRA2HATCH (/ ss i ent entdata pt1 pt2 pt3 pt4 layer color
                      pline hatch option delete-orig trans-val
                      ptlist oldecho hatchdata)
  (setq oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\n=== Преобразование полей армирования Лира САПР ===")

  ;; Опции
  (initget "Да Нет")
  (setq delete-orig
    (getkword "\nУдалить оригинальные 3DFACE после конвертации? [Да/Нет] <Да>: "))
  (if (= delete-orig nil) (setq delete-orig "Да"))

  (initget "Контуры Штриховки")
  (setq option
    (getkword "\nСоздать [Контуры/Штриховки] <Штриховки>: "))
  (if (= option nil) (setq option "Штриховки"))

  (if (= option "Штриховки")
    (progn
      (setq trans-val (getint "\nУкажите прозрачность штриховки (0-90) <70>: "))
      (if (= trans-val nil) (setq trans-val 70))
    )
  )

  ;; Выбор всех 3DFACE
  (setq ss (ssget "X" '((0 . "3DFACE"))))

  (if ss
    (progn
      (princ (strcat "\nНайдено 3DFACE объектов: " (itoa (sslength ss))))
      (setq i 0)

      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))

        ;; Извлечение координат
        (setq pt1 (get-xy 10 entdata 20))
        (setq pt2 (get-xy 11 entdata 21))
        (setq pt3 (get-xy 12 entdata 22))
        (setq pt4 (get-xy 13 entdata 23))

        (setq layer (cdr (assoc 8 entdata)))
        (setq color (cdr (assoc 420 entdata)))
        (if (not color) (setq color (cdr (assoc 62 entdata))))

        ;; Создание списка точек
        (setq ptlist (list pt1 pt2 pt3))
        (if (and pt4 (not (equal pt3 pt4 0.0001)))
          (setq ptlist (append ptlist (list pt4)))
        )

        ;; Создание полилинии
        (if (>= (length ptlist) 3)
          (progn
            (setq pline (make-lwpolyline ptlist
                                         (if layer (strcat layer "_contour") "0")
                                         color))

            ;; Создание штриховки если выбрана опция
            (if (and (= option "Штриховки") pline)
              (progn
                (command "_.-BHATCH" "_P" "SOLID" "_S" pline "" "")
                (setq hatch (entlast))

                ;; Установка свойств штриховки
                (if (and hatch (not (equal hatch pline)))
                  (progn
                    ;; Установка свойств штриховки через entmod
                    (setq hatchdata (entget hatch))

                    ;; Установка слоя
                    (if layer
                      (progn
                        (setq hatchdata (subst (cons 8 (strcat layer "_hatch"))
                                               (assoc 8 hatchdata)
                                               hatchdata))
                      )
                    )

                    ;; Установка цвета
                    (if color
                      (if (numberp color)
                        (if (> color 256)
                          (setq hatchdata (append hatchdata (list (cons 420 color))))
                          (setq hatchdata (append hatchdata (list (cons 62 color))))
                        )
                      )
                    )

                    ;; Установка прозрачности
                    (setq hatchdata (append hatchdata (list (cons 440 (fix (* trans-val 655.36))))))

                    ;; Применение изменений
                    (entmod hatchdata)
                  )
                )
              )
            )
          )
        )

        (setq i (1+ i))
        (if (= (rem i 50) 0)
          (princ (strcat "\rОбработано: " (itoa i) " из " (itoa (sslength ss))))
        )
      )

      ;; Удаление оригинальных 3DFACE
      (if (= delete-orig "Да")
        (progn
          (princ "\nУдаление оригинальных 3DFACE...")
          (command "_.ERASE" ss "")
        )
      )

      (princ (strcat "\n\n=== Готово! ==="))
      (princ (strcat "\nОбработано объектов: " (itoa (sslength ss))))
      (if (= option "Штриховки")
        (princ "\nСозданы полилинии и штриховки.")
        (princ "\nСозданы полилинии.")
      )
    )
    (princ "\nОшибка: не найдено объектов 3DFACE!")
  )

  (setvar "CMDECHO" oldecho)
  (princ)
)

;; Загрузка
(princ "\n=== Команды загружены ===")
(princ "\nLIRA2HATCH   - Полная версия с опциями")
(princ "\n3DFACE2HATCH - Быстрая конвертация в штриховки (прозрачность 70%)")
(princ "\n3DFACE2PLINE - Только контуры без штриховок")
(princ "\n")
(princ)
