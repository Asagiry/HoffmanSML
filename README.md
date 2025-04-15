# Huffman Coding Implementation in Standard ML

Реализация алгоритма Хаффмана для сжатия данных на языке Standard ML.

## 📚 Функциональность

### Основные компоненты
- **Построение частотной таблицы** (`buildFrequencyTable`)
- **Создание дерева Хаффмана** (`buildHuffmanTree`)
- **Генерация таблицы кодирования** (`buildCodeTable`)
- **Кодирование/декодирование строк** (`encodeHuffman`/`decodeHuffman`)

### Вспомогательные функции
- Быстрая сортировка (`quickSort`)
- Преобразование строки ↔ список символов (`String.explode`/`String.implode`)

## 🧠 Структура данных

```sml
datatype HuffmanTree =
    Leaf of char * int    (* Символ и его частота *)
  | Node of HuffmanTree * HuffmanTree * int  (* Левый потомок, правый потомок, суммарная частота *)
