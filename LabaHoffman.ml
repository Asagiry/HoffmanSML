datatype HuffmanTree =
    Leaf of char * int
  | Node of HuffmanTree * HuffmanTree * int;

(* Реализация сортировки *)
fun quickSort cmp [] = []
  | quickSort cmp (pivot::rest) =
      let
        val (left, right) = List.partition (fn x => cmp x pivot < 0) rest
      in
        quickSort cmp left @ [pivot] @ quickSort cmp right
      end;

(* Построение из строки списка char *)
fun buildFrequencyTableFromString text =
  buildFrequencyTable (String.explode text);

fun buildFrequencyTable text =
  let
    (* Вспомогательная функция для подсчета частоты символов *)
    fun count char [] = 1
      | count char ((ch, freq)::rest) =
          if ch = char then freq + 1 else count char rest

    (* Функция добавления или обновления частоты символа *)
    fun addChar char [] = [(char, 1)]
      | addChar char ((ch, freq)::rest) =
          if ch = char then (ch, freq + 1)::rest
          else (ch, freq)::addChar char rest

    (* Рекурсивный подсчет частот по тексту *)
    fun build [] freqTable = freqTable
      | build (char::rest) freqTable = build rest (addChar char freqTable)
  in
    build text []
  end;

(* Построение дерева Хаффмана *)
fun buildHuffmanTree freqTable =
  let
    (* Преобразуем таблицу частот в начальные узлы дерева *)
    val initialNodes = map (fn (ch, freq) => Leaf(ch, freq)) freqTable

    (* Функция для объединения двух узлов в новый узел *)
    fun mergeNodes (node1, node2) =
      let
        val combinedFreq = 
          (case node1 of Leaf(_, freq1) => freq1 | Node(_, _, freq1) => freq1) +
          (case node2 of Leaf(_, freq2) => freq2 | Node(_, _, freq2) => freq2)
      in
        Node(node1, node2, combinedFreq)
      end

    (* Построение дерева через последовательное объединение узлов *)
    fun buildTree [] = raise Fail "Tree cannot be empty"
      | buildTree [singleNode] = singleNode
      | buildTree nodes =
          let
            (* Сортируем узлы по частоте и объединяем два с минимальной частотой *)
            val sortedNodes = quickSort (fn a => fn b => 
              let
                val freqA = (case a of Leaf(_, freq) => freq | Node(_, _, freq) => freq)
                val freqB = (case b of Leaf(_, freq) => freq | Node(_, _, freq) => freq)
              in
                freqA - freqB
              end)
              nodes
          in
            case sortedNodes of
                node1::node2::rest => buildTree (mergeNodes(node1, node2)::rest)
          end
  in
    buildTree initialNodes
  end;

(* Построение таблицы кодирования *)
fun buildCodeTable tree =
  let
    fun traverse (Leaf(ch, _), code) = [(ch, code)]
      | traverse (Node(left, right, _), code) =
          traverse (left, code @ [false]) @ traverse (right, code @ [true])
  in
    traverse (tree, [])
  end;

(* Кодирование текста *)
fun encodeHuffman text codeTable =
  let
    (* Ищем код символа в таблице кодов *)
    fun charToCode ch =
      case List.find (fn (c, _) => c = ch) codeTable of
          SOME (_, code) => code
        | NONE => raise Fail ("Character " ^ Char.toString ch ^ " not found in code table")
  in
    List.concat (map charToCode text)
  end;

(* Декодирование текста *)
fun decodeHuffman encodedText tree =
  let
    fun traverse (Leaf(ch, _), [], decoded) = ch :: decoded
      | traverse (Leaf(ch, _), rest, decoded) = traverse (tree, rest, ch :: decoded)
      | traverse (Node(left, right, _), bit::rest, decoded) =
          traverse (if bit then right else left, rest, decoded)
      | traverse (_, [], _) = raise Fail "Encoded text ended unexpectedly"
  in
    List.rev (traverse (tree, encodedText, []))
  end;




val text = "aaabaaadd"; (* Текстовая строка *)

(* Генерация таблицы частот из строки *)
val freqTable = buildFrequencyTableFromString text;

(* Построение дерева Хаффмана *)
val huffmanTree = buildHuffmanTree freqTable;

(* Построение таблицы кодов *)
val codeTable = buildCodeTable huffmanTree;

(* Кодирование текста *)
val encodedText = encodeHuffman (String.explode text) codeTable;

(* Декодирование текста *)
val decodedText = String.implode (decodeHuffman encodedText huffmanTree);
