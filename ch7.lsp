;;; Streams
;; You can even create streams that read from or write to strings.
;; By default, input is read from the stream *standard-input*.
;; The default place to output is *standard-output*.

;; pathname is a portable way of specifying a file. pathname has six
;; components: host, device, directory, name, type, and version.
;; new tool: make-pathname
(setf path (make-pathname :name "data.txt"))

;; new tool: open(the basic function for opening a file), close
#| Args:
- direction - :input, :output, or :io
- if-exists - usually :supersede
|#
;; open, write and close example
(setf str (open path :direction :output
		     :if-exists :supersede))
;;=> #<OUTPUT BUFFERED FILE-STREAM CHARACTER #P"data.txt">
(format str "Something~%")
;;=> NIL
(close str)
;;=> T

;; open, read and close exmaple
(setf str (open path :direction :input))
;;=> #<INPUT BUFFERED FILE-STREAM CHARACTER #P"data.txt" @1>
(read-line str)
;;=> "Something", NIL
(close str)
;;=> T

;; new tool: with-open-file
(with-open-file (str path :direction :output
		     :if-exists :supersede)
  (format str "Something~%"))
;; The `with-open-file` macro puts the `close` within an
;; `unwind-protect`, so the file is guaranteed to get closed, even if
;; an error interrupts the evaluation of the body.

;;; Input

...
