(("IMP"
  (("IMP" "Meaning")
   hline
   ("[Space]" "Stack Manipulation")
   ("[Tab][Space]" "Arithmetic")
   ("[Tab][Tab]" "Heap access")
   ("[LF]" "Flow Control")
   ("[Tab][LF]" "I/O")))
 ("Stack Manipulation [SPC]"
  (("Command" "Parameters" "Meaning")
   hline
   ("[Space]" "Number" "Push the number onto the stack")
   ("[LF][Space]" "-" "Duplicate the top item on the stack")
   ("[LF][Tab]" "-" "Swap the top two items on the stack")
   ("[LF][LF]" "-" "Discard the top item on the stack")))
 ("Arithmetic [TAB][SPC]"
  (("Command" "Parameters" "Meaning")
   hline
   ("[Space][Space]" "-" "Addition")
   ("[Space][Tab]" "-" "Subtraction")
   ("[Space][LF]" "-" "Multiplication")
   ("[Tab][Space]" "-" "Integer Division")
   ("[Tab][Tab]" "-" "Modulo")))
 ("Heap Access [TAB][TAB]"
  (("Command" "Parameters" "Meaning")
   hline
   ("[Space]" "-" "Store")
   ("[Tab]" "-" "Retrieve")))
 ("Flow Control [LF]"
  (("Command" "Parameters" "Meaning")
   hline
   ("[Space][Space]" "Label" "Mark a location in the program")
   ("[Space][Tab]" "Label" "Call a subroutine")
   ("[Space][LF]" "Label" "Jump unconditionally to a label")
   ("[Tab][Space]" "Label" "Jump to a label if the top of the stack is zero")
   ("[Tab][Tab]" "Label" "Jump to a label if the top of the stack is negative")
   ("[Tab][LF]" "-" "End a subroutine and transfer control back to the caller")
   ("[LF][LF]" "-" "End the program")))
 ("I/O [TAB][LF]"
  (("Command" "Parameters" "Meaning")
   hline
   ("[Space][Space]" "-" "Output the character at the top of the stack")
   ("[Space][Tab]" "-" "Output the number at the top of the stack")
   ("[Tab][Space]" "-" "Read a character and place it in the location given by the top of the stack")
   ("[Tab][Tab]" "-" "Read a number and place it in the location given by the top of the stack")))
 ("Example"
  (("Example" "Operation")
   hline
   ("[Space][Space][Space][Tab][LF]" " Put a 1 on the stack")
   ("[LF][Space][Space][Space][Tab][Space][Space] [Space][Space][Tab][Tab][LF] " "Set a Label at this point")
   ("[Space][LF][Space]" "Duplicate the top stack item")
   ("[Tab][LF][Space][Tab]" "Output the current value")
   ("[Space][Space][Space][Tab][Space][Tab][Space][LF]" "Put 10 (newline) on the stack...")
   ("[Tab][LF][Space][Space]" "...and output the newline")
   ("[Space][Space][Space][Tab][LF]" "Put a 1 on the stack")
   ("[Tab][Space][Space][Space]" "Addition. This increments our current value.")
   ("[Space][LF][Space]" "Duplicate that value so we can test it")
   ("[Space][Space][Space][Tab][Space][Tab][Tab][LF]" "Push 11 onto the stack")
   ("[Tab][Space][Space][Tab]" "Subtraction. So if we've reached the end, we have a zero on the stack.")
   ("[LF][Tab][Space][Space][Tab][Space][Space] [Space][Tab][Space][Tab][LF]" "If we have a zero, jump to the end ")
   ("[LF][Space][LF][Space][Tab][Space] [Space][Space][Space][Tab][Tab][LF]" "Jump to the start")
   (" [LF][Space][Space][Space][Tab][Space] [Space][Space][Tab][Space][Tab][LF] " "Set the end label")
   ("[Space][LF][LF]" "Discard our accumulator, to be tidy")
   ("[LF][LF][LF]" "Finish"))))
