(editor:defcommand "XX" (p)
   "delete something. Invoke with Alt-X"      
   "Moves five characters forward."
   (editor::replace-regexp-command p "error([A-Z]*, " "error("))

(editor:defcommand "error2" (p)
   "delete something. Invoke with Alt-X"      
   "Moves five characters forward."
   (editor::replace-regexp-command p "error2([A-Z]*, " "error2("))

(editor:defcommand "error" (p)
   "delete something. Invoke with Alt-X"      
   "Moves five characters forward."
   (editor::replace-regexp-command p "error([A-Z]*, " "error("))

(editor:defcommand "checkargs" (p)
   "delete something. Invoke with Alt-X"      
   "Moves five characters forward."
   (editor::replace-regexp-command p "checkargs([A-Z]*, " "checkargs("))

(editor:defcommand "checkinteger" (p)
   "delete something. Invoke with Alt-X"      
   "Moves five characters forward."
   (editor::replace-regexp-command p "checkinteger([A-Z]*, " "checkinteger("))