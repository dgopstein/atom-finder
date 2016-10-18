(ns atom-finder.core)

(import org.eclipse.cdt.core.parser.FileContent)

(defn file-content
  "read in source file to be analyzed"
  [filename]
  (FileContent/createForExternalFileLocation filename))

(defn -main
  [& args]
  (prn (file-content "/home/dgopstein/nyu/confusion/atoms/simplifications/1984-anonymous/nonconfusing.c")))
