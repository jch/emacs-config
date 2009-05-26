
;;keyolution.el 

;;Author:   Volker Franz <volker.fr...@tuebingen.mpg.de> 
;;Status:   alpha, only tested on XEmacs 
;;Created:  03/2001 
;;Revision: $Date: 2002/01/08 18:03:42 $ $Revision: 1.11 $ 

;;keyolution.el is free software; you can redistribute it and/or 
;;modify it under the terms of the GNU General Public License as 
;;published by the Free Software Foundation; either version 2, or (at 
;;your option) any later version. 
;; 
;;keyolution.el is distributed in the hope that it will be useful, but 
;;WITHOUT ANY WARRANTY; without even the implied warranty of 
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
;;General Public License for more details. 
;; 
;;You should have received a copy of the GNU General Public License 
;;along with your Emacs; see the file COPYING.  If not, write to the 
;;Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, 
;;MA 02111-1307, USA. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;Commentary: 

;;I want to know, which keybindings I use most often --- in order to 
;;"evolve" the keybindings to shorter & more efficient ones. The 
;;following code writes the interactive keypresses to the buffer 
;;*keyolution* --- and saves this buffer every 500 keypresses to a 
;;file with the name: 

;;~/.emacs-keyolution/emacs-keyolutionN.data 

;;(where N is a uniq integer). You can then evaluate these files with 
;;standard GNU text utilities (like sort, wc, uniq etc.). 

;;To try this code, do: 
;; - create a directory: ~/.emacs-keyolution 
;; - load the file keyolution.el in XEmcas 
;; - M-x turn-on-keyolution 

;;To use it permanently, write to your .emacs file: 
;;(load "keyolution") 
;;(turn-on-keyolution) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun turn-on-keyolution() 
  "Switches keyolution on" 
  (interactive) 
  (add-hook 'pre-command-hook 'keyolution-pre-command-get-command t)) 

(defun turn-off-keyolution() 
  "Switches keyolution off" 
  (interactive) 
  (remove-hook 'pre-command-hook 'keyolution-pre-command-get-command)) 

;;User variables: 
(defvar keyolution-buffer-name "*keyolution*" 
  "Buffer in which keyolution stores information about key presses") 

(defvar keyolution-ensure-privacy t 
  "Omits self-insert-commands from keyolution-buffer") 

(defvar keyolution-data-file-directory "~/.emacs-keyolution") 
(defvar keyolution-data-file-name      "emacs-keyolution") 
(defvar keyolution-data-file-suffix    ".data") 
(defvar keyolution-commands-per-data-file 500) 

;;Internal variables: 
(defvar keyolution-command-counter 0) 

;;Functions: 
(defun keyolution-pre-command-get-command () 
  (let ((this-command-major-mode major-mode));Remember major-mode 
    (save-excursion 
      (set-buffer (get-buffer-create keyolution-buffer-name)) 
      (goto-char (point-max)) 
      ;;Write to keyolution-buffer 
      (if (and keyolution-ensure-privacy 
               (eq this-command 'self-insert-command)) 
          (insert (format "%s\tself-insert-command\n" this-command-major-mode)) 
        (insert (format  "%s\t%s\t%s\n" 
                         this-command-major-mode 
                         (key-description (this-command-keys))   
                         this-command))) 
      ;;Maybe save keyolution-buffer: 
      (setq keyolution-command-counter (1+ keyolution-command-counter)) 
      (if (>= keyolution-command-counter keyolution-commands-per-data-file) 
          (let ((fn (keyolution-new-data-file-name 
                     keyolution-data-file-suffix))) 
            (write-file fn) 
            (kill-buffer (current-buffer)) 
            (setq keyolution-command-counter 0)))))) 

(defun keyolution-new-data-file-name (&optional filename-suffix) 
  ;;Code similar to  Kyle Jones' mail-reader VM, see: vm-misc.el 
  "Creates a unique name for a keyolution data file." 
  (let ((done nil) 
        (filename nil) 
        (tempfile-counter 0)) 
    (while (not done) 
      (setq filename (convert-standard-filename 
                      (expand-file-name (format "%s%d%s" 
                                                keyolution-data-file-name 
                                                tempfile-counter 
                                                (or filename-suffix "")) 
                                        keyolution-data-file-directory)) 
            tempfile-counter (1+ tempfile-counter) 
            done (not (file-exists-p filename)))) 
    filename )) 
;;end of keyolution.el