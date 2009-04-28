;; buffi.el ---  BUildFile FInder, a smart compile wrapper to build multiple [java] projects

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: buffi.el
;; Created:  10 May 1999
;; Version: 0.2
;; Author: Raphael Pierquin <raphael.pierquin@agisphere.com>, Court Demas <court@acm.org>
;; URL: http://www.agisphere.com/~pierquin/emacs
;; Description: wraps 'compile' to build multiple [java] projects

;; Compatibility: tested on Emacs20.7

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; buffi allows you to easily work on multiple projects with different
;; buildfiles at the same time.  (Note : I call a 'buildfile' any file
;; that drive a compilation sequence, such as Makefiles, or ant's
;; build.xml files)  It's especially useful for Java projects where
;; you're usually working with source code in subdirectories with the
;; buildfiles somewhere up the directory tree.  The idea is that you
;; may have a dozen source files (including buildfiles) open from
;; different projects at the same time.  If I'm editing a file from
;; Project A and execute 'buffi-build' (bound to C-c c) then I
;; probably want the buildfile from that project to be executed.  The
;; buildfile is most likely either in the current directory or one of
;; its parents, and the code below will perform that search and
;; execute the "make" (or "ant") command in the appropriate directory.
;; The other case is when you execute 'buffi-build' and are NOT on a
;; regular source file (maybe you're in some random documentation
;; buffer or something).  In this case it will do the best it can and
;; execute the make on the first buildfile buffer it can find.

;; Here's what is does :
;; buffi-build():
;;  if current buffer is buildfile, build upon it
;;  if not,
;;  if current directory has a buildfile, build upon it
;;    if not recurse to parent
;;  if none of the parents has a buildfile,
;;     find a buildfile buffer and build upon it
;;  otherwise,
;;     report an error.

;;; Install :

;; put buffi.el in you load path, and add the following in your .emacs :

;; (require 'buffi)

;; Tested with GNU emacs 20.7

;;; History

;; 0.1 to 0.2

;; * buffi-build prompts before running compile, so user can set options and targets.
;;   It will lookup in compile-history for last command in the given buidfile directory.

;; * if buffi finds more than one buildfile in a directory, it will choose the first appearing
;;   in buffi-buildfile.

;; here's the UI :)
(defun buffi-build nil
  "runs compile with buildfile found by buffi-find-buildfile"
  (interactive)
  (progn
    (setq buff-buildfile nil)
    (setq buffi-buildfile (buffi-find-buildfile))
    (if buffi-buildfile
        (progn
          (if ; first see what we found is a buffer
              (bufferp buffi-buildfile)
              (progn
                (switch-to-buffer buffi-buildfile t)
                (setq buffi-buildfile (buffer-file-name buffi-buildfile))))
          ; prompt for a target
          (setq compile-command (read-from-minibuffer "Compile command: "
                                                      (buffi-get-commandline buffi-buildfile) nil nil
                                                      '(compile-history . 1)))
          (compile compile-command))
      (message '"buffi found nothing to build !"))))

(defun buffi-find-buildfile ()
  "find the buildfile"
  (interactive)
  (or
    ( ; first try current buffer
      buffi-buildbufferp   (current-buffer) )
    ( ; then try current and upper directories
      ; (current directory is where current buffer file is, if there is one.)
      if
         ( buffer-file-name (current-buffer))
         ( buffi-find-here  (file-name-directory (buffer-file-name (current-buffer)))))
    ( ; then try every buffer
      buffi-find-first-in-list 'buffi-buildbufferp  (buffer-list))))

;; internals
(defun buffi-find-here (path)
  "recursively search the buildfile"
  ( or
      ( ; first try every file in path
       ;buffi-find-first-in-list 'buffi-buildfilep  (directory-files path t))
       buffi-choose-best-buildfile (buffi-filter-list 'buffi-buildfilep
                                                      (directory-files path t)
                                                      nil)
                                   (mapcar 'car buffi-buildfiles))
      ( unless
        ; give up if we're in root directory
        (member path '("/" "//" "/../" ))
        ( ; else try in the parent directory
         buffi-find-here
         (expand-file-name (concat (file-name-as-directory path )
                                   "../"))))))

(defun buffi-find-first-in-list (afunction elements)
  "returns first element out of elements, for which function returns true"
  (cond
   ( ; terminal condition : return nil if there's no more element
    (not elements)
    nil )
   ( ; if running function on first element returns t , return the element too
    ( funcall afunction ( car elements ))
    ( car elements ))
   ( ; else deal with the rest of elements
    t
    ( buffi-find-first-in-list afunction (cdr elements)))))

(defun buffi-filter-list (afunction tofilter filtered)
  "returns all elements out of tofilter, for which function returns true"
  (cond
   ( ; terminal condition : return nil if there's no more element
    (not tofilter)
    filtered )
   ( ; if running function on first element returns t , include it in filtered and go on
    (funcall afunction (car tofilter))
    (buffi-filter-list afunction
                       (cdr tofilter)
                       (cons (car tofilter)
                             filtered)))
   ( ; else deal with the rest of elements
    t
    (buffi-filter-list afunction
                       (cdr tofilter)
                       filtered))))

(defun buffi-choose-best-buildfile (listtocheck orderedbuildfiles)
  "returns the member of listtocheck who's the first in orderedbuildfiles"
  (cond
   (
    (not orderedbuildfiles)
    nil)
   (
    (buffi-find-first-in-list (function (lambda (x) (equal (file-name-nondirectory x)
                                                           (car orderedbuildfiles))))
                             listtocheck)
    (car listtocheck))
   (t
    (buffi-choose-best-buildfile listtocheck (cdr orderedbuildfiles)))))

(defun buffi-buildbufferp (buffer)
  "returns buffer if buffer file is a buildfile"
  (if
      (and
        (buffer-file-name buffer )
        (buffi-buildfilep (buffer-file-name buffer )))
      buffer
    nil))

(defun buffi-buildfilep (filename)
  "returns filename if it's a buildfile"
  (if
      (member
       (file-name-nondirectory filename)
       (mapcar 'car buffi-buildfiles))
      filename
    nil))

;; one might need to tune this
(defun buffi-get-commandline (filename)
  "get the last command line in history for that buildfile, or marshall it"
  (or (buffi-find-first-in-list (function (lambda (x) (string-match (concat "^cd "
                                                                            (file-name-directory filename)
                                                                            " ; ")
                                                                    x)))
                                compile-history)
      (buffi-marshall-commandline filename)))

(defun buffi-marshall-commandline (filename)
  "marshall the command line"
  (concat
   "cd "
   (file-name-directory filename)
   " ; "
   ( buffi-get-command filename )
   " "
   (file-name-nondirectory filename)
   " "
   ( buffi-get-args    filename )))

(defun buffi-get-command (filename)
  "returns command line to build with buildfile filename"
  ( eval ( cadr (assoc (file-name-nondirectory filename) buffi-buildfiles ))))

(defun buffi-get-args (filename)
  "returns command line to build with buildfile filename"
  ( eval ( cadr (cdr (assoc (file-name-nondirectory filename) buffi-buildfiles )))))

;; Here you can add your buildfile here
;; This list is order sensitive : first entries are prefered
(setq buffi-buildfiles
      '(
        ;; ("buildfilename" "buildcommand" "buildparameter")
        ( "build.sh"    '"sh"                    '"-emacs" )
        ( "build.xml"   '"ant -emacs -buildfile" nil       )
        ( "GNUmakefile" '"make -w -f"            nil       )
        ( "makefile"    '"make -w -f"            nil       )
        ( "Makefile"    '"make -w -f"            nil       )))

(global-set-key "\C-cc" 'buffi-build)
(require 'compile)
(provide 'buffi)
