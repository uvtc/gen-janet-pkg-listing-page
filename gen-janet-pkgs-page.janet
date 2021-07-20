#!/usr/bin/env janet

# Copyright 2021 John Gabriele.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

(import janet-html)

(defn shell-out
  [cmd]
  (let [x (os/spawn cmd :p {:out :pipe})
        s (:read (x :out) :all)]
    (:wait x)
    s))

# For each project we'll need to get its project.janet file
# to extract name, author, description, url, and license.
#
# If it's at sourcehut, given a git repo url get the raw project.janet:
#
#     https://git.sr.ht/~bakpakin/temple
#     https://git.sr.ht/~bakpakin/temple/blob/master/project.janet
#
# If it's a github project, then that's:
#
#     https://github.com/andrewchambers/janet-big.git
#     https://raw.githubusercontent.com/andrewchambers/janet-big/master/project.janet
#
# If it's at gitlab:
#
#     https://gitlab.com/louis.jackman/janet-hypertext.git
#     https://gitlab.com/louis.jackman/janet-hypertext/-/raw/master/project.janet

(defn sourcehut-raw-project-file-url
  [git-url]
  (string git-url "/blob/master/project.janet"))

(defn github-raw-project-file-url
  [git-url]
  (let [prefix "https://raw.githubusercontent.com/"
        suffix "/master/project.janet"
        mid (string/slice git-url 19 -5)]
    (string prefix mid suffix)))

(defn gitlab-raw-project-file-url
  [git-url]
  (let [suffix "/-/raw/master/project.janet"
        prefix (string/slice git-url 0 -5)]
    (string prefix suffix)))

(defn get-and-parse-janet-project-file
  "Returns the defproject struct."
  [name url]
  (def dirnm "project-janet-files") # where we'll cache these proj files
  (if-not (os/stat dirnm)
    (do
      (print "Making " dirnm "...")
      (os/mkdir dirnm)))
  (os/cd dirnm)
  (def proj-fnm (string name ".project.janet"))
  (when (not (os/stat proj-fnm))
    (print "Can't find " proj-fnm ". Getting it...")
    (cond
      (string/has-prefix? "https://git.sr.ht/" url)
      (shell-out ["wget" (sourcehut-raw-project-file-url url)])

      (string/has-prefix? "https://github.com/" url)
      (shell-out ["wget" (github-raw-project-file-url url)])
      
      (string/has-prefix? "https://gitlab.com/" url)
      (shell-out ["wget" (gitlab-raw-project-file-url url)])

      (do (print name ". Not sourcehut, github, nor gitlab. Bailing out.")
          (os/exit 1)))
      
      (os/rename "project.janet" proj-fnm))
  (def prj-struct (struct ;(drop 1 (parse (slurp proj-fnm)))))
  (os/cd "..")
  prj-struct)

(defn make-list-item
  `Returns a data structure suitable for use by janet-html/html.`
  [itm]
  #(pp itm) # debug
  (let [name    (string (get itm 0))
        git-url (get itm 1)
        proj    (get-and-parse-janet-project-file name git-url)]
    [:li [:a {:href (proj :url)} name] " - " (proj :description)]))

#------------------------------------------------------------------
(defn main
  [&]
  (if-not (os/stat "pkgs.janet")
    (shell-out
     ["wget" "https://raw.githubusercontent.com/janet-lang/pkgs/master/pkgs.janet"]))
  (def pkgs (pairs (eval (get (parse (slurp "pkgs.janet"))
                              2))))
  (sort-by |(get $ 0) pkgs)
  #(pp pkgs)
  (def out-html (janet-html/html
                 [:html [:head [:title "Janet Package Directory"]]
                  [:body
                     [:h1 "Janet Package Directory"]
                   [:ul
                      (map make-list-item pkgs)]]]))

  (print out-html))
