#!/usr/bin/env janet

# Copyright 2021--2022 John Gabriele.
# See LICENSE.txt file for license.

# This script downloads the pkgs.janet file from the
# Janet package repository (git repo), then downloads
# descriptions of all the packages listed therein,
# creating an html page containing all package names
# with their descriptions.
#
# Usage:
#
#     gen-janet-pkgs-page.janet > index.html

(import janet-html)

# For getting the 'declare-project out of a project.janet file.
# bakpakin wrote, then added this for the next Janet release.
# Need it here until the next Janet release.
(defn parse-all
  ``Parse a string and return all parsed values as a tuple. For complex
  parsing, such as for a repl with error handling, use the `parser` api.``
  [str]
  (let [p   (parser/new)
        ret @[]]
    (parser/consume p str)
    (parser/eof p)
    (while (parser/has-more p)
      (array/push ret (parser/produce p)))
    (if (= :error (parser/status p))
        (error (parser/error p))
        ret)))


(defn shell-out
  ``The `cmd` arg should be a list of strings, just like
  what you pass to `os/spawn`.``
  [cmd]
  (let [x (os/spawn cmd :p {:out :pipe})
        s (:read (x :out) :all)]
    (:wait x)
    s))

# For each project we'll need to get its project.janet file
# to extract name, author, description, url, and license.
#
# If it's at sourcehut, given a git repo url, get the raw
# project.janet like so:
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
  ``Returns the defproject struct for a given project name.
  Maintains a "./project-janet-files" directory where it caches
  downloaded "project.janet" files.``
  [name url]
  (def dirnm "project-janet-files")
  (when (not (os/stat dirnm))
    (eprint dirnm " directory not found. Making it...")
    (os/mkdir dirnm))
  (os/cd dirnm)
  (def proj-fnm (string name ".project.janet"))
  (when (not (os/stat proj-fnm))
    (eprint "Can't find " proj-fnm ". Fetching it...")
    (cond
      (string/has-prefix? "https://git.sr.ht/" url)
      (shell-out ["wget" (sourcehut-raw-project-file-url url)])

      (string/has-prefix? "https://github.com/" url)
      (shell-out ["wget" (github-raw-project-file-url url)])

      (string/has-prefix? "https://gitlab.com/" url)
      (shell-out ["wget" (gitlab-raw-project-file-url url)])

      (do (eprint name ". Not sourcehut, github, nor gitlab. Bailing out.")
          (os/exit 1)))

    (os/rename "project.janet" proj-fnm))

  (def prj-struct
    (struct ;(drop 1
                   (find |(match $ ['declare-project & rest] rest)
                         (parse-all (slurp proj-fnm))))))
  (os/cd "..")
  prj-struct)


(defn make-table-row
  ``Given a project-name/git-url pair, returns a data structure
  representing one html table row, suitable for use by `janet-html/html`.``
  [itm]
  #(pp itm) # debug
  (let [name    (string (get itm 0))
        git-url (get itm 1)
        proj    (get-and-parse-janet-project-file name git-url)]
    [:tr [:td [:a {:href (proj :url)} name]]
         [:td (proj :description)]]))


(def css-content ``
  h1, h2, th {
    font-family: sans-serif;
  }
  table, th, td {
    border: 1px solid #ccc;
    border-collapse: collapse;
    padding: 10px;
  }
  thead {
    background-color: #09a5b8;
    color: #fff;
  }
  tbody tr:nth-child(odd) {
    background-color: #fff;
  }
  tbody tr:nth-child(even) {
    background-color: #eee;
}
``)

#------------------------------------------------------------------
(defn main
  [&]
  (when (not (os/stat "pkgs.janet"))
    (shell-out
     ["wget"
      "https://raw.githubusercontent.com/janet-lang/pkgs/master/pkgs.janet"]))
  # Array of pairs of package name symbol and git repo url.
  (def pkgs (pairs (eval (get (parse (slurp "pkgs.janet"))
                              2))))
  (sort-by |(get $ 0) pkgs)
  #(pp pkgs) # debug
  (def out-html (janet-html/html
                 [:html
                    [:head
                       [:title "Janet Package Directory"]
                     [:style css-content]]
                  [:body
                     [:h1 "Janet Package Directory"]
                   [:table
                      [:thead [:tr [:th "Package"] [:th "Description"]]]
                      (map make-table-row pkgs)]]]))

  (print out-html))
