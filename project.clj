(defproject clojure-reversi "0.1.0-SNAPSHOT"
  :description "An implementation of Reversi (also known as Othello) written in Clojure"
  :url "https://github.com/mistercam/reversi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [quil "1.4.1"]]
  :main reversi.main)