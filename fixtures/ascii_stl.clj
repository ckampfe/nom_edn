(ns ckampfe.ascii-stl
  (:require [instaparse.core :as i])
  (:gen-class))

(def moon (slurp "./Moon.stl"))

(def ascii-grammar "
  S = Prefix Facets Suffix
  Prefix = <'solid'> <Space> #'[\\w]+' <(<Space> 'STL')?> <Space>
  Facets = Facet+
  Facet =
    FacetNormal <Space>
      OuterLoop <Space>
        Vertex <Space> Vertex <Space> Vertex <Space>
      EndLoop <Space>
    EndFacet <Space>

  FacetNormal = <'facet normal'> <Space> Float <Space> Float <Space> Float
  <OuterLoop> = <'outer loop'>
  <EndLoop> = <'endloop'>
  <EndFacet> = <'endfacet'>
  Vertex = <'vertex'> <Space> Float <Space> Float <Space> Float
  Float = #'-?[\\d]+(\\.?[\\d]+)?' | #'-?[\\d]+(\\.?[\\d]+)?e[+-]{1}\\d{3}'
  <Suffix> = <'endsolid'> <Space> <#'[\\w]+'> <(<Space> 'STL')?> <Space>
  <Space> = #'[\\s]+'
  ")

(defn parse-transform [s]
  (let [parse-tree (i/parse (i/parser ascii-grammar) s)
        transform-map {:Float (fn [s] (Float/parseFloat s))
                       :FacetNormal (fn [x y z] {:x x :y y :z z})
                       :Facet (fn [n v1 v2 v3]
                                {:normal n
                                 :v1 v1
                                 :v2 v2
                                 :v3 v3})
                       :Facets (fn [& f] (vec f))
                       :Prefix (fn [stl-name] stl-name)
                       :Vertex (fn [x y z] {:x x :y y :z z})
                       :S (fn [stl-name triangles] {:name stl-name
                                                    :triangles triangles})}]
    (i/transform transform-map parse-tree)))

(time (parse-transform moon))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
