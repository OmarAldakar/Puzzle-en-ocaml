#!/bin/sh

set -e

cd src/
ocamlc -c sat_solver.mli sat_solver.ml
ocamlc -c k_parmis_n.ml
ocamlc -c -open Graphics bsp.ml
ocamlc -c -open Graphics -open Bsp -open K_parmis_n bsp_unique.ml 
ocamlc -c -open Graphics -open Bsp_unique -open Sat_solver -open Bsp main.ml
ocamlc -open Graphics -open Bsp_unique -open Sat_solver -open Bsp -open Main -c menu.ml

ocamlc graphics.cma sat_solver.cmo k_parmis_n.cmo bsp.cmo bsp_unique.cmo main.cmo menu.cmo -o puzzle.byte

rm *.cm*
cd ../

./src/puzzle.byte
rm ./src/puzzle.byte
