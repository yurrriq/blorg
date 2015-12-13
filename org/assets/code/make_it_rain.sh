#!/usr/bin/env bash
lilypond "the_dream.ly"
convert -density 200 "the_dream.pdf" -flatten "output.png"
