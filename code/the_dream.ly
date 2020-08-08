\version "2.18.2"
\include "lilypond-book-preamble.ly"
\paper { oddFooterMarkup = ##f }

#(load "the_dream.scm")

\score {
  \relative c' {
    #(hashq-ref riffs 'I)
    \break
    #(hashq-ref riffs 'IV)
    #(hashq-ref riffs 'I')
    \break
    #(hashq-ref riffs 'V)
    #(hashq-ref riffs 'turnaround)
    \bar "|."
  }
  \layout { indent = 0 }
}

# This is hilariously inefficient :)
lilypond -v | head -n 1 | awk '{printf $3}'
