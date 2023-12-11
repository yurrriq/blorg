\version "2.24.1"
\paper { oddFooterMarkup = ##f }

#(load "the_dream.scm")

\score {
  \relative c, {
    \clef bass
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
