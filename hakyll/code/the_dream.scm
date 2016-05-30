(define riffs
  (make-hash-table 4))

(hashq-set!
  riffs 'I
  #{
    \mark \markup { I }
    f4 a c d | f d c a |
    f4 a c d | ees d c b |
    #})

(hashq-set!
  riffs 'IV
  #{
    \mark \markup { IV }
    bes4 d f g | bes g f d |
    #})

(hashq-set!
  riffs 'I'
  #{
    \mark \markup { "I'" }
    f,4 a c d | f d c a |
    #})

(hashq-set!
  riffs 'V
  #{
    \mark \markup { V }
    g a bes b | c bes a g |
    #})

(hashq-set!
  riffs 'turnaround
  #{
    f4 f aes aes | g g ges c |
    #})
