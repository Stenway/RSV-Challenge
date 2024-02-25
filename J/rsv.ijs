NB.*rsv s-- j script for encoding and decoding rsv files.
NB.
NB. verbatim: see:
NB.
NB.   https://github.com/Stenway/RSV-Specification
NB.   https://github.com/Stenway/RSV-Challenge
NB.   https://www.youtube.com/watch?v=tb_70o6ohMA
NB. 
NB. interface word(s):
NB. ------------------------------------------------------------------------------
NB.  read   - reads a file as a list of bytes
NB.  rsvdec - decode rsv bytes - marks nulls with (NULLMARK)
NB.  rsvenc - encode rsv bytes - marks nulls with (NULLMARK)
NB.  rsvok  - 1 if blblcl rsv nouns have no bad bytes - 0 otherwise
NB.  write  - writes a list of bytes to file
NB.                                                                                                                 
NB. created: 2024jan08
NB. changes: ---------------------------------------------------------------------

coclass 'rsv'
NB.*end-header

NB. interface words (IFACEWORDSrsv) group
IFACEWORDSrsv=:<;._1 ' read rsvdec rsvenc rsvok write'

NB. string used to mark RSV nulls
NULLMARK=:'null'

NB. row terminator byte - hex: FD
REOR=:253{a.

NB. value terminator byte - hex: FF
REOV=:255{a.

NB. null value byte - hex: FE
RNULL=:254{a.

NB. root words (ROOTWORDSrsv) group      
ROOTWORDSrsv=:<;._1 ' IFACEWORDSrsv ROOTWORDSrsv VMDrsv read rsvdec rsvenc rsvok write'

NB. bytes that should never be emitted by UTF8 encoders
UTF8BADBYTES=:_8{.a.

NB. version, make count, and date
VMDrsv=:'0.9.0';7;'25 Feb 2024 11:39:28'

NB. signal with optional message
assert=:0 0"_ $ 13!:8^:((0: e. ])`(12"_))

NB. tests for character data
ischar=:2&=@(3!:0)

NB. reads a file as a list of bytes
read=:1!:1&(]`<@.(32&>@(3!:0)))


rsvdec=:3 : 0

NB.*rsvdec v-- decode rsv bytes - marks nulls with (NULLMARK).
NB.
NB. monad: blblcl =. rsvdec clRsv
NB.
NB.   rsv=. read jpath '~RSVTEST/Valid_001.rsv'
NB.   rsvdec rsv

]`(NULLMARK"_)@.((,RNULL)&-:)L:0 <;._2&.> <;._2 y
)


rsvenc=:3 : 0

NB.*rsvenc v-- encode rsv bytes - marks nulls with (NULLMARK).
NB.
NB. monad: clRsv =. rsvenc blblclRsv
NB.
NB.   rsv=. rsvdec read jpath '~RSVTEST/Valid_001.rsv'
NB.   rsvenc rsv

(0=#y) }. ; ,&REOR&.> ;&.> REOV -.&.>~ ,&REOV L: 0 (]`(RNULL"_))@.(NULLMARK&-:) L: 0 y
)


rsvok=:3 : 0

NB.*rsvok v-- 1 if blblcl rsv nouns have no bad bytes - 0 otherwise.
NB.
NB. monad: pa =. rsvok blblclRsv
NB.
NB.   NB. check blblcl for bad bytes before encoding
NB.   lol=: (<"1 <"0 ?2 2$1000), (<'') , <"0 ;:'some words'
NB.   lol=: utf8@": L: 0 ,&.> lol
NB.
NB.   NB. no bad bytes in utf8 formatted cells - result 1
NB.   rsvok lol
NB.
NB.   NB. add an RSV delimiter byte - result 0
NB.   rsvok lol,<,<RSVEOR

NB. sublists must be list of lists of char
msg=. 'not a list of lists of characters'
msg assert 1 = #@$y
msg assert *./ ischar&> ;y
msg assert -.0 e. #@$ &> y

NB. without bad bytes
-. +./ ;; L: 1 ] 1&e.@(UTF8BADBYTES&e.) L: 0 y
)

NB. writes a list of bytes to file
write=:1!:2 ]`<@.(32&>@(3!:0))

NB.POST_rsv post processor. 

(".;(0=nc <'SHOWSMO_ijod_'){'1';'SHOWSMO_ijod_') smoutput IFACE=: (0 : 0)
NB. (rsv) interface word(s): 20240225j113928
NB. ------------------------
NB. read    NB. reads a file as a list of bytes
NB. rsvdec  NB. decode rsv bytes - marks nulls with (NULLMARK)
NB. rsvenc  NB. encode rsv bytes - marks nulls with (NULLMARK)
NB. rsvok   NB. 1 if blblcl rsv nouns have no bad bytes - 0 otherwise
NB. write   NB. writes a list of bytes to file
)

cocurrent 'base'
coinsert  'rsv'

