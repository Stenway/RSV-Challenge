NB.*rsv_smoke t-- (rsv) smoke tests.
NB.
NB. Smoke tests  for  the (rsv.ijs)  script.  When run in  J  the
NB. results are all  1s. Run in J current J  9.5 systems. Depends
NB. on verbs loaded by the standard J profile.
NB.
NB. verbatim: J install
NB.
NB.   https://code.jsoftware.com/wiki/System/Installation#J9.5_release
NB.
NB. created: 2024jan10
NB. changes: -----------------------------------------------------

load '~addons/jacks/rsv.ijs'
NB. load 'rsv'

NB. is configured folder (1 folder set, 0 otherwise): iscf '~RSVTEST'
iscf_ijod_=:[: -. jpath_j_ -: ]

NB. test folder must be configured
iscf_ijod_ '~RSVTEST'

NB. test files at: https://github.com/Stenway/RSV-Challenge/tree/main/TestFiles
NB. valid test files must decode without errors and have no bad bytes
valid_rsv=: 1 dir '~RSVTEST/Val*.rsv'
rsvok@rsvdec@read&> valid_rsv

NB. encode decode test - 1 when OK 0 otherwise
rsvdent=: {{rsv -: rsvenc rsvdec rsv=. read y}}

NB. decode encode first valid rsv test file
rsvdent ;0{valid_rsv

NB. list any valid files that fail decode/encode test - should be none
bool=: rsvdent&> valid_rsv
smoutput >valid_rsv #~ -.bool

NB. fail on any zeroes
bool

NB. format data in all list of lists cells as utf8
utf8cells=: utf8@":L:0

NB. generate some J list of lists
lol0=: (<"1 <"0 ?2 2$1000), (<'') , <"0 ;:'some words'

NB. cell data is not all character
1 [ smoutput (rsvok :: 'expected assertion caught'"_) lol0

lol0=: utf8cells ,&.> lol0

NB. unicode
lol1=: utf8cells ,&.> (<"0 ] 3 # < ucp 960 + i. 10), <<ucp 8704+ i. 5

NB. are we good?
rsvok lol0
rsvok lol1

NB. write rsv files to J temp and reload
(rsvenc lol0) write jpath '~temp/lol0.rsv'
(rsvenc lol1) write jpath '~temp/lol1.rsv'

NB. should match when read back
lol0 -: rsvdec read jpath '~temp/lol0.rsv'
lol1 -: rsvdec read jpath '~temp/lol1.rsv'
