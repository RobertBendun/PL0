" Vim syntax file
" LANGUAGE: PL0
" Maintainer: Robert Bendun
" Filenames: *.pl0

if exists("b:current_syntax")
	finish
endif

syn keyword pl0_fun fun

syn match pl0_intrinsic display "#\<\w\+\>"

syn match pl0_dec_number display "\v<\d%(_?\d)*"

syn keyword pl0_todo contained TODO FIXME
syn region pl0_comment start="//" end="$" contains=pl0_todo
syn region pl0_shebang start="#!" end="$"

let b:current_syntax = "pl0"

hi def link pl0_dec_number Number
hi def link pl0_comment    Comment
hi def link pl0_fun        Keyword
hi def link pl0_intrinsic  Special
hi def link pl0_shebang    SpecialComment
hi def link pl0_todo       Todo
