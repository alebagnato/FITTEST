
function last_idx_base Ch [id] Id [id]
	replace [number]
		N [number]
	construct Idx [number]
		_ [index Id Ch]
	deconstruct Idx
		0
	by
		N
end function

function last_idx_recur Ch [id] Id [id]

	replace [number]
		N [number]

	construct Idx [number]
		_ [index Id Ch]

	deconstruct not Idx
		0

	construct Idx1 [number]
		Idx [+ 1]

	construct NextPart [id]
		Id [: Idx1 999]

	construct NewN [number]
		Idx [+ N]

	construct LastN [number]
		NewN [last_idx_recur Ch NextPart][last_idx_base Ch NextPart]

	by
		LastN
end function

function last_idx Ch [id] Id [id]
	replace [number]
		N [number]
	by
		N [last_idx_recur Ch Id]
end function

function split_last_by Ch [id] Id [id]

	replace [repeat id]
		_ [repeat id]

	construct Idx [number]
		_ [last_idx Ch Id]

	assert 
		Idx [> 0]

	construct Idx1 [number]
		Idx [- 1]

	construct Idx2 [number]
		Idx [+ 1]

	construct FirstPart [id]
		Id [: 1 Idx1]

	construct LastPart [id]
		Id [: Idx2 999]

	construct List [repeat id]
		FirstPart LastPart

	by
		List

end function

function split_last_by_as_str Ch [id] Id [id]
       replace [list stringlit]
                _ [list stringlit]

	construct Pair [repeat id]
		_ [split_last_by Ch Id]


	deconstruct Pair
		F [id] L [id]

	construct Fstr [stringlit]
		_ [unparse F]

	construct Lstr [stringlit]
		_ [unparse L]

	by
		Fstr ', Lstr
end function
