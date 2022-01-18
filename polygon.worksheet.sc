/*
{([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
[[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
[{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
[<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
<{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
*/



/*
{([(<[}>{{[( - Expected ], but found } instead.
[[<[)<([ - Expected ], but found ) instead.
[{[{(]}([{[{( - Expected ), but found ] instead.
[<(<(<(<))><(( - Expected >, but found ) instead.
<{([([>(<<{{ - Expected ], but found > instead.
*/


"{}[]()<>{}".replace("{}", "").replace("()", "").replace("<>", "").replace("[]", "")


/*

[({([[{{ - Complete by adding }}]])})].
[(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
(((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
{<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
<{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

*/


val s1 = ")}>]})"
val s2 = ")]}}<{]>)])}"
val ends = Set('<', '(', '[', '{')

ends intersect s2.toSet

(13 / 2).toInt
(5 / 2).toInt
(3 / 2).toInt