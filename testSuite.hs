import RegInterpreter

testAll = [x | (x,y) <- (zip inputs_full outputs_full), (regexFullMatch x) /= y] == []

inputs_full = [
    ("ab|c","ab"),
    ("ab|.","abd"),
    ("(ab|.)*","abd"),
    ("((ab)*c|c*a(ab)*)*","c"),
    ("((ab)*c|c*a(ab)*)*",""),
    ("((ab)*c|c*a(ab)*)*","a"),
    ("((ab)*c|c*a(ab)*)*","cccccccccccababcccccca"),
    ("((ab)*c|c*a(ab)*)*","ccccc"),
    ("((ab)*c|c*a(ab)*)*","aaaaaaaaaaaaaaa"),
    ("((ab)*c|c*a(ab)*)*","aaaaaaaaaaaaaaa"),
    ("((ab)*c|c*a(ab)*)*","ccccccccccaccca"),
    ("(((((((ab)*c|c*a(ab)*)*)*)*)*)*)*","cacacacaccccaab"),
    ("(((((((ab)*c|c*a(ab)*)*)*)*)*)*)*","cacacacaccccab"),
    ("((ab)*c|c*a(ab)*)*","cacacacaccccaab"),
    ("((ab)*c|c*a(ab)*)*","cacacacaccccab"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "bcd4ddddoaabaeeefeefefe"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "bcd4oaabaeeefeefefe"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "bcd4ddddoaabaeeefeeffefe"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "abaeeefeefefe"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "7e5ea5e3e"),
    ("(((a|bcd(.(d*|_).)*a*)ba(e(f|_)|_)*)|_((a|_).e)*((_)*)*(_))|((d.e)*c)", "c")]

outputs_full = [
    True, 
    False, 
    True,
    True,
    True,
    True,
    True,
    True,
    True,
    True,
    True,
    True,
    False,
    True,
    False,
    True,
    True,
    False,
    True,
    True,
    True]
