let src = Logs.Src.create "glicko2"

let info x = Logs.info ~src x
let debug x = Logs.debug ~src x
let err x = Logs.err ~src x
