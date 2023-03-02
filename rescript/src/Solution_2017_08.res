let data = Node.Fs.readFileSync("data/2017_08_real.txt", #utf8)

type register = Register(string)

type operation =
    | Increase
    | Decrease

exception ParsingError({input: string})

let parseInt = (s) => {
    switch Belt.Int.fromString(s) {
        | Some(x) => x
        | None => raise(ParsingError({input: s}))
    }
}

let parseOperation = (s) => {
    switch s {
        | "inc" => Increase
        | "dec" => Decrease
        | _ => raise(ParsingError({input: s}))
    }
}

type comparisonOperand =
    | Equals
    | NotEquals
    | LessThan
    | LessThanOrEquals
    | GreaterThan
    | GreaterThanOrEquals

let parseOperand = (s) => {
    switch s {
        | "==" => Equals
        | "!=" => NotEquals
        | "<" => LessThan
        | "<=" => LessThanOrEquals
        | ">" => GreaterThan
        | ">=" => GreaterThanOrEquals
        | _ => raise(ParsingError({input: s}))
    }
}

type instruction = {
    register: register,
    operation: operation,
    howMuch: int,
    comparisonRegister: register,
    comparisonOperand: comparisonOperand,
    compareWith: int,
}

let parseInstruction = (s) => {
    let c = Js.String.split(" if ", s)

    let a = Js.String.split(" ", c[0])
    let b = Js.String.split(" ", c[1])

    switch (a, b) {
        | ([r, o, hm], [cr, co, cw]) => { 
            register: Register(r),
            operation: parseOperation(o),
            howMuch: parseInt(hm),
            comparisonRegister: Register(cr),
            comparisonOperand: parseOperand(co),
            compareWith: parseInt(cw),
        }
        | _ => raise(ParsingError({input: s}))
    }
}

let applyInstruction = (m, i) => {
    let register = Belt.Map.getWithDefault(m, i.register, 0)
    let comparisonRegister = Belt.Map.getWithDefault(m, i.comparisonRegister, 0)
    let co = switch i.comparisonOperand {
        | Equals => \"=="
        | NotEquals => \"!="
        | LessThan => \"<"
        | LessThanOrEquals => \"<="
        | GreaterThan => \">"
        | GreaterThanOrEquals => \">="
    }
    let o = switch i.operation {
        | Increase => \"+"
        | Decrease => \"-"
    }

    let b = co(comparisonRegister, i.compareWith)
    if b {
        let newValue = o(register, i.howMuch)
        Belt.Map.set(m, i.register, newValue)
    } else {
        m
    }
}

let maxValue = (m) => {
    let v = Belt.Map.valuesToArray(m)
    Js.Math.maxMany_int(v)
}

let applyInstruction2 = ((m, highest), i) => {
    let newM = applyInstruction(m, i)
    let newHighest = maxValue(newM)
    let max = Js.Math.max_int(highest, newHighest)
    (newM, max)
}

let instructionStrings = Js.String.split("\n", data)
let instructions = Belt.Array.map(instructionStrings, parseInstruction)

module RegisterCmp = Belt.Id.MakeComparable({
  type t = register
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let emptyRegisters = Belt.Map.make(~id=module(RegisterCmp))

let resultingRegisters = Js.Array2.reduce(instructions, applyInstruction, emptyRegisters)
let part1 = maxValue(resultingRegisters)
Js.log(`Part 1: ${Belt.Int.toString(part1)}`)
assert(part1 == 6061)

let (_, part2) = Js.Array2.reduce(instructions, applyInstruction2, (emptyRegisters, 0))
Js.log(`Part 2: ${Belt.Int.toString(part2)}`)
assert(part1 == 6696)
