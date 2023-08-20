struct Value: Hashable, CustomStringConvertible {
    let value: Int
    
    var description: String {
         return String(value)
    }
}

struct StateId: Hashable, CustomStringConvertible {
    let symbol: Character
    
    var description: String {
         return String(symbol)
    }
}

struct Action {
    let write: Value
    let move: Int
    let continueWith: StateId
}

struct Instructions {
    let instructions: [Value: Action]
}

struct Program {
    let beginIn: StateId
    let steps: Int
    let instructions: [StateId: Instructions]
   
    func run() -> State {
        var state = State(currentState: beginIn, currentCursor: 0, tape: [:])
        for step in 0..<steps {
            state = state.next(using: instructions)
            if step % 100_000 == 0 {
                print("Step \(step)")
            }
        }
        return state
    }
}

struct State {
    let currentState: StateId
    let currentCursor: Int
    let tape: [Int: Value]

    func valueAtCursor() -> Value {
        return tape[currentCursor] ?? Value(value: 0)
    }
    
    func next(using instructions: [StateId: Instructions]) -> State {
        let currentValue = valueAtCursor()
        guard let action = instructions[currentState]?.instructions[currentValue] else {
            fatalError("No instructions available for current state \(currentState) and value \(currentValue)")
        }
        
        let newEntries = [currentCursor: action.write]
        let updatedTape = tape.merging(newEntries) { (_, new) in new }
        
        let updatedCursor: Int = currentCursor + action.move
        let updatedState = action.continueWith
        
        return State(currentState: updatedState, currentCursor: updatedCursor, tape: updatedTape)
    }
    
    func diagnosticChecksum() -> Int {
        return tape.countIf { value -> Bool in
            value.value == 1
        }
    }
}

func solve(input: String) -> Int {
    let program = parse(input: input)
    return program.run().diagnosticChecksum()
}
