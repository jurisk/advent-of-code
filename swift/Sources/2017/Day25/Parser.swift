func parse(input: String) -> Program {
    let blocks = input.components(separatedBy: "\n\n")
    
    let beginState = StateId(symbol: blocks[0].split(separator: "\n")[0].dropLast(1).last!)
    let steps = Int(blocks[0].split(separator: "\n")[1].split(separator: " ")[5])!
    
    func extractAction(from lines: [Substring]) -> (Value, Action) {
        let value = Int(lines[0].split(separator: " ")[5].dropLast(1))!
        
        let writeValue = Int(lines[1].split(separator: " ")[4].dropLast())!
        let write = Value(value: writeValue)

        let move: Int = {
            let moveString = lines[2]
            switch moveString {
            case _ where moveString.contains("right"):
                return 1
            case _ where moveString.contains("left"):
                return -1
            default:
                fatalError("Unrecognized move direction: \(moveString)")
            }
        }()
        
        let continueWith = StateId(symbol: lines[3].dropLast(1).last!)
        
        return (Value(value: value), Action(write: write, move: move, continueWith: continueWith))
    }
    
    let instructionBlocks = blocks.dropFirst()
    let instructions = instructionBlocks.reduce(into: [StateId: Instructions]()) { dict, block in
        let lines = block.split(separator: "\n")
        let stateId = StateId(symbol: lines[0].dropLast(1).last!)
        
        var actions = [Value: Action]()
        for i in stride(from: 1, to: lines.count, by: 4) {
            let subset = lines[i..<i+4]
            let (value, action) = extractAction(from: Array(subset))
            actions[value] = action
        }
        
        dict[stateId] = Instructions(instructions: actions)
    }
    
    return Program(beginIn: beginState, steps: steps, instructions: instructions)
}
