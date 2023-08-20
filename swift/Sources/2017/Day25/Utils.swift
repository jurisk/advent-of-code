extension Dictionary {
    func countIf(predicate: (Value) -> Bool) -> Int {
        return self.values.reduce(0) { count, value in
            count + (predicate(value) ? 1 : 0)
        }
    }
}
