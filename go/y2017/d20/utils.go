package main

import "golang.org/x/exp/constraints"

func MinIndexByKey[T any, K constraints.Ordered](collection []T, extractKey func(a T) K) (int, T) {
	var min T
	var minKey K
	var minIndex = -1

	if len(collection) == 0 {
		return minIndex, min
	}

	min = collection[0]
	minKey = extractKey(min)
	minIndex = 0

	for i := 1; i < len(collection); i++ {
		item := collection[i]
		itemKey := extractKey(item)

		if itemKey < minKey {
			min = item
			minKey = itemKey
			minIndex = i
		}
	}

	return minIndex, min
}
