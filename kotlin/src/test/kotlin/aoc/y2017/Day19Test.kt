package aoc.y2017

import java.nio.file.Files
import java.nio.file.Paths
import kotlin.test.Test
import kotlin.test.assertEquals

internal class Day19Test {
    private fun readFile(name: String): String {
        val path = "aoc/y2017/$name"
        val url = javaClass.classLoader.getResource(path)
        if (url != null) {
            return String(Files.readAllBytes(Paths.get(url.toURI())))
        } else {
            throw RuntimeException("Path $path not found")
        }
    }

    @Test
    fun part1Test() {
        val result = part1(readFile("test.txt"))
        assertEquals("ABCDEF", result)
    }

    @Test
    fun part1Real() {
        val result = part1(readFile("real.txt"))
        assertEquals("DTOUFARJQ", result)
    }

    @Test
    fun part2Test() {
        val result = part2(readFile("test.txt"))
        assertEquals(38, result)
    }

    @Test
    fun part2Real() {
        val result = part2(readFile("real.txt"))
        assertEquals(16_642, result)
    }
}
