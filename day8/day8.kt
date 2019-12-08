import java.io.File

typealias Layer = List<Char>

data class SpaceImageFormat(val layers: List<Layer>)

fun String.decode(width: Int, height: Int): SpaceImageFormat =
	SpaceImageFormat(layers = trim().asIterable().chunked(width * height))

fun Layer.countDigits(digit: Char): Int =
	count { it == digit }

fun part1(image: SpaceImageFormat): Int {
	val layerWithLeastZeros = image.layers.minBy { it.countDigits('0') }!!
	val ones = layerWithLeastZeros.countDigits('1')
	val twos = layerWithLeastZeros.countDigits('2')
	return ones * twos
}

fun main() {
	val image = File("input.txt").readText().decode(25, 6)
	println("Part 1... ${part1(image)}")
}