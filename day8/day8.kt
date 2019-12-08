import java.io.File

typealias Pixel = Char

const val BLACK: Char = '0'
const val WHITE: Char = '1'
const val TRANSPARENT: Char = '2'

data class Size(val width: Int, val height: Int) {
	val pixels: Int = width * height
}

data class Layer(val pixels: List<Pixel>)

data class SpaceImageFormat(val size: Size, val layers: List<Layer>)

fun String.decode(size: Size): SpaceImageFormat =
	SpaceImageFormat(
		size = size,
		layers = trim().asIterable().chunked(size.pixels).map(::Layer)
	)

fun transparentLayer(size: Size): Layer =
	Layer(List<Pixel>(size.pixels) { TRANSPARENT })

fun Layer.countPixels(pixel: Pixel): Int =
	pixels.count { p -> p == pixel }

fun Pair<Pixel, Pixel>.merge() =
	if (first == TRANSPARENT) second else first

fun Layer.merge(next: Layer): Layer =
	Layer(pixels.zip(next.pixels).map { p -> p.merge() })

fun Layer.render(size: Size): String =
	pixels.map { p -> if (p == WHITE) '#' else ' ' }
		.chunked(size.width)
		.map { row -> row.joinToString("") }
		.joinToString("\n")

fun part1(image: SpaceImageFormat): Int {
	val layerWithLeastZeros = image.layers.minBy { layer -> layer.countPixels('0') }!!
	val ones = layerWithLeastZeros.countPixels('1')
	val twos = layerWithLeastZeros.countPixels('2')
	return ones * twos
}

fun part2(image: SpaceImageFormat): String =
	image.layers.fold(transparentLayer(image.size), Layer::merge)
		.render(image.size)

fun main() {
	val image = File("input.txt").readText().decode(Size(25, 6))
	println("Part 1... ${part1(image)}")
	println("Part 2...\n${part2(image)}")
}