extern crate termion;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::File;
use std::io::stdin;
use std::io::prelude::*;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use termion::{clear,cursor};


fn read_file(filename: &str) -> std::io::Result<String> {
	let mut file = File::open(filename)?;
	let mut contents = String::new();
	file.read_to_string(&mut contents)?;
	Ok(contents)
}


// Position without Layer

#[derive(Copy,Clone,Debug,Eq,Hash)]
struct Pos {
	x: usize,
	y: usize
}

impl PartialEq for Pos {
	fn eq(&self, other: &Self) -> bool {
		self.x == other.x && self.y == other.y
	}
}

impl Pos {
	fn on_layer(&self, layer: i32) -> LayerPos {
		LayerPos { x: self.x, y: self.y, layer: layer }
	}
}

impl fmt::Display for Pos {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{},{}", self.x, self.y)
	}
}

// Position with Layer

#[derive(Copy,Clone,Debug,Eq,Hash)]
struct LayerPos {
	x: usize,
	y: usize,
	layer: i32
}

impl PartialEq for LayerPos {
	fn eq(&self, other: &Self) -> bool {
		self.x == other.x && self.y == other.y && self.layer == other.layer
	}
}

impl LayerPos {
	fn with_layer(&self, layer: i32) -> LayerPos {
		LayerPos { x: self.x, y: self.y, layer: layer }
	}

	fn jump(&self, from: &LayerPos) -> LayerPos {
		LayerPos { x: self.x, y: self.y, layer: from.layer + self.layer }
	}

	fn pos(&self) -> Pos {
		Pos { x: self.x, y: self.y }
	}
}

impl fmt::Display for LayerPos {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{},{},{}", self.x, self.y, self.layer)
	}
}

// Direction

#[derive(Copy,Clone)]
enum Dir {
	UP, DOWN, LEFT, RIGHT
}

impl Dir {
	fn iter() -> Iter<'static, Dir> {
		static DIRS: [Dir; 4] = [Dir::UP, Dir::DOWN, Dir::LEFT, Dir::RIGHT];
		DIRS.into_iter()
	}
}


// Route

#[derive(Debug)]
struct Route {
	steps: Vec<LayerPos>
}

impl Route {
	fn new() -> Route {
		Route { steps: vec![] }
	}

	fn add(&mut self, p: &LayerPos) {
		self.steps.push(*p);
	}

	fn len(&self) -> usize {
		self.steps.len()
	}

	fn contains(&self, p: &LayerPos) -> bool {
		self.steps.contains(p)
	}
}


// Maze 

#[derive(Debug)]
struct Maze {
	grid: Vec<Vec<char>>,
	layered: bool,
	portals: HashMap<Pos, LayerPos>,
	start: LayerPos,
	end: LayerPos
}

impl Index<&Pos> for Maze {
	type Output = char;

	fn index(&self, pos: &Pos) -> &Self::Output {
		&self.grid[pos.y][pos.x]
	}
}

impl IndexMut<&Pos> for Maze {
	fn index_mut(&mut self, pos: &Pos) -> &mut Self::Output {
		&mut self.grid[pos.y][pos.x]
	}
}

impl Maze {
	fn new(input: &str, layered: bool) -> Maze {
		let height: usize = input.lines().count();
		let width: usize = input.lines().map(|line| line.len()).max().unwrap_or(0);

		let mut grid = vec![vec!['\0'; width]; height];
		for (y, line) in input.lines().enumerate() {
			for (x, c) in line.chars().enumerate() {
				grid[y][x] = c;
			}
		}

		let mut maze = Maze {
			grid,
			layered,
			portals: HashMap::new(),
			start: LayerPos { x: 0, y: 0, layer: 0 },
			end: LayerPos { x: 0, y: 0, layer: 0 }
		};

		maze.find_portals();
		maze
	}

	fn go(&self, p: &Pos, dir: &Dir) -> Option<Pos> {
		match dir {
			Dir::UP =>
				if p.y == 0 { None } else { Some(Pos { x: p.x, y: p.y - 1 }) },

			Dir::DOWN =>
				if p.y >= self.grid.len()-1 { None } else { Some(Pos { x: p.x, y: p.y + 1 }) },

			Dir::LEFT =>
				if p.x == 0 { None } else { Some(Pos { x: p.x - 1, y: p.y }) },

			Dir::RIGHT =>
				if p.x >= self.grid[p.y].len()-1 { None } else { Some(Pos { x: p.x + 1, y: p.y }) }
		}
	}

	fn find_portals(&mut self) {
		let mut portals: HashMap<String, Vec<LayerPos>> = HashMap::new();

		for y in 0..self.grid.len() {
			for x in 0..self.grid[y].len() {
				let pos = Pos { x, y };
				if self[&pos].is_uppercase() {
					for dir in Dir::iter() {
						self.check_for_portal(&pos, dir, &mut portals);
					}
				}
			}
		}

		for (name, ps) in portals {
			if ps.len() == 2 {
				self.portals.insert(ps[0].pos(), ps[1]);
				self.portals.insert(ps[1].pos(), ps[0]);
			} else {
				eprintln!("unmatched name {}", name);
			}
		}
	}

	fn check_for_portal(&mut self, pos: &Pos, dir: &Dir, portals: &mut HashMap<String, Vec<LayerPos>>) {
		for one in self.go(pos, dir).iter() {
			if self[one].is_uppercase() {
				for two in self.go(one, dir).iter() {
					if self[two] == '.' && !portals.values().any(|ps| ps.iter().any(|p| p.pos() == *two)) {
						let c1 = self[pos] as u8;
						let c2 = self[one] as u8;
						let cs = match dir {
							Dir::UP   => vec![c2, c1],
							Dir::LEFT => vec![c2, c1],
							_         => vec![c1, c2]
						};
						let name = String::from_utf8(cs).unwrap();

						if name == "AA" {
							self.start = two.on_layer(0);
						} else if name == "ZZ" {
							self.end = two.on_layer(0);
						} else {
							let portal_vec = portals.entry(name).or_insert(vec![]);
							if self.layered {
								if self.near_edge(two) {
									portal_vec.push(two.on_layer(1));
								} else {
									portal_vec.push(two.on_layer(-1));
								}
							} else {
								portal_vec.push(two.on_layer(0));
							}
						}
					}
				}
			}
		}
	}

	fn near_edge(&self, p: &Pos) -> bool {
		p.y < 5 || p.x < 5 || self.grid.len()-5 < p.y || self.grid[p.y].len()-5 < p.x
	}


	fn neighbours(&self, pos: &LayerPos) -> Vec<LayerPos> {
		let mut ns = Vec::new();
		for dir in Dir::iter() {
			match self.go(&pos.pos(), dir) {
				Some(p) => if self[&p] == '.' { ns.push(p.on_layer(pos.layer)) },
				None => {}
			}
		}
		match self.portals.get(&pos.pos()) {
			Some(p) => {
				let q = p.jump(pos);
				if q.layer >= 0 { ns.push(q); }
			}
			None => {}
		}
		ns
	}

	fn find_routes(&self, visual: bool) -> Vec<Route> {
		let mut visited = HashSet::new();
		visited.insert(self.start);
		self.search(&self.start, &visited, visual)
	}

	fn search(&self, pos: &LayerPos, visited: &HashSet<LayerPos>, visual: bool) -> Vec<Route> {
		if visual {
			print!("{}{}", clear::All, cursor::Goto(1,1));
			self.visualise(&Route::new(), pos, visited);
		}

		let mut routes: Vec<Route> = Vec::new();
		if pos.layer == 0 && *pos == self.end {
			routes.push(Route::new());
		} else {
			for n in self.neighbours(pos) {
				if !visited.contains(&n) {
					let mut v = visited.clone();
					v.insert(n);
					self.search(&n, &v, visual).into_iter().for_each(|mut r| {
						r.add(&pos);
						routes.push(r)
					});
				}
			}
		}
		routes
	}

	fn visualise(&self, route: &Route, pos: &LayerPos, visited: &HashSet<LayerPos>) {
		for (y, row) in self.grid.iter().enumerate() {
			for (x, c) in row.iter().enumerate() {
				let p = Pos { x, y };
				if p == pos.pos() {
					print!("@");
				} else if route.contains(&pos) {
					print!("+");
				} else if visited.iter().any(|v| v.pos() == p) {
					print!("~");
				} else if self.portals.contains_key(&p) {
					print!("*");
				} else {
					print!("{}", c);
				}
			}
			println!();
		}
		print!("pos={} neighbours=", pos);
		for n in self.neighbours(pos) { print!("{}  ", n); }
		println!();

		let mut i = String::new();
		stdin().read_line(&mut i).unwrap();
	}

}



// #[test]
fn test_example_map() {
	let maze = Maze::new("
                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ", false);

	let routes = maze.find_routes(false);
	let shortest = routes.iter().min_by_key(|&r| r.len());
	assert_eq!(Some(58), shortest.map(Route::len));
}

#[test]
fn test_example_layered_map() {
	let maze = Maze::new("
             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     ", true);

	let routes = maze.find_routes(false);
	let shortest = routes.iter().min_by_key(|&r| r.len());
	assert_eq!(Some(396), shortest.map(Route::len));
}


fn part1(input: &str) {
	let maze = Maze::new(input, false);
	let routes = maze.find_routes(false);
	let shortest = routes.iter().min_by_key(|&r| r.len());
	println!("Part 1 .. {}", shortest.unwrap().len());	
}

fn part2(input: &str) {
	let maze = Maze::new(input, true);
	let routes = maze.find_routes(false);
	let shortest = routes.iter().min_by_key(|&r| r.len());
	println!("Part 1 .. {}", shortest.unwrap().len());	
}

fn main() -> Result<(), std::io::Error> {
	let input = read_file("input.txt")?;
	part1(&input);
	part2(&input);

	Ok(())
}
