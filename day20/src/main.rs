extern crate termion;

use std::collections::HashMap;
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

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
enum PortalDir {
	IN, OUT
}

#[derive(Debug)]
struct Route {
	steps: Vec<LayerPos>,
	portals: Vec<LayerPos>,
	portal_dir: PortalDir
}

fn portal_dir_between(p1: &LayerPos, p2: &LayerPos) -> Option<PortalDir> {
	if p1.layer > p2.layer { 
		Some(PortalDir::IN)
	} else if p1.layer < p2.layer {
		Some(PortalDir::OUT)
	} else {
		None
	}
}

impl Route {
	fn new() -> Route {
		Route { 
			steps: vec![],
			portals: vec![],
			portal_dir: PortalDir::IN
		}
	}

	fn portal_dir_if_adding(&self, pos: &LayerPos) -> Option<PortalDir> {
		portal_dir_between(self.steps.last()?, pos)
	}

	fn add(&mut self, pos: &LayerPos) {
		if let Some(dir) = self.portal_dir_if_adding(pos) {
			self.portals.push(*pos);
			self.portal_dir = dir
		}
		self.steps.push(*pos);
	}

	fn len(&self) -> usize {
		self.steps.len()
	}

	fn contains(&self, p: &LayerPos) -> bool {
		self.steps.contains(p)
	}

	fn contains_on_any_layer(&self, p: &Pos) -> bool {
		self.steps.iter().any(|s| s.pos() == *p)
	}

	fn adding_would_create_loop(&self, pos: &LayerPos) -> bool {
		if let Some(i) = self.portals.iter().rposition(|s| s.pos() == pos.pos()) {
			if i == 0 {
				false
			} else {
				let dir = self.portal_dir_if_adding(pos);
				let prev_dir = portal_dir_between(&self.portals[i-1], &self.portals[i]);
				dir == prev_dir
			}
		} else {
			false
		}
	}
}

impl Clone for Route {
	fn clone(&self) -> Self {
		Route { 
			steps: self.steps.to_vec(),
			portals: self.portals.to_vec(),
			portal_dir: self.portal_dir
		}
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
			if let Some(p) = self.go(&pos.pos(), dir) {
				if self[&p] == '.' {
					ns.push(p.on_layer(pos.layer));
				}
			}
		}

		if let Some(p) = self.portals.get(&pos.pos()) {
			let q = p.jump(pos);
			if q.layer >= 0 { ns.push(q); }
		}

		ns
	}

	fn find_routes(&self, visual: bool) -> Vec<Route> {
		let mut routes = Vec::new();
		let mut stack = Vec::new();
		stack.push( (self.start, Route::new()) );

		while let Some((pos, route)) = stack.pop() {
			if visual {
				print!("{}{}", clear::All, cursor::Goto(1,1));
				self.visualise(&route, &pos);
			}

			if pos.layer == 0 && pos == self.end {
				routes.push(route);
			} else {
				for n in self.neighbours(&pos) {
					if !route.contains(&n) && !route.adding_would_create_loop(&n) {
						let mut r = route.clone();
						r.add(&n);
						stack.push((n, r));
					}
				}
			}
		}
		routes
	}

	fn visualise(&self, route: &Route, pos: &LayerPos) {
		for (y, row) in self.grid.iter().enumerate() {
			for (x, c) in row.iter().enumerate() {
				let p = Pos { x, y };
				if p == pos.pos() {
					print!("@");
				} else if route.contains(&p.on_layer(pos.layer)) {
					print!("+");
				} else if route.contains_on_any_layer(&p) {
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
		for n in self.neighbours(pos) { 
			let x = if route.adding_would_create_loop(&n) { " *loop*" } else if route.contains(&n) { "*" } else { "" };
			print!("{}{}  ", n, x);
		}
		println!(" route={} {:?}", route.len(), route.portal_dir);
		for p in route.portals.iter() { print!("{}  ", p); }
		println!();

		if self.portals.contains_key(&pos.pos()) {
			let mut i = String::new();
			stdin().read_line(&mut i).unwrap();
		}
	}

}


#[test]
fn test_route_loop_detection() {
	let mut r = Route::new();
	r.add(&LayerPos { x: 0, y: 2, layer: 0 });
	r.add(&LayerPos { x: 0, y: 1, layer: 0 });
	r.add(&LayerPos { x: 0, y: 0, layer: 0 });
	r.add(&LayerPos { x: 1, y: 0, layer: 0 });
	r.add(&LayerPos { x: 2, y: 0, layer: 0 });
	r.add(&LayerPos { x: 0, y: 0, layer: 1 });
	r.add(&LayerPos { x: 1, y: 0, layer: 1 });
	r.add(&LayerPos { x: 2, y: 0, layer: 1 });
	assert!(r.adding_would_create_loop(&LayerPos { x: 0, y: 0, layer: 2 }));
}

#[test]
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

	let routes = maze.find_routes(true);
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
