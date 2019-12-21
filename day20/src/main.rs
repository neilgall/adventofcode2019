extern crate termion;

use std::collections::{HashMap, HashSet};
use std::fs::File;
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


// Position

#[derive(Copy,Clone,Eq,Hash,Debug)]
struct Pos {
	x: usize,
	y: usize
}

impl PartialEq for Pos {
	fn eq(&self, other: &Self) -> bool {
		self.x == other.x && self.y == other.y
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
	steps: Vec<Pos>
}

impl Route {
	fn new() -> Route {
		Route { steps: vec![] }
	}

	fn add(&mut self, p: &Pos) {
		self.steps.push(*p);
	}

	fn len(&self) -> usize {
		self.steps.len()
	}

	fn contains(&self, p: &Pos) -> bool {
		self.steps.contains(p)
	}
}


// Maze 

type Portals = HashMap<Pos, Pos>;

#[derive(Debug)]
struct Maze {
	grid: Vec<Vec<char>>,
	portals: Portals,
	start: Pos,
	end: Pos
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
	fn new(input: &str) -> Maze {
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
			portals: HashMap::new(),
			start: Pos { x: 0, y: 0 },
			end: Pos { x: 0, y: 0 }
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
		let mut scan = MazeScan::new();

		for y in 0..self.grid.len() {
			for x in 0..self.grid[y].len() {
				let pos = Pos { x, y };
				for dir in Dir::iter() {
					scan.check(self, &pos, dir);
				}
			}
		}

		for (name, ps) in scan.names_to_pos {
			if ps.len() == 2 {
				self.portals.insert(ps[0], ps[1]);
				self.portals.insert(ps[1], ps[0]);
			} else {
				eprintln!("unmatched name {}", name);
			}
		}
	}

	fn neighbours(&self, pos: &Pos) -> Vec<Pos> {
		let mut ns = Vec::new();
		for dir in Dir::iter() {
			match self.go(&pos, dir) {
				Some(p) => ns.push(p),
				None => {}
			}
		}
		match self.portals.get(pos) {
			Some(p) => ns.push(*p),
			None => {}
		}
		ns
	}

	fn find_routes(&self) -> Vec<Route> {
		self.search(&self.start, &HashSet::new())
	}

	fn search(&self, pos: &Pos, visited: &HashSet<Pos>) -> Vec<Route> {
		// print!("{}{}", clear::All, cursor::Goto(1,1));
		// self.visualise(&Route::new(pos), visited);

		let mut routes: Vec<Route> = Vec::new();
		if *pos == self.end {
			routes.push(Route::new());
		} else {
			for n in self.neighbours(pos) {
				if self[&n] == '.' && !visited.contains(&n) {
					let mut v = visited.clone();
					v.insert(n);
					self.search(&n, &v).into_iter().for_each(|mut r| {
						r.add(&pos);
						routes.push(r)
					});
				}
			}
		}
		routes
	}

	fn visualise(&self, route: &Route, visited: &HashSet<Pos>) {
		for (y, row) in self.grid.iter().enumerate() {
			for (x, c) in row.iter().enumerate() {
				let p = Pos { x, y };
				if route.contains(&p) {
					print!("+");
				} else if visited.contains(&p) {
					print!("~");
				} else if self.portals.contains_key(&p) {
					print!("*");
				} else {
					print!("{}", c);
				}
			}
			println!();
		}
	}

}


// Maze Scanner

struct MazeScan {
	names_to_pos: HashMap<String, Vec<Pos>>
}

impl MazeScan {
	fn new() -> MazeScan {
		MazeScan { names_to_pos: HashMap::new() }
	}

	fn found(&self, p: &Pos) -> bool {
		self.names_to_pos.values().any(|ps| ps.contains(p))
	}

	fn check(&mut self, maze: &mut Maze, pos: &Pos, dir: &Dir) {
		if maze[pos].is_uppercase() {
			for p in maze.go(pos, dir).iter() {
				if maze[p].is_uppercase() {
					for q in maze.go(p, dir).iter() {
						if maze[q] == '.' && !self.found(q) {
							let c1 = maze[pos] as u8;
							let c2 = maze[p] as u8;
							let cs = match dir {
								Dir::UP   => vec![c2, c1],
								Dir::LEFT => vec![c2, c1],
								_         => vec![c1, c2]
							};
							let name = String::from_utf8(cs).unwrap();
							if name == "AA" {
								maze.start = *q;
							} else if name == "ZZ" {
								maze.end = *q;
							} else {
								self.names_to_pos.entry(name).or_insert(vec![]).push(*q)
							}
						}
					}
				}
			}
		}
	}
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
           U   P   P               ");

	let routes = maze.find_routes();
	let shortest = routes.iter().min_by_key(|&r| r.len());
	assert_eq!(58, shortest.unwrap().len());
}


fn part1(maze: &Maze) {
	let routes = maze.find_routes();
	let shortest = routes.iter().min_by_key(|&r| r.len());
	println!("Part 1 .. {}", shortest.unwrap().len());	
}

fn main() -> Result<(), std::io::Error> {
	let input = Maze::new(&read_file("input.txt")?);
	part1(&input);

	Ok(())
}
