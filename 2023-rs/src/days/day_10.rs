/*
 * Copyright (c) 2024 Kirill Lukashev <kirill.lukashev.sic@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
 * https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
 * <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
 * option. This file may not be copied, modified, or distributed
 * except according to those terms.
 */

use std::collections::VecDeque;

use self::TileKind::*;
use crate::prelude::*;

type Coord = (isize, isize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Tile {
    coord: Coord,
    kind: TileKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum TileKind {
    /// S
    Start,
    /// |
    NorthSouth,
    /// -
    EastWest,
    /// L
    NorthEast,
    /// J
    NorthWest,
    /// 7
    SouthWest,
    /// F
    SouthEast,
}

impl Tile {
    fn can_go_to(self, to: Tile) -> bool {
        let dx = to.coord.0 - self.coord.0;
        let dy = to.coord.1 - self.coord.1;

        match (dx, dy) {
            (1, 0) => {
                matches!(
                    (self.kind, to.kind),
                    (
                        Start | EastWest | NorthEast | SouthEast,
                        Start | EastWest | NorthWest | SouthWest,
                    )
                )
            },
            (-1, 0) => {
                matches!(
                    (self.kind, to.kind),
                    (
                        Start | EastWest | NorthWest | SouthWest,
                        Start | EastWest | NorthEast | SouthEast,
                    )
                )
            },
            (0, 1) => {
                matches!(
                    (self.kind, to.kind),
                    (
                        Start | NorthSouth | SouthWest | SouthEast,
                        Start | NorthSouth | NorthWest | NorthEast,
                    )
                )
            },
            (0, -1) => {
                matches!(
                    (self.kind, to.kind),
                    (
                        Start | NorthSouth | NorthWest | NorthEast,
                        Start | NorthSouth | SouthWest | SouthEast
                    )
                )
            },
            _ => false,
        }
    }
}

type Map = FxHashMap<Coord, Tile>;

fn part1(start: Coord, map: &Map) -> usize {
    let mut visited = FxHashSet::default();
    let mut queue = VecDeque::new();
    queue.push_back((start, 0));
    let mut max_distance = 0;
    while let Some((coord, distance)) = queue.pop_front() {
        if !visited.insert(coord) {
            continue;
        }
        let tile = map.get(&coord).copied().expect("Should be in map");
        max_distance = max_distance.max(distance);
        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let new_coord = (coord.0 + dx, coord.1 + dy);
            if let Some(new_tile) = map.get(&new_coord).copied() {
                if tile.can_go_to(new_tile) {
                    queue.push_back((new_coord, distance + 1));
                }
            }
        }
    }
    max_distance
}

fn parse(input: &str) -> Result<(Coord, Map)> {
    let mut map = FxHashMap::default();
    let mut start = None;
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.bytes().enumerate() {
            let coord = (x as isize, y as isize);
            let kind = match c {
                b'S' => Start,
                b'|' => NorthSouth,
                b'-' => EastWest,
                b'L' => NorthEast,
                b'J' => NorthWest,
                b'7' => SouthWest,
                b'F' => SouthEast,
                _ => continue,
            };
            let tile = Tile { coord, kind };
            if kind == Start {
                start = Some(tile);
            } else {
                map.insert(coord, tile);
            }
        }
    }
    if let Some(start) = start {
        map.insert(start.coord, start);
        Ok((start.coord, map))
    } else {
        Err(format_err!("No start tile found"))
    }
}

pub struct Day10;

impl Solution for Day10 {
    fn parse(&self, input: &str) -> Result<()> {
        parse(input).map(|_| ())
    }

    fn part1(&self, input: &str) -> Result<String> {
        let (start, map) = parse(input)?;
        Ok(part1(start, &map).to_string())
    }

    fn part2(&self, input: &str) -> Result<String> {
        Ok("not yet implemented".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_1() {
        let input = r#".....
.S-7.
.|.|.
.L-J.
....."#;

        let (start, map) = parse(input).unwrap();
        assert_eq!(part1(start, &map), 4);
    }

    #[test]
    fn part1_2() {
        let input = r#"..F7.
.FJ|.
SJ.L7
|F--J
LJ..."#;

        let (start, map) = parse(input).unwrap();
        assert_eq!(part1(start, &map), 8);
    }

    #[test]
    fn part1_3() {
        let input = r#"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"#;

        let (start, map) = parse(input).unwrap();
        assert_eq!(part1(start, &map), 8);
    }
}
