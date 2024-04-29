/*
 * Copyright (c) 2024 Kirill Lukashev <kirill.lukashev.sic@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
 * https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
 * <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
 * option. This file may not be copied, modified, or distributed
 * except according to those terms.
 */

use std::collections::HashMap;
use std::ops::ControlFlow;

use itertools::Itertools;

use crate::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Route([u8; 3]);

impl Route {
    fn from_str(s: &str) -> Self {
        let mut route = [0; 3];
        for (i, c) in s.bytes().enumerate() {
            route[i] = c;
        }
        Self(route)
    }

    fn to_str(self) -> String {
        self.0.iter().map(|&c| c as char).collect()
    }

    fn is_start_route(&self) -> bool {
        self.0[2] == b'A'
    }

    fn is_end_route(&self) -> bool {
        self.0[2] == b'Z'
    }
}

struct Data {
    instructions: Vec<Instruction>,
    routes: FxHashMap<Route, (Route, Route)>,
}

fn parse(input: &str) -> Result<Data> {
    let mut lines = input.lines();

    let instructions: Vec<Instruction> = lines
        .next()
        .ok_or(format_err!("No instructions"))?
        .bytes()
        .map(|b| {
            match b {
                b'L' => Ok(Instruction::Left),
                b'R' => Ok(Instruction::Right),
                _ => Err(format_err!("Invalid instruction")),
            }
        })
        .collect::<Result<_>>()?;

    let capacity = input.lines().count() - 2;
    let mut routes_map = FxHashMap::with_capacity_and_hasher(capacity, Default::default());

    lines.next();

    for line in lines {
        let (route, routes) = line
            .split(" = ")
            .collect_tuple()
            .ok_or(format_err!("Invalid route"))?;
        let (lhs, rhs) = routes
            .trim_matches(['(', ')'].as_ref())
            .split(", ")
            .collect_tuple()
            .ok_or(format_err!("Invalid routes"))?;
        let (route, lhs, rhs) = (
            Route::from_str(route),
            Route::from_str(lhs),
            Route::from_str(rhs),
        );
        routes_map.insert(route, (lhs, rhs));
    }

    Ok(Data {
        instructions,
        routes: routes_map,
    })
}

fn part1(data: &Data) -> usize {
    let init = data
        .routes
        .get(&Route::from_str("AAA"))
        .copied()
        .expect("Should have AAA route");

    let ControlFlow::Break((_, n_steps)) = data
        .instructions
        .iter()
        .copied()
        .cycle()
        .enumerate()
        .try_fold((init, 0), |((lhs, rhs), _), (step, instruction)| {
            let route = match instruction {
                Instruction::Left => lhs,
                Instruction::Right => rhs,
            };

            let (lhs, rhs) = data.routes.get(&route).copied().expect("Should have route");

            if route == Route::from_str("ZZZ") {
                ControlFlow::Break(((lhs, rhs), step + 1))
            } else {
                ControlFlow::Continue(((lhs, rhs), step + 1))
            }
        })
    else {
        unreachable!()
    };

    n_steps
}

fn part2(data: &Data) -> usize {
    let start_routes: Vec<Route> = data
        .routes
        .keys()
        .copied()
        .filter(Route::is_start_route)
        .collect();

    fn route_length(route: Route, data: &Data) -> usize {
        let init = data.routes.get(&route).copied().expect("Should have route");

        let ControlFlow::Break((_, n_steps)) = data
            .instructions
            .iter()
            .copied()
            .cycle()
            .enumerate()
            .try_fold((init, 0), |((lhs, rhs), _), (step, instruction)| {
                let route = match instruction {
                    Instruction::Left => lhs,
                    Instruction::Right => rhs,
                };

                let (lhs, rhs) = data.routes.get(&route).copied().expect("Should have route");

                if route.is_end_route() {
                    ControlFlow::Break(((lhs, rhs), step + 1))
                } else {
                    ControlFlow::Continue(((lhs, rhs), step + 1))
                }
            })
        else {
            unreachable!()
        };

        n_steps
    }

    start_routes
        .into_iter()
        .map(|route| route_length(route, data))
        .fold(1, num::integer::lcm)
}

pub struct Day08;

impl Solution for Day08 {
    fn parse(&self, input: &str) -> Result<()> {
        parse(input).map(|_| ())
    }

    fn part1(&self, input: &str) -> Result<String> {
        let data = parse(input)?;
        let res = part1(&data);
        Ok(res.to_string())
    }

    fn part2(&self, input: &str) -> Result<String> {
        let data = parse(input)?;
        let res = part2(&data);
        Ok(res.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_1() {
        let src = r#"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"#;

        let data = parse(src).unwrap();
        let res = part1(&data);
        assert_eq!(res, 2);
    }

    #[test]
    fn part1_2() {
        let src = r#"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"#;

        let data = parse(src).unwrap();
        let res = part1(&data);
        assert_eq!(res, 6);
    }

    #[test]
    fn part2_1() {
        let src = r#"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"#;

        let data = parse(src).unwrap();
        let res = part2(&data);
        assert_eq!(res, 6);
    }
}
