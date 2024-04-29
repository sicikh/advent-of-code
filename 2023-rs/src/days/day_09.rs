/*
 * Copyright (c) 2024 Kirill Lukashev <kirill.lukashev.sic@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
 * https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
 * <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
 * option. This file may not be copied, modified, or distributed
 * except according to those terms.
 */

use crate::prelude::*;

fn parse(input: &str) -> Result<Vec<Vec<i64>>> {
    fn parse_line(line: &str) -> Result<Vec<i64>> {
        Ok(line
            .split(' ')
            .map(str::parse::<i64>)
            .collect::<Result<_, _>>()?)
    }

    input.lines().map(parse_line).collect()
}

fn speeds(data: &[i64]) -> Vec<Vec<i64>> {
    fn speed(data: &[i64]) -> Vec<i64> {
        data.iter()
            .copied()
            .map_windows(|[lhs, rhs]| rhs - lhs)
            .collect()
    }

    let mut speeds = vec![data.to_vec()];

    for _ in 1..data.len() {
        let new_speed = speed(speeds.last().expect("Should not be empty"));
        if new_speed.iter().copied().all(|num| num == 0) {
            break;
        }
        speeds.push(new_speed);
    }

    speeds
}

fn part1(data: &[Vec<i64>]) -> i64 {
    fn extrapolate(data: &[i64]) -> i64 {
        let speeds = speeds(data);

        speeds
            .into_iter()
            .map(|v| v.last().copied().expect("Should not be empty"))
            .sum()
    }

    data.iter().map(|v| extrapolate(v.as_slice())).sum()
}

fn part2(data: &[Vec<i64>]) -> i64 {
    fn extrapolate_backwards(data: &[i64]) -> i64 {
        let speeds = speeds(data);

        speeds
            .into_iter()
            .rev()
            .map(|v| v.first().copied().expect("Should not be empty"))
            .fold(0, |acc, el| el - acc)
    }

    data.iter()
        .map(|v| extrapolate_backwards(v.as_slice()))
        .sum()
}

pub struct Day09;

impl Solution for Day09 {
    fn parse(&self, input: &str) -> Result<()> {
        parse(input).map(|_| ())
    }

    fn part1(&self, input: &str) -> Result<String> {
        let data = parse(input)?;
        Ok(part1(data.as_slice()).to_string())
    }

    fn part2(&self, input: &str) -> Result<String> {
        let data = parse(input)?;
        Ok(part2(data.as_slice()).to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_1() {
        let input = "0 3 6 9 12 15";
        let data = parse(input).unwrap();
        assert_eq!(part1(data.as_slice()), 18);
    }

    #[test]
    fn part1_2() {
        let input = "1 3 6 10 15 21";
        let data = parse(input).unwrap();
        assert_eq!(part1(data.as_slice()), 28);
    }

    #[test]
    fn part1_3() {
        let input = "10 13 16 21 30 45";
        let data = parse(input).unwrap();
        assert_eq!(part1(data.as_slice()), 68);
    }

    #[test]
    fn part2_1() {
        let input = "10 13 16 21 30 45";
        let data = parse(input).unwrap();
        assert_eq!(part2(data.as_slice()), 5);
    }
}
