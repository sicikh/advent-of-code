#![feature(iter_map_windows)]
/*
 * Copyright (c) 2024 Kirill Lukashev <kirill.lukashev.sic@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
 * https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
 * <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
 * option. This file may not be copied, modified, or distributed
 * except according to those terms.
 */
#![allow(dead_code, unused_imports, unused_variables)]

use std::collections::HashMap;

use clap::Parser;

use crate::cli::Cli;
use crate::days::*;
use crate::downloader::get_input;
use crate::prelude::*;

mod cli;
mod days;
mod downloader;
mod error;
mod prelude;
mod solution;

type Solutions = HashMap<usize, Box<dyn Solution>>;

fn solutions() -> Solutions {
    HashMap::from([
        (8, Box::new(Day08) as Box<dyn Solution>),
        (9, Box::new(Day09) as Box<dyn Solution>),
        (10, Box::new(Day10) as Box<dyn Solution>),
    ])
}

async fn solve(solutions: &Solutions, day: usize) -> Result<()> {
    if let Ok(input) = get_input(day).await {
        if let Some(solution) = solutions.get(&day) {
            let (part1, part2) = bench(solution.as_ref(), &input)?;
            info!("Day {day}. Part 1: {part1}, Part 2: {part2}");
        } else {
            error!("Solution for day {day} not found");
        }
    } else {
        error!("Failed to get input for day {day}");
    }

    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .without_time()
        .with_max_level(Level::INFO)
        .init();

    let cli = Cli::parse();
    let solutions = solutions();

    for day in cli.days {
        solve(&solutions, day).await?;
    }

    Ok(())
}

fn bench(solution: &dyn Solution, input: &str) -> Result<(String, String)> {
    let now = tokio::time::Instant::now();
    solution.parse(input)?;
    let parse_time = now.elapsed();
    info!("Parser took {:?}", parse_time);
    let now = tokio::time::Instant::now();
    let part1 = solution.part1(input)?;
    let part1_time = now.elapsed();
    info!("Part 1 took {:?}", part1_time);
    let now = tokio::time::Instant::now();
    let part2 = solution.part2(input)?;
    let part2_time = now.elapsed();
    info!("Part 2 took {:?}", part2_time);
    Ok((part1, part2))
}
