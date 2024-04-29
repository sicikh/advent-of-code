/*
 * Copyright (c) 2024 Kirill Lukashev <kirill.lukashev.sic@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
 * https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
 * <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
 * option. This file may not be copied, modified, or distributed
 * except according to those terms.
 */

use std::path::PathBuf;

use reqwest::{header, Client};

use crate::prelude::*;

pub async fn get_input(day: usize) -> Result<String> {
    let path = PathBuf::from(format!("./inputs/day_{day}.txt"));

    if path.exists() {
        Ok(tokio::fs::read_to_string(path).await?)
    } else {
        let input = download_input(day).await?;
        tokio::fs::create_dir_all(path.parent().unwrap()).await?;
        tokio::fs::write(path, &input).await?;
        Ok(input)
    }
}

async fn download_input(day: usize) -> Result<String> {
    info!("Downloading input for day {}...", day);
    let mut request_headers = header::HeaderMap::new();
    request_headers.insert(
        header::COOKIE,
        header::HeaderValue::from_str(format!("session={}", get_cookie().await?).as_str())?,
    );

    let client = Client::builder()
        .default_headers(request_headers)
        .cookie_store(true)
        .build()?;
    let url = format!("https://adventofcode.com/2023/day/{}/input", day);
    let response = client.get(&url).send().await?;

    if response.status().is_success() {
        info!("Download for day {day} done!");
        Ok(response.text().await?)
    } else {
        error!("Failed to download input: {}", response.text().await?);
        Err(format_err!("Failed to download input"))
    }
}

async fn get_cookie() -> Result<String> {
    Ok(tokio::fs::read_to_string("cookie").await?)
}
