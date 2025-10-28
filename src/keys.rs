// SPDX-License-Identifier: MIT

#[rustfmt::skip]
pub const QWERTY_POOL: [char; 16] = [
    'y', 'u', 'i', 'o', 'p',
    'h', 'j', 'k', 'l', ';', '\'',
    'n', 'm', ',', '.', '/',
];

#[rustfmt::skip]
pub const DVORAK_POOL: [char; 16] = [
    'f', 'g', 'c', 'r', 'l',
    'd', 'h', 't', 'n', 's', '-',
    'b', 'm', 'w', 'v', 'z',
];

#[rustfmt::skip]
pub const COLEMAK_POOL: [char; 16] = [
    'j', 'l', 'u', 'y', ';',
    'h', 'n', 'e', 'i', 'o', '\'',
    'k', 'm', ',', '.', '/',
];
