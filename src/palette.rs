// SPDX-License-Identifier: MIT

use bevy::color::{Alpha, Color, Srgba};

const ALPHA: f32 = 0.7;
pub const CHANNELS: [&str; 3] = ["r", "g", "b"];

#[derive(bevy::prelude::Resource, Clone)]
pub struct ColorPalette {
    pub triangle: Color,
    pub rhombus: Color,
    pub pentagon: Color,
    pub hexagon: Color,
    pub player: Color,
    pub indicator: Color,
    pub glow: Color,
    pub projectile: Color,
    pub obstacle: Color,
    pub launcher: Color,
    pub background: Color,
    pub text: Color,
    pub outline: Color,
    pub highlight: Color,
    pub star: Color,
    pub dust_start: Color,
    pub dust_end: Color,
    pub explosion_start: Color,
    pub explosion_end: Color,
    pub text_next: Color,
    pub text_past: Color,
    pub text_future: Color,
}

impl Default for ColorPalette {
    fn default() -> Self {
        Self {
            triangle: Color::srgb_u8(255, 85, 85),
            rhombus: Color::srgb_u8(255, 121, 198),
            pentagon: Color::srgb_u8(189, 147, 249),
            hexagon: Color::srgb_u8(255, 184, 108),
            player: Color::srgb_u8(248, 248, 242),
            indicator: Color::srgb_u8(241, 250, 140),
            glow: Color::srgb_u8(139, 233, 253),
            projectile: Color::srgb_u8(139, 233, 253),
            obstacle: Color::srgb_u8(80, 250, 123),
            launcher: Color::srgb_u8(68, 71, 90),
            background: Color::srgb_u8(40, 42, 54),
            text: Color::srgb_u8(248, 248, 242),
            outline: Color::srgb_u8(98, 114, 164),
            highlight: Color::srgb_u8(248, 248, 242),
            star: Color::srgb_u8(98, 114, 164),
            dust_start: Color::srgb_u8(189, 147, 249),
            dust_end: Color::srgb_u8(139, 233, 253),
            explosion_start: Color::srgb_u8(255, 184, 108),
            explosion_end: Color::srgb_u8(255, 85, 85),
            text_next: Color::srgb_u8(139, 233, 253),
            text_past: Color::srgb_u8(40, 42, 54),
            text_future: Color::srgb_u8(248, 248, 242),
        }
    }
}

impl ColorPalette {
    pub fn shape_colors(&self) -> [Color; 4] {
        [self.triangle, self.rhombus, self.pentagon, self.hexagon]
    }
    pub fn foe_word_bg(&self) -> Color {
        self.background.with_alpha(ALPHA)
    }
    pub fn in_game_menu(&self) -> Color {
        self.background.with_alpha(ALPHA)
    }
    pub fn selected_outline(&self) -> Color {
        self.highlight
    }
    pub fn unselected_outline(&self) -> Color {
        self.outline
    }
    pub fn volume_bar(&self) -> Color {
        self.highlight
    }
    pub fn label(&self) -> Color {
        self.text
    }
    pub fn star_color(&self) -> Color {
        let c = Srgba::from(self.star);
        Color::srgba(c.red, c.green, c.blue, 0.4)
    }
    pub fn outline_mesh(&self) -> Color {
        let c = Srgba::from(self.outline);
        Color::srgba(c.red, c.green, c.blue, ALPHA)
    }
    pub fn explosion(&self) -> [Color; 2] {
        [self.explosion_start, self.explosion_end]
    }
    pub fn title_chars(&self) -> [Color; 8] {
        [
            Color::srgb_u8(80, 250, 123),
            Color::srgb_u8(255, 121, 198),
            Color::srgb_u8(255, 184, 108),
            Color::srgb_u8(255, 121, 198),
            Color::srgb_u8(241, 250, 140),
            Color::srgb_u8(189, 147, 249),
            Color::srgb_u8(139, 233, 253),
            Color::srgb_u8(255, 85, 85),
        ]
    }
    pub fn color_for_field(&self, field: ColorField) -> Color {
        match field {
            ColorField::Triangle => self.triangle,
            ColorField::Rhombus => self.rhombus,
            ColorField::Pentagon => self.pentagon,
            ColorField::Hexagon => self.hexagon,
            ColorField::Player => self.player,
            ColorField::Indicator => self.indicator,
            ColorField::Glow => self.glow,
            ColorField::Projectile => self.projectile,
            ColorField::Obstacle => self.obstacle,
            ColorField::Launcher => self.launcher,
            ColorField::Background => self.background,
            ColorField::Text => self.text,
            ColorField::Outline => self.outline,
            ColorField::Highlight => self.highlight,
            ColorField::Star => self.star,
            ColorField::DustStart => self.dust_start,
            ColorField::DustEnd => self.dust_end,
            ColorField::ExplosionStart => self.explosion_start,
            ColorField::ExplosionEnd => self.explosion_end,
            ColorField::FoeNext => self.text_next,
            ColorField::FoePast => self.text_past,
            ColorField::FoeFuture => self.text_future,
        }
    }
    pub fn set_color_for_field(&mut self, field: ColorField, color: Color) {
        match field {
            ColorField::Triangle => self.triangle = color,
            ColorField::Rhombus => self.rhombus = color,
            ColorField::Pentagon => self.pentagon = color,
            ColorField::Hexagon => self.hexagon = color,
            ColorField::Player => self.player = color,
            ColorField::Indicator => self.indicator = color,
            ColorField::Glow => self.glow = color,
            ColorField::Projectile => self.projectile = color,
            ColorField::Obstacle => self.obstacle = color,
            ColorField::Launcher => self.launcher = color,
            ColorField::Background => self.background = color,
            ColorField::Text => self.text = color,
            ColorField::Outline => self.outline = color,
            ColorField::Highlight => self.highlight = color,
            ColorField::Star => self.star = color,
            ColorField::DustStart => self.dust_start = color,
            ColorField::DustEnd => self.dust_end = color,
            ColorField::ExplosionStart => self.explosion_start = color,
            ColorField::ExplosionEnd => self.explosion_end = color,
            ColorField::FoeNext => self.text_next = color,
            ColorField::FoePast => self.text_past = color,
            ColorField::FoeFuture => self.text_future = color,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug, bevy::prelude::Component, bevy::prelude::Reflect)]
pub enum ColorField {
    Triangle,
    Rhombus,
    Pentagon,
    Hexagon,
    Player,
    Indicator,
    Glow,
    Projectile,
    Obstacle,
    Launcher,
    Background,
    Text,
    Outline,
    Highlight,
    Star,
    DustStart,
    DustEnd,
    ExplosionStart,
    ExplosionEnd,
    FoeNext,
    FoePast,
    FoeFuture,
}

impl ColorField {
    pub const ALL: [ColorField; 22] = [
        ColorField::Triangle,
        ColorField::Rhombus,
        ColorField::Pentagon,
        ColorField::Hexagon,
        ColorField::Player,
        ColorField::Indicator,
        ColorField::Glow,
        ColorField::Projectile,
        ColorField::Obstacle,
        ColorField::Launcher,
        ColorField::Background,
        ColorField::Text,
        ColorField::Outline,
        ColorField::Highlight,
        ColorField::Star,
        ColorField::DustStart,
        ColorField::DustEnd,
        ColorField::ExplosionStart,
        ColorField::ExplosionEnd,
        ColorField::FoeNext,
        ColorField::FoePast,
        ColorField::FoeFuture,
    ];

    pub fn index(self) -> usize {
        Self::ALL.iter().position(|f| *f == self).unwrap()
    }

    pub fn label(self) -> &'static str {
        match self {
            ColorField::Triangle => "triangle",
            ColorField::Rhombus => "rhombus",
            ColorField::Pentagon => "pentagon",
            ColorField::Hexagon => "hexagon",
            ColorField::Player => "player",
            ColorField::Indicator => "indicator",
            ColorField::Glow => "glow",
            ColorField::Projectile => "projectile",
            ColorField::Obstacle => "obstacle",
            ColorField::Launcher => "launcher",
            ColorField::Background => "background",
            ColorField::Text => "text",
            ColorField::Outline => "outline",
            ColorField::Highlight => "highlight",
            ColorField::Star => "star",
            ColorField::DustStart => "dust start",
            ColorField::DustEnd => "dust end",
            ColorField::ExplosionStart => "explosion start",
            ColorField::ExplosionEnd => "explosion end",
            ColorField::FoeNext => "text next",
            ColorField::FoePast => "text past",
            ColorField::FoeFuture => "text future",
        }
    }
}

pub fn channel_u8(color: Color, ch: usize) -> u8 {
    let c = color.to_srgba();
    let v = match ch {
        0 => c.red,
        1 => c.green,
        _ => c.blue,
    };
    (v * 255.).round() as u8
}

pub fn with_channel(color: Color, ch: usize, val: u8) -> Color {
    let mut c = color.to_srgba();
    let f = val as f32 / 255.;
    match ch {
        0 => c.red = f,
        1 => c.green = f,
        _ => c.blue = f,
    }
    Color::Srgba(c)
}
