// SPDX-License-Identifier: MIT

#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

mod credits;
mod font_store;
mod game;
mod menu;
mod msg;
mod palette;
#[cfg(feature = "screenshot")]
mod screenshot;

pub use palette::{CHANNELS, ColorField, ColorPalette, channel_u8, with_channel};

use bevy::{
    asset::{RenderAssetUsages, embedded_asset},
    prelude::*,
    render::render_resource::AsBindGroup,
    settings::{
        ReflectSettingsGroup, SaveSettingsDeferred, SaveSettingsSync, SettingsGroup, SettingsPlugin,
    },
    shader::ShaderRef,
    sprite::update_text2d_layout,
    sprite_render::{AlphaMode2d, Material2d, Material2dPlugin},
    text::{detect_text_needs_rerender, load_font_assets_into_font_collection},
};
use rand::distr::weighted::WeightedIndex;
use std::{collections::HashMap, f32::consts::PI, sync::LazyLock};

const APP_QUALIFIER: &str = "com.github";
const APP_ORG: &str = "4imothy";

const SHOW_LOCAL_POINTS: bool = cfg!(feature = "show_local_points");
const INVINCIBLE: bool = cfg!(feature = "invincible");
const NO_OBSTACLES: bool = cfg!(feature = "no_obstacles");
const ONE_FOE: bool = cfg!(feature = "one_foe");
const MIN_DELAY: bool = cfg!(feature = "min_delay");
const TIME_MULTIPLIER: f32 = if cfg!(feature = "speedup") { 5. } else { 1. };

const PLAYER_RADIUS: f32 = 70.;
const PLAYER_HALF_ANGLE: f32 = PI / 6.;
const PLAYER_ENGINE_WIDTH: f32 = PLAYER_RADIUS / 2.5;
const PLAYER_ENGINE_LENGTH: f32 = PLAYER_ENGINE_WIDTH / 2.;
const PLAYER_ENGINE_GLOW_RADIUS: f32 = PLAYER_ENGINE_WIDTH * 0.4;
const PLAYER_ENGINE_OFFSET_X: f32 = PLAYER_RADIUS * 0.3;
const PLAYER_ENGINE_OFFSET_Y: f32 = PLAYER_RADIUS * 0.5;
const PLAYER_ENGINE_ANGLE: f32 = PI / 9.;
const FOE_SIZE: f32 = PLAYER_RADIUS * 1.4;
const OBSTACLE_RADIUS: f32 = PLAYER_RADIUS / 3.;
const PROJECTILE_RADIUS: f32 = PLAYER_RADIUS / 10.;
const INDICATOR_THICKNESS: f32 = PLAYER_RADIUS / 15.;
const INDICATOR_RADIUS: f32 = 1.5 * PLAYER_RADIUS;
const INDICATOR_LONG_RECTANGLE_LENGTH: f32 = INDICATOR_THICKNESS * 3.;
const EXPLOSION_PARTICLE_RADIUS_LOWER: f32 = PLAYER_RADIUS / 10.;
const EXPLOSION_PARTICLE_RADIUS_UPPER: f32 = PLAYER_RADIUS / 5.;
const PLAYER_RING_RADIUS: f32 = PLAYER_RADIUS * 1.2;
const PLAYER_RING_THICKNESS: f32 = INDICATOR_THICKNESS / 1.5;
const PLAYER_NOTCH_RADIUS: f32 = PLAYER_RADIUS * 0.15;
const SELECTION_ARROW_RADIUS: f32 = PLAYER_RING_RADIUS * 1.5;
const SELECTION_ARROW_HALF_HEIGHT: f32 = PLAYER_NOTCH_RADIUS * 1.2;
const SELECTION_ARROW_HALF_WIDTH: f32 = PLAYER_NOTCH_RADIUS * 1.5;
const HEXAGON_LAUNCHER_LENGTH: f32 = FOE_SIZE * 1.5;
const HEXAGON_LAUNCHER_WIDTH: f32 = OBSTACLE_RADIUS * 2.;
const PENTAGON_LAUNCHER_LENGTH: f32 = FOE_SIZE * 1.2;
const PENTAGON_LAUNCHER_WIDTH: f32 = FOE_SIZE;

const PLAYER_MOVEMENT_SPEED: f32 = 400.;
const PLAYER_ROTATION_SPEED: f32 = 4.;
const PLAYER_DRAG: f32 = 5.0;
const PLAYER_THRUST: f32 = PLAYER_DRAG * PLAYER_MOVEMENT_SPEED;
const PLAYER_STOP_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 80.;
const AUDIO_PADDING: f32 = FOE_SIZE * 2.;
const PROJECTILE_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED * 4.;
const FOE_SEPARATION_RADIUS: f32 = FOE_SIZE * 3.;
const FOE_SEPARATION_WEIGHT: f32 = 1.5;
const FOE_SPAWN_RETRY: usize = 5;

const SPAWN_MARGIN: f32 = FOE_SIZE * 2.0;
const NUM_SHAPES: usize = 4;
const FOE_MAX_SIDES: usize = 6;
const SHAPES: [Shape; NUM_SHAPES] = [
    Shape::Triangle,
    Shape::Rhombus,
    Shape::Pentagon,
    Shape::Hexagon,
];

const FIRST_FOE_SPAWN_DELAY: f32 = 0.;
const SPAWN_DELTA: f32 = 0.3;
const SPAWN_DELAY_DECAY: f32 = 0.09;
const THREAT_TARGET_BASE: f32 = 4.;
const THREAT_TARGET_MAX: f32 = 15.;
const THREAT_TARGET_RAMP: f32 = 0.20;
const THREAT_LULL_TIME: f32 = 3.;
const NUM_PHASE: usize = NUM_SHAPES + 1;
const FOES_PER_PHASE: f32 = 4.;
const PHASE_WEIGHTS: [[f32; NUM_SHAPES]; NUM_PHASE] = [
    [0.50, 0.50, 0.00, 0.00],
    [0.35, 0.50, 0.15, 0.00],
    [0.20, 0.45, 0.25, 0.10],
    [0.10, 0.45, 0.30, 0.15],
    [0.10, 0.40, 0.30, 0.20],
];
const SHAPE_NUM_KEYS: [usize; NUM_SHAPES] = [3, 4, 5, 6];
const SHAPE_MOV_SPEEDS: [f32; NUM_SHAPES] = [
    PLAYER_MOVEMENT_SPEED / 6.,
    PLAYER_MOVEMENT_SPEED / 8.,
    PLAYER_MOVEMENT_SPEED / 10.,
    PLAYER_MOVEMENT_SPEED / 12.,
];
const SHAPE_ROT_SPEEDS: [f32; NUM_SHAPES] = [
    PLAYER_ROTATION_SPEED / 6.,
    PLAYER_ROTATION_SPEED / 8.,
    PLAYER_ROTATION_SPEED / 10.,
    PLAYER_ROTATION_SPEED / 12.,
];
const HEXAGON_LAUNCH_DELAY: f32 = 8.;
const HEXAGON_TARGET_MIN: f32 = 0.8;
const PENTAGON_SPAWN_DELAY: f32 = 7.;
const OBSTACLE_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 3.;
const OBSTACLE_MIN_SPEED: f32 = OBSTACLE_MOVEMENT_SPEED / 4.;
const KNOCKBACK_DECAY: f32 = 5.;
const KNOCKBACK_MULTIPLIER: f32 = 1.5;
const KNOCKBACK_STOP_SPEED: f32 = 15.;
const FUEL_DRAIN_RATE: f32 = 0.45;
const FUEL_PER_KEY: f32 = 0.01;
const FUEL_PASSIVE_RATE: f32 = 0.01;

const FOE_MAX_NUM_KEYS: usize = *SHAPE_NUM_KEYS.last().unwrap();
const FOE_FONT_SIZE: f32 = PLAYER_RADIUS * 0.7;
const FOE_AVOID_COUNT: usize = 10;
const WORD_AVOID_RETRIES: usize = 10;
const PENTAGON_SUMMON_WEIGHTS: [f32; 1] = [1.0];
const PROJECTILE_INC_TIME: f32 = 0.1;
const SUMMONER_COLLISION_PADDING: f32 = PLAYER_RADIUS / 2.;
const SUMMONER_ORBIT_RADIUS: f32 = PLAYER_RADIUS * 7.;
const COS_MIN_LEADING_VERTEX_ALIGNMENT: f32 = 0.3;
const COUNTDOWN_START_SCALE: f32 = 1.5;
const COUNTDOWN_THICKNESS: f32 = INDICATOR_THICKNESS / 2.;
const COUNTDOWN_Z_OFFSET: f32 = -0.5;
const LAUNCH_RECOIL_SPEED: f32 = FOE_SIZE * 1.5;
const LAUNCHER_ARM_RECOIL_DURATION: f32 = 0.25;
const LAUNCHER_ARM_RECOIL_MIN_SCALE: f32 = 0.3;

const RING_RESOLUTIONS: u32 = 128;

const STAR_Z_INDEX: f32 = -1.;

const TRIANGLE_LOCAL_POINTS: [Vec3; 3] = [
    Vec3::new(0., FOE_SIZE / 1.75, 0.),
    Vec3::new(FOE_SIZE / 2., -FOE_SIZE / 2., 0.),
    Vec3::new(-FOE_SIZE / 2., -FOE_SIZE / 2., 0.),
];
const TRIANGLE: Triangle2d = Triangle2d {
    vertices: [
        Vec2::new(TRIANGLE_LOCAL_POINTS[0].x, TRIANGLE_LOCAL_POINTS[0].y),
        Vec2::new(TRIANGLE_LOCAL_POINTS[1].x, TRIANGLE_LOCAL_POINTS[1].y),
        Vec2::new(TRIANGLE_LOCAL_POINTS[2].x, TRIANGLE_LOCAL_POINTS[2].y),
    ],
};
const RHOMBUS_LOCAL_POINTS: [Vec3; 4] = [
    Vec3::new(0., -FOE_SIZE, 0.),
    Vec3::new(FOE_SIZE / 1.5, 0., 0.),
    Vec3::new(0., FOE_SIZE, 0.),
    Vec3::new(-FOE_SIZE / 1.5, 0., 0.),
];
const RHOMBUS: Rhombus = Rhombus {
    half_diagonals: Vec2::new(RHOMBUS_LOCAL_POINTS[1].x, RHOMBUS_LOCAL_POINTS[2].y),
};
static PENTAGON_LOCAL_POINTS: LazyLock<[Vec3; 5]> = LazyLock::new(|| {
    let verts = PENTAGON.vertices(0.);
    let mut points = [Vec3::ZERO; 5];
    for (i, v) in verts.into_iter().enumerate() {
        points[i] = v.extend(0.);
    }
    points
});
static TRIANGLE_VERTEX_DIRS: LazyLock<[Vec2; 1]> =
    LazyLock::new(|| [TRIANGLE_LOCAL_POINTS[0].xy().normalize()]);
static RHOMBUS_LEADING_DIRS: LazyLock<[Vec2; 2]> = LazyLock::new(|| {
    [
        RHOMBUS_LOCAL_POINTS[0].xy().normalize(),
        RHOMBUS_LOCAL_POINTS[2].xy().normalize(),
    ]
});
static PENTAGON_VERTEX_DIRS: LazyLock<[Vec2; 5]> =
    LazyLock::new(|| PENTAGON_LOCAL_POINTS.map(|p| p.xy().normalize()));
static HEXAGON_VERTEX_DIRS: LazyLock<[Vec2; 6]> =
    LazyLock::new(|| HEXAGON_LOCAL_POINTS.map(|p| p.xy().normalize()));
const PENTAGON: RegularPolygon = RegularPolygon {
    circumcircle: Circle { radius: FOE_SIZE },
    sides: 5,
};
static HEXAGON_LOCAL_POINTS: LazyLock<[Vec3; 6]> = LazyLock::new(|| {
    let verts = HEXAGON.vertices(0.);
    let mut points = [Vec3::ZERO; 6];
    for (i, v) in verts.into_iter().enumerate() {
        points[i] = v.extend(0.);
    }
    points
});
const HEXAGON: RegularPolygon = RegularPolygon {
    circumcircle: Circle { radius: FOE_SIZE },
    sides: 6,
};
const PLAYER_LOCAL_TRIANGLE: [Vec3; 3] = [
    Vec3::new(0., -PLAYER_RADIUS / 2., 0.),
    Vec3::new(PLAYER_RADIUS / 2., PLAYER_RADIUS / 2., 0.),
    Vec3::new(-PLAYER_RADIUS / 2., PLAYER_RADIUS / 2., 0.),
];

const WORD_LIST_LEN: usize = 2000;
static WORD_LIST_3: [&str; WORD_LIST_LEN] = include!("../assets/words_3.rs");
static WORD_LIST_4: [&str; WORD_LIST_LEN] = include!("../assets/words_4.rs");
static WORD_LIST_5: [&str; WORD_LIST_LEN] = include!("../assets/words_5.rs");
static WORD_LIST_6: [&str; WORD_LIST_LEN] = include!("../assets/words_6.rs");

pub fn hexagon_target_offset(from: Vec2, player_pos: Vec2, viewport_width: f32, frac: f32) -> Vec2 {
    let half_w = viewport_width / 2. - FOE_SIZE;
    let half_h = VIEWPORT_HEIGHT / 2. - FOE_SIZE;
    let dir = (from - player_pos).normalize_or(Vec2::X);
    let t_max = (half_w / dir.x.abs()).min(half_h / dir.y.abs());
    dir * (t_max * frac)
}

fn spawner_foe_delay_mu(progress: f32, max_dif: bool) -> f32 {
    if MIN_DELAY {
        SPAWN_DELTA
    } else {
        2. * f32::exp(-SPAWN_DELAY_DECAY * if max_dif { f32::MAX } else { progress }) + 1.
    }
}

fn threat_target(progress: f32, max_dif: bool) -> f32 {
    if max_dif {
        return THREAT_TARGET_MAX;
    }
    (THREAT_TARGET_BASE + progress * THREAT_TARGET_RAMP).min(THREAT_TARGET_MAX)
}

fn foe_weights(progress: f32, max_dif: bool) -> [f32; NUM_SHAPES] {
    if max_dif || MIN_DELAY {
        return *PHASE_WEIGHTS.last().unwrap();
    }
    let phase: usize = ((progress / FOES_PER_PHASE) as usize).min(PHASE_WEIGHTS.len() - 1);
    PHASE_WEIGHTS[phase]
}

const EXPLOSION_Z_INDEX: f32 = 0.;
const EXPLOSION_Z_INDEX_RANGE: f32 = 0.1;
const OBSTACLE_Z_INDEX: f32 = 1.;
const TRACKING_Z_INDEX: f32 = 2.;
const SUMMONER_Z_INDEX: f32 = 3.;
const HEXAGON_Z_INDEX: f32 = 4.;
const PLAYER_Z_INDEX: f32 = 6.;
const INDICATOR_Z_INDEX: f32 = 7.;
const VIEWPORT_HEIGHT: f32 = 1500.;

const SCORE_TEXT_PADDING: f32 = 10.;

const EXPLOSION_PARTICLE_MAX_LIFETIME: f32 = 1.;
const EXPLOSION_PARTICLE_INITIAL_ALPHA: f32 = 0.7;

const SELECT_PULSE_LIFETIME: f32 = 0.35;
const SELECT_PULSE_END_SCALE: f32 = 1.8;
const SELECT_PULSE_INITIAL_ALPHA: f32 = 0.9;

const GAME_OVER_SLOWDOWN_REAL_TIME: f32 = 3.;
const TIME_BEFORE_RESUME: f32 = 3.;

const PRESS_REPEAT_DELAY: f32 = 0.4;
const PRESS_REPEAT_INTERVAL: f32 = 0.1;

#[derive(Resource, SettingsGroup, Reflect)]
#[reflect(Resource, SettingsGroup, Default)]
struct Config {
    high_score: usize,
    volume: u8,
    up: KeyCode,
    down: KeyCode,
    left: KeyCode,
    right: KeyCode,
    select: KeyCode,
    back: KeyCode,
    deselect: KeyCode,
    rotate_left: KeyCode,
    rotate_right: KeyCode,
    max_difficulty: bool,
    colors: Vec<(ColorField, [u8; 3])>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            high_score: 0,
            volume: 100,
            up: KeyCode::KeyW,
            down: KeyCode::KeyS,
            left: KeyCode::KeyA,
            right: KeyCode::KeyD,
            select: KeyCode::Space,
            back: KeyCode::Escape,
            deselect: KeyCode::Space,
            rotate_left: KeyCode::ShiftLeft,
            rotate_right: KeyCode::ShiftRight,
            max_difficulty: false,
            colors: Vec::new(),
        }
    }
}

impl Config {
    pub fn build_palette(&self) -> ColorPalette {
        let mut p = ColorPalette::default();
        for &(field, rgb) in &self.colors {
            p.set_color_for_field(field, Color::srgb_u8(rgb[0], rgb[1], rgb[2]));
        }
        p
    }

    pub fn set_color_field(&mut self, field: ColorField, color: Color) {
        let rgb = [
            channel_u8(color, 0),
            channel_u8(color, 1),
            channel_u8(color, 2),
        ];
        if let Some(entry) = self.colors.iter_mut().find(|(f, _)| *f == field) {
            entry.1 = rgb;
        } else {
            self.colors.push((field, rgb));
        }
    }

    pub fn reset_colors(&mut self) {
        self.colors.clear();
    }

    pub fn engine_firing(&self, keys: &ButtonInput<KeyCode>) -> bool {
        self.left_thruster(keys) || self.right_thruster(keys)
    }

    pub fn left_thruster(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_pressed([self.rotate_left, KeyCode::ArrowLeft])
    }

    pub fn right_thruster(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_pressed([self.rotate_right, KeyCode::ArrowRight])
    }

    pub fn nav_up(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.up, KeyCode::ArrowUp])
    }

    pub fn nav_down(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.down, KeyCode::ArrowDown])
    }

    pub fn nav_left(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.left, KeyCode::ArrowLeft])
    }

    pub fn nav_right(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.right, KeyCode::ArrowRight])
    }

    pub fn nav_states(&self, keys: &ButtonInput<KeyCode>) -> ([bool; 4], [bool; 4]) {
        let dirs = [
            (self.up, KeyCode::ArrowUp),
            (self.down, KeyCode::ArrowDown),
            (self.left, KeyCode::ArrowLeft),
            (self.right, KeyCode::ArrowRight),
        ];
        (
            dirs.map(|(c, a)| keys.any_pressed([c, a])),
            dirs.map(|(c, a)| keys.any_just_pressed([c, a])),
        )
    }

    pub fn select_pressed(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.select, KeyCode::Enter])
    }

    pub fn back_pressed(&self, keys: &ButtonInput<KeyCode>) -> bool {
        keys.any_just_pressed([self.back, KeyCode::Escape])
    }

    pub fn is_deselect(&self, code: KeyCode) -> bool {
        code == self.deselect || code == KeyCode::Backspace
    }
}

fn main() {
    let mut app = App::new();
    app.add_plugins((
        SettingsPlugin::new(&format!(
            "{APP_QUALIFIER}.{APP_ORG}.{}",
            env!("CARGO_PKG_NAME")
        )),
        DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: env!("CARGO_PKG_NAME").to_string(),
                ..default()
            }),
            ..default()
        }),
        Material2dPlugin::<StarfieldMaterial>::default(),
        menu_plugin,
        game_plugin,
    ))
    .add_message::<PauseMsg>()
    .add_systems(
        Startup,
        (
            setup,
            handle_font_action,
            load_font_assets_into_font_collection,
        )
            .chain(),
    )
    .add_systems(Update, handle_font_action)
    .add_systems(Update, sync_starfield)
    .add_systems(Update, auto_save_settings)
    .add_systems(Last, flush_settings_on_exit)
    .init_state::<Screen>()
    .insert_resource(ColorPalette::default())
    .insert_resource(ClearColor(ColorPalette::default().background))
    .insert_resource(AppFont(Handle::default()));
    #[cfg(feature = "screenshot")]
    app.add_plugins(screenshot::plugin);
    #[cfg(not(feature = "screenshot"))]
    app.add_systems(
        Startup,
        (|mut next: ResMut<NextState<Screen>>| next.set(Screen::MainMenu)).after(setup),
    );
    #[cfg(not(target_arch = "wasm32"))]
    app.add_systems(
        Update,
        menu::handle_file_drop.run_if(in_state(Screen::Settings)),
    );
    embedded_asset!(app, "starfield.wgsl");
    font_store::init();
    app.run();
}

fn setup(
    mut commands: Commands,
    config: Res<Config>,
    mut global_volume: ResMut<GlobalVolume>,
    mut fonts: ResMut<Assets<Font>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut star_materials: ResMut<Assets<StarfieldMaterial>>,
    mut app_font: ResMut<AppFont>,
    mut palette: ResMut<ColorPalette>,
) {
    *palette = config.build_palette();
    let font = Font::from_bytes(include_bytes!("../assets/font.ttf").to_vec());
    app_font.0 = fonts.add(font);
    commands.spawn((
        Camera2d,
        Projection::from(OrthographicProjection {
            scaling_mode: bevy::camera::ScalingMode::FixedVertical {
                viewport_height: VIEWPORT_HEIGHT,
            },
            ..OrthographicProjection::default_2d()
        }),
        Transform::from_xyz(0., PLAYER_RADIUS, 0.),
    ));
    global_volume.volume = bevy::audio::Volume::Linear(config.volume as f32 / 100.);
    let (color, bg, dust_a, dust_b) = starfield_colors(&palette);
    let mat = star_materials.add(StarfieldMaterial {
        seed: UVec4::new(rand::random::<u32>(), 0, 0, 0),
        color,
        bg,
        dust_a,
        dust_b,
        view: Vec4::ZERO,
    });
    commands.spawn((
        StarQuad,
        Mesh2d(meshes.add(Rectangle::new(1., 1.))),
        MeshMaterial2d(mat),
        Transform::from_xyz(0., PLAYER_RADIUS, STAR_Z_INDEX),
    ));
}

fn sync_starfield(
    palette: Res<ColorPalette>,
    mut star_mats: ResMut<Assets<StarfieldMaterial>>,
    star: Query<&MeshMaterial2d<StarfieldMaterial>, With<StarQuad>>,
    mut clear_color: ResMut<ClearColor>,
) {
    if !palette.is_changed() {
        return;
    }
    clear_color.0 = palette.background;
    let Ok(handle) = star.single() else { return };
    let Some(mut mat) = star_mats.get_mut(&handle.0) else {
        return;
    };
    (mat.color, mat.bg, mat.dust_a, mat.dust_b) = starfield_colors(&palette);
}

fn starfield_colors(palette: &ColorPalette) -> (Vec4, Vec4, Vec4, Vec4) {
    let star = LinearRgba::from(palette.star_color());
    let bg = LinearRgba::from(palette.background);
    let da = LinearRgba::from(palette.dust_start);
    let db = LinearRgba::from(palette.dust_end);
    (
        Vec4::new(star.red, star.green, star.blue, star.alpha),
        Vec4::new(bg.red, bg.green, bg.blue, 0.0),
        Vec4::new(da.red, da.green, da.blue, 0.0),
        Vec4::new(db.red, db.green, db.blue, 0.0),
    )
}

fn handle_font_action(
    mut fonts: ResMut<Assets<Font>>,
    mut app_font: ResMut<AppFont>,
    mut text_fonts: Query<&mut TextFont>,
) {
    let Some(bytes) = font_store::take_pending() else {
        return;
    };
    let handle = fonts.add(Font::from_bytes(bytes));
    app_font.0 = handle.clone();
    for mut tf in &mut text_fonts {
        tf.font = FontSource::Handle(handle.clone());
    }
}

fn auto_save_settings(config: Res<Config>, drag: Res<VolumeDrag>, mut commands: Commands) {
    if !config.is_changed() || config.is_added() {
        return;
    }
    if drag.0 {
        commands.queue(SaveSettingsDeferred::default());
    } else {
        commands.queue(SaveSettingsSync::IfChanged);
    }
}

fn flush_settings_on_exit(mut exit: MessageReader<AppExit>, mut commands: Commands) {
    if !exit.is_empty() {
        exit.clear();
        commands.queue(SaveSettingsSync::IfChanged);
    }
}

fn menu_plugin(app: &mut App) {
    #[cfg(target_arch = "wasm32")]
    menu::setup_web_drag_drop();
    app.add_systems(OnExit(Screen::MainMenu), menu::despawn_screen::<MenuScreen>)
        .add_systems(OnEnter(Screen::MainMenu), menu::menu_setup)
        .add_systems(
            OnEnter(Screen::Settings),
            (menu::settings_setup, || font_store::set_settings_open(true)),
        )
        .add_systems(
            OnExit(Screen::Settings),
            (menu::despawn_screen::<SettingsScreen>, || {
                font_store::set_settings_open(false)
            }),
        )
        .add_systems(OnEnter(Screen::Help), menu::help_setup)
        .add_systems(OnExit(Screen::Help), menu::despawn_screen::<HelpScreen>)
        .add_systems(OnEnter(Screen::Credits), menu::credits_setup)
        .add_systems(
            OnExit(Screen::Credits),
            menu::despawn_screen::<CreditsScreen>,
        )
        .add_systems(OnEnter(Screen::ColorSettings), menu::color_settings_setup)
        .add_systems(
            OnExit(Screen::ColorSettings),
            menu::despawn_screen::<ColorSettingsScreen>,
        )
        .add_systems(OnEnter(Screen::Keybinds), menu::keybinds_setup)
        .add_systems(
            OnExit(Screen::Keybinds),
            (
                menu::despawn_screen::<KeybindsScreen>,
                |mut rebinding: ResMut<menu::Rebinding>| rebinding.kind = None,
            ),
        )
        .add_systems(
            Update,
            (
                menu::mouse,
                menu::on_selection,
                menu::grid_navigate,
                menu::grid_action,
            )
                .run_if(in_menu),
        )
        .add_systems(
            Update,
            (
                menu::sync_swatches,
                menu::sync_cursor_style,
                menu::color_click,
                menu::commit_on_focus_change,
                menu::color_input_borders,
            )
                .run_if(in_state(Screen::ColorSettings)),
        )
        .add_systems(
            Update,
            (
                menu::volume_drag_control,
                menu::volume_start_drag,
                menu::update_volume_bar,
                menu::sync_settings_colors,
            )
                .run_if(in_state(Screen::Settings).or_else(in_state(GameScreen::Settings))),
        )
        .add_systems(
            Update,
            menu::update_drop_zone.run_if(in_state(Screen::Settings)),
        )
        .add_systems(
            Update,
            menu::sync_text_color
                .run_if(in_state(Screen::Settings).or_else(in_state(Screen::ColorSettings))),
        )
        .add_systems(
            Update,
            (
                menu::capture_rebind
                    .after(menu::grid_action)
                    .after(menu::mouse),
                menu::sync_rebind_text,
            )
                .run_if(in_state(Screen::Keybinds)),
        )
        .insert_resource(menu::Rebinding::default())
        .insert_resource(VolumeDrag(false))
        .insert_resource(KeyState::default())
        .add_systems(Update, update_key_state);
}

fn update_key_state(
    keys: Res<ButtonInput<KeyCode>>,
    config: Res<Config>,
    mut key: ResMut<KeyState>,
    rtime: Res<Time<Real>>,
) {
    let (held, just) = config.nav_states(&keys);
    key.update(
        held[0],
        held[1],
        held[2],
        held[3],
        just[0],
        just[1],
        just[2],
        just[3],
        rtime.delta_secs(),
    );
}

fn in_menu(state: Res<State<Screen>>, game_state: Res<State<GameScreen>>) -> bool {
    matches!(
        state.get(),
        Screen::MainMenu
            | Screen::Credits
            | Screen::Settings
            | Screen::Help
            | Screen::ColorSettings
            | Screen::Keybinds,
    ) || matches!(
        game_state.get(),
        GameScreen::Pause | GameScreen::End | GameScreen::Settings
    )
}

fn back_just_pressed(keys: Res<ButtonInput<KeyCode>>, config: Res<Config>) -> bool {
    config.back_pressed(&keys)
}

fn game_plugin(app: &mut App) {
    let exit = (
        game::default_state,
        despawn::<Player>,
        despawn::<Foe>,
        despawn::<Obstacle>,
        despawn::<ExplosionParticle>,
        despawn::<Projectile>,
        despawn::<Indicator>,
        despawn::<Slowdown>,
        despawn::<Stats>,
    );
    app.add_systems(OnEnter(Screen::Game), game::setup)
        .add_systems(OnExit(Screen::Game), exit)
        .insert_resource(FoePointsCache(HashMap::new()))
        .insert_resource(Time::<Fixed>::from_hz(60.))
        .init_state::<GameScreen>()
        .add_message::<GameMsg>()
        .add_message::<AudioMsg>()
        .add_systems(OnEnter(GameScreen::Pause), menu::pause_setup)
        .add_systems(
            OnExit(GameScreen::Pause),
            menu::despawn_screen::<PauseScreen>,
        )
        .add_systems(OnEnter(GameScreen::Settings), menu::game_settings_setup)
        .add_systems(
            OnExit(GameScreen::Settings),
            menu::despawn_screen::<GameSettingsScreen>,
        )
        .add_systems(OnEnter(GameScreen::End), menu::end_setup)
        .add_systems(OnExit(GameScreen::End), menu::despawn_screen::<EndScreen>)
        .add_systems(
            OnEnter(GameScreen::ResumeCountdown),
            menu::resume_countdown_setup,
        )
        .add_systems(
            OnExit(GameScreen::ResumeCountdown),
            menu::despawn_screen::<ResumeCountdown>,
        )
        .add_systems(
            FixedUpdate,
            (
                game::foe_points,
                (
                    game::foe_collisions,
                    game::update_spawned_relations,
                    game::player_collisions,
                    game::obstacle_collisions,
                )
                    .after(game::foe_points),
                game::spawner,
                game::update_clock,
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(
            Update,
            (
                game::player_movement,
                game::camera_follow.after(game::player_movement),
                game::track_selected_foe.after(game::player_movement),
                game::keypress,
                game::slowdown_time,
                game::projectile,
                game::tracking_foe,
                game::summoner_foe,
                game::launcher_foe,
                game::foe_launcher,
                game::track_player_for_foe_launcher,
                game::summoner,
                game::obstacle,
                game::explosion_system,
                game::select_pulse_system,
                game::update_countdown_indicators,
                game::update_launcher_ring,
                game::fuel_charge,
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(Update, game::update_stars.after(game::camera_follow))
        .add_systems(
            Update,
            menu::resume_countdown.run_if(in_state(GameScreen::ResumeCountdown)),
        )
        .add_systems(
            Update,
            toggle_pause.run_if(back_just_pressed.and_then(in_state(Screen::Game))),
        )
        .add_systems(
            PostUpdate,
            (
                msg::on_msg,
                msg::on_audio,
                game::reset_collisions.after(msg::on_msg),
                game::despawner.after(msg::on_msg),
                game::sync_foe_text
                    .after(msg::on_msg)
                    .before(detect_text_needs_rerender),
                game::lock_foe_text
                    .after(msg::on_msg)
                    .after(update_text2d_layout)
                    .before(TransformSystems::Propagate),
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(PostUpdate, msg::on_toggle_pause);
}

#[derive(Resource)]
struct FoePointsCache(HashMap<Entity, FoePoints>);

struct FoePoints {
    data: [Vec3; FOE_MAX_SIDES],
    len: usize,
}

#[derive(Component)]
struct SettingsScreen;

#[derive(Component)]
struct HelpScreen;

#[derive(Component)]
struct PauseScreen;

#[derive(Component)]
struct GameSettingsScreen;

#[derive(Component)]
struct EndScreen;

#[derive(Component)]
struct CreditsScreen;

#[derive(Component)]
struct MenuScreen;

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug, Hash, States)]
enum Screen {
    #[default]
    Loading,
    MainMenu,
    Game,
    Settings,
    Help,
    Credits,
    ColorSettings,
    Keybinds,
}

#[derive(Component)]
struct KeybindsScreen;

#[derive(Component)]
struct ColorSettingsScreen;

#[derive(Component)]
pub struct ColorSwatch;

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug, Hash, States)]
enum GameScreen {
    #[default]
    Running,
    ResumeCountdown,
    Pause,
    Settings,
    End,
}

#[derive(Component)]
struct Player {
    selected: Option<Entity>,
    selection_active: bool,
    velocity: Vec2,
}

#[derive(Message)]
enum GameMsg {
    Explosion(Vec2),
    Despawn(Entity),
    SpawnFoe(Shape, Vec2, Option<Entity>, Option<f32>),
    SpawnObstacle(Vec2, Vec2),
    Invisible(Entity),
    Visible(Entity),
    Select(Entity),
    SelectPulse(Vec2),
    DeSelect(Entity),
    Projectile(Entity, Vec3),
    GameEnd,
}

#[derive(Message)]
enum PauseMsg {
    TogglePause,
}

#[derive(Resource)]
struct Spawner {
    foe_dist: WeightedIndex<f32>,
    foe_delay: f32,
    foe_delay_mu: f32,
    next_side: usize,
    lull: f32,
}

#[derive(Component)]
struct Summoner {
    since: f32,
    delay: f32,
    foe_dist: WeightedIndex<f32>,
}

#[derive(Clone, Copy)]
enum Shape {
    Triangle,
    Rhombus,
    Pentagon,
    Hexagon,
}

impl Shape {
    fn name(self) -> &'static str {
        match self {
            Shape::Triangle => "triangle",
            Shape::Rhombus => "rhombus",
            Shape::Pentagon => "pentagon",
            Shape::Hexagon => "hexagon",
        }
    }
    fn vertex_dirs(self) -> &'static [Vec2] {
        match self {
            Shape::Triangle => &*TRIANGLE_VERTEX_DIRS,
            Shape::Rhombus => &*RHOMBUS_LEADING_DIRS,
            Shape::Pentagon => &*PENTAGON_VERTEX_DIRS,
            Shape::Hexagon => &*HEXAGON_VERTEX_DIRS,
        }
    }
}

#[derive(Component)]
struct Tracking;

#[derive(Component)]
struct Launcher {
    since: f32,
    delay: f32,
    target_offset: Vec2,
    target_frac: f32,
}

#[derive(Component)]
struct FoeLauncher {
    recoil: f32,
}

#[derive(Component)]
struct CountdownIndicator;

#[derive(Component)]
struct FoeText;

#[derive(Component)]
struct FoeTextBg;

#[derive(Component)]
struct Targeted;

#[derive(Component)]
struct ToDespawn;

#[derive(Component)]
struct Indicator;

#[derive(Component)]
struct SelectionArrowPivot;

#[derive(Component)]
struct PlayerLauncher;

#[derive(Component)]
struct EngineGlow {
    left: bool,
}

#[derive(Component)]
struct LauncherNotch;

#[derive(Component)]
struct LocalPoint;

#[derive(Component)]
struct Slowdown {
    time: f32,
}

#[derive(Component)]
struct LauncherRing;

#[derive(Component)]
struct Obstacle {
    velocity: Vec2,
    colliding: bool,
}

#[derive(Resource)]
struct AudioAssets {
    pub unmatched_keypress: Handle<AudioSource>,
    pub explosion: Handle<AudioSource>,
    pub foe_launch: Handle<AudioSource>,
    pub end: Handle<AudioSource>,
}

#[derive(Resource)]
struct ShapeAssets {
    meshes: [Handle<Mesh>; NUM_SHAPES],
    materials: [Handle<ColorMaterial>; NUM_SHAPES],
}

#[derive(Component)]
pub struct DropZone;

#[derive(Resource, Clone)]
pub struct AppFont(pub Handle<Font>);

#[derive(Resource)]
pub struct ExplosionAssets {
    pub meshes: [Handle<Mesh>; 5],
}

pub const SMALL_FONT_SIZE: f32 = 25.;

fn make_font(font: &Handle<Font>, size: f32) -> TextFont {
    TextFont::from_font_size(FontSize::Px(size)).with_font(font.clone())
}

pub fn text_font(font: &Handle<Font>) -> TextFont {
    make_font(font, 30.)
}

pub fn foe_font(font: &Handle<Font>) -> TextFont {
    make_font(font, FOE_FONT_SIZE)
}

pub fn small_font(font: &Handle<Font>) -> TextFont {
    make_font(font, SMALL_FONT_SIZE)
}

pub fn title_font(font: &Handle<Font>) -> TextFont {
    make_font(font, 80.)
}

#[derive(Message)]
pub enum AudioMsg {
    Explosion,
    UnmatchedKeypress,
    FoeLaunch,
    End,
}

impl AudioMsg {
    fn sound(&self, sounds: &Res<AudioAssets>) -> Handle<AudioSource> {
        match self {
            AudioMsg::Explosion => sounds.explosion.clone(),
            AudioMsg::UnmatchedKeypress => sounds.unmatched_keypress.clone(),
            AudioMsg::FoeLaunch => sounds.foe_launch.clone(),
            AudioMsg::End => sounds.end.clone(),
        }
    }
}

#[derive(Component)]
struct Foe {
    shape: Shape,
    keys: [char; FOE_MAX_NUM_KEYS],
    cleared: usize,
    skipped: usize,
    next_index: usize,
    orig_len: usize,
    knockback: Option<Vec2>,
    colliding: bool,
    spawned_by: Option<Entity>,
    entered_viewport: bool,
    leading_dir: Vec2,
}

impl Foe {
    pub fn new(
        shape: Shape,
        keys: [char; FOE_MAX_NUM_KEYS],
        orig_len: usize,
        spawned_by: Option<Entity>,
    ) -> Self {
        Foe {
            shape,
            keys,
            next_index: 0,
            cleared: 0,
            skipped: 0,
            orig_len,
            knockback: None,
            colliding: false,
            spawned_by,
            entered_viewport: false,
            leading_dir: shape.vertex_dirs()[0],
        }
    }
}

#[derive(Component)]
struct ExplosionParticle {
    velocity: Vec2,
    lifetime: f32,
    material: Handle<ColorMaterial>,
}

#[derive(Component)]
struct SelectPulse {
    lifetime: f32,
    material: Handle<ColorMaterial>,
}

#[derive(Component)]
struct Projectile {
    target: Entity,
}

#[derive(Component)]
struct Stats {
    score: usize,
    running: bool,
    correct_keys: usize,
    defeated: usize,
    prev_high_score: usize,
    cause: Option<&'static str>,
}

#[derive(Component)]
struct ResumeCountdown {
    counter: f32,
    displayed: usize,
}

#[derive(Resource)]
struct VolumeDrag(bool);

#[derive(Resource, Default)]
struct KeyState {
    should_repeat: bool,
    held_time: f32,
    last_repeat: f32,
    v: Option<bool>,
    h: Option<bool>,
}

fn axis(prev: Option<bool>, neg: bool, pos: bool, neg_just: bool, pos_just: bool) -> Option<bool> {
    if pos_just {
        Some(true)
    } else if neg_just {
        Some(false)
    } else if pos ^ neg {
        Some(pos)
    } else if !pos && !neg {
        None
    } else {
        prev
    }
}

impl KeyState {
    fn update(
        &mut self,
        up: bool,
        down: bool,
        left: bool,
        right: bool,
        up_just: bool,
        down_just: bool,
        left_just: bool,
        right_just: bool,
        dt: f32,
    ) {
        self.v = axis(self.v, up, down, up_just, down_just);
        self.h = axis(self.h, left, right, left_just, right_just);

        let held = up || down || left || right;
        let just_pressed = up_just || down_just || left_just || right_just;

        if just_pressed {
            self.held_time = 0.;
            self.last_repeat = 0.;
            self.should_repeat = false;
        } else if held {
            self.held_time += dt;
            self.should_repeat = self.held_time >= PRESS_REPEAT_DELAY
                && self.held_time - self.last_repeat >= PRESS_REPEAT_INTERVAL;
            if self.should_repeat {
                self.last_repeat = self.held_time;
            }
        } else {
            self.held_time = 0.;
            self.last_repeat = 0.;
            self.should_repeat = false;
        }
    }
}

#[derive(Component)]
pub struct VolumeControl;

#[derive(Component)]
pub struct VolumeControlBar;

#[derive(Component)]
pub struct VolumeDisplay;

#[derive(Component)]
pub struct MaxDifToggle;

#[derive(Component)]
pub struct Selected;

#[derive(Component)]
pub struct Selectable;

#[derive(Component)]
pub struct StarQuad;

#[derive(Asset, TypePath, AsBindGroup, Clone)]
pub struct StarfieldMaterial {
    #[uniform(0)]
    seed: UVec4,
    #[uniform(1)]
    color: Vec4,
    #[uniform(2)]
    bg: Vec4,
    #[uniform(3)]
    dust_a: Vec4,
    #[uniform(4)]
    dust_b: Vec4,
    #[uniform(5)]
    pub view: Vec4,
}

impl Material2d for StarfieldMaterial {
    fn fragment_shader() -> ShaderRef {
        concat!("embedded://", env!("CARGO_PKG_NAME"), "/starfield.wgsl").into()
    }
    fn alpha_mode(&self) -> AlphaMode2d {
        AlphaMode2d::Opaque
    }
}

#[derive(Resource)]
struct Clock(f32);

#[derive(Resource)]
pub struct Fuel(pub f32);

pub fn viewport_width(win: &Window) -> f32 {
    VIEWPORT_HEIGHT * (win.width() / win.height())
}

pub fn text_color(palette: &ColorPalette, i: usize, next: usize) -> TextColor {
    TextColor(if next == i {
        palette.text_next
    } else if next > i {
        palette.text_past
    } else {
        palette.text_future
    })
}

fn toggle_pause(mut msg: MessageWriter<PauseMsg>) {
    msg.write(PauseMsg::TogglePause);
}

fn despawn<T: Component>(mut commands: Commands, q: Query<Entity, With<T>>) {
    for e in q {
        commands.entity(e).despawn();
    }
}

#[cfg(target_arch = "wasm32")]
mod web_audio {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen(inline_js = "
let ctx;
const O = window.AudioContext;
window.AudioContext = function(...a) { ctx = new O(...a); return ctx; };
window.AudioContext.prototype = O.prototype;
export function resume_audio() { if (ctx) ctx.resume(); }
")]
    extern "C" {
        pub fn resume_audio();
    }
}
