// SPDX-License-Identifier: MIT

#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

mod credits;
mod game;
mod keys;
mod menu;
mod msg;
use bevy::{
    asset::{AssetMetaCheck, RenderAssetUsages},
    input::common_conditions::input_just_pressed,
    prelude::*,
};
use bevy_pkv::PkvStore;
use core::f32;
use rand::distr::weighted::WeightedIndex;
use serde::{Deserialize, Serialize};
use std::{f32::consts::PI, sync::LazyLock};

const SHOW_LOCAL_POINTS: bool = cfg!(feature = "show_local_points");
const INVINCIBLE: bool = cfg!(feature = "invincible");
const ONE_KEY: bool = cfg!(feature = "one_key");
const MAX_DIF: bool = cfg!(feature = "max_dif");
const MIN_DELAY: bool = cfg!(feature = "min_delay");

const PLAYER_MOVEMENT_SPEED: f32 = 400.;
const PLAYER_ROTATION_SPEED: f32 = 4.;
const TRIANGLE_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 4.;
const TRIANGLE_ROTATION_SPEED: f32 = PLAYER_ROTATION_SPEED / 6.;
const RHOMBUS_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 6.;
const RHOMBUS_ROTATION_SPEED: f32 = PLAYER_ROTATION_SPEED / 8.;
const PENTAGON_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 8.;
const PENTAGON_ROTATION_SPEED: f32 = PLAYER_ROTATION_SPEED / 6.;
const PROJECTILE_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED * 4.;
const OBSTACLE_MOVEMENT_SPEED: f32 = PLAYER_MOVEMENT_SPEED / 3.;
const OBSTACLE_TIME_TO_ENTER_VIEWPORT: f32 = 5.;
const BOUNCE_DECAY: f32 = 5.;
const BOUNCE_DURATION: f32 = 0.5;

const FIRST_FOE_SPAWN_DELAY: f32 = 0.;
const FIRST_OBSTACLE_SPAWN_DELAY: f32 = 10.;
const SPAWN_DELTA: f32 = 0.3;
const SPAWN_LOCATION_MULTIPLIER: f32 = 1.2;
const NUM_SHAPES: usize = 3;
const SHAPES: [Shape; NUM_SHAPES] = [Shape::Triangle, Shape::Rhombus, Shape::Pentagon];
const SPAWNER_FOE_WEIGHTS: [f32; NUM_SHAPES] = [0.1, 0.5, 0.4];
const FOE_SPAWN_SINCE_FACTOR: f32 = 5.;
const FOE_FORCE_SUMMONS: [f32; 3] = [
    FOE_SPAWN_SINCE_FACTOR / SPAWNER_FOE_WEIGHTS[0],
    FOE_SPAWN_SINCE_FACTOR / SPAWNER_FOE_WEIGHTS[1],
    FOE_SPAWN_SINCE_FACTOR / SPAWNER_FOE_WEIGHTS[2],
];

const TRIANGLE_NUM_KEYS: usize = 1;
const RHOMBUS_NUM_KEYS: usize = 2;
const PENTAGON_NUM_KEYS: usize = 3;
const FOE_MAX_NUM_KEYS: usize = PENTAGON_NUM_KEYS;
const PENTAGON_SUMMON_WEIGHTS: [f32; 1] = [1.0];
const PENTAGON_SPAWN_DELAY: f32 = 5.;
const SUMMONER_PROJECTILE_INC_TIME: f32 = 0.1;
const SUMMONER_COLLISION_PADDING: f32 = PLAYER_RADIUS / 2.;
const SUMMONER_ORBIT_RADIUS: f32 = PLAYER_RADIUS * 7.;
const COS_MIN_LEADING_VERTEX_ALIGNMENT: f32 = 0.3;

const FOE_SIZE: f32 = PLAYER_RADIUS * 1.2;
const TRIANGLE_CENTERING_OFFSET_Y: f32 = PLAYER_RADIUS / 2.5;
const TRIANGLE_LOCAL_POINTS: [Vec3; 3] = [
    Vec3::new(0., FOE_SIZE - TRIANGLE_CENTERING_OFFSET_Y, 0.),
    Vec3::new(FOE_SIZE / 2., -TRIANGLE_CENTERING_OFFSET_Y, 0.),
    Vec3::new(-FOE_SIZE / 2., -TRIANGLE_CENTERING_OFFSET_Y, 0.),
];
const TRIANGLE: Triangle2d = Triangle2d::new(
    Vec2::new(TRIANGLE_LOCAL_POINTS[0].x, TRIANGLE_LOCAL_POINTS[0].y),
    Vec2::new(TRIANGLE_LOCAL_POINTS[1].x, TRIANGLE_LOCAL_POINTS[1].y),
    Vec2::new(TRIANGLE_LOCAL_POINTS[2].x, TRIANGLE_LOCAL_POINTS[2].y),
);
const RHOMBUS_LOCAL_POINTS: [Vec3; 4] = [
    Vec3::new(0., -FOE_SIZE, 0.),
    Vec3::new(-FOE_SIZE / 1.5, 0., 0.),
    Vec3::new(FOE_SIZE / 1.5, 0., 0.),
    Vec3::new(0., FOE_SIZE, 0.),
];
const RHOMBUS: Rhombus = Rhombus {
    half_diagonals: Vec2::new(RHOMBUS_LOCAL_POINTS[2].x, RHOMBUS_LOCAL_POINTS[3].y),
};
static PENTAGON_LOCAL_POINTS: LazyLock<[Vec3; 5]> = LazyLock::new(|| {
    let verts = PENTAGON.vertices(0.);
    let mut points = [Vec3::ZERO; 5];
    for (i, v) in verts.into_iter().enumerate() {
        points[i] = v.extend(0.);
    }
    points
});
static PENTAGON_VERTEX_DIRS: LazyLock<[Vec2; 5]> =
    LazyLock::new(|| PENTAGON_LOCAL_POINTS.map(|p| p.xy().normalize()));
const PENTAGON: RegularPolygon = RegularPolygon {
    circumcircle: Circle { radius: FOE_SIZE },
    sides: 5,
};
const PLAYER_LOCAL_TRIANGLE: [Vec3; 3] = [
    Vec3::new(0., 0., 0.),
    Vec3::new(PLAYER_RADIUS / 2., PLAYER_RADIUS, 0.),
    Vec3::new(-PLAYER_RADIUS / 2., PLAYER_RADIUS, 0.),
];

fn spawner_foe_delay_mu(x: f32, max_dif: bool) -> f32 {
    if MIN_DELAY {
        SPAWN_DELTA
    } else {
        7. * f32::exp(-0.07 * if max_dif { f32::MAX } else { x }) + 0.5
    }
}

fn obstacle_spawn_delay_mu(x: f32, max_dif: bool) -> f32 {
    if MIN_DELAY {
        SPAWN_DELTA * 5.
    } else {
        5. * f32::exp(-0.02 * if max_dif { f32::MAX } else { x }) + 3.
    }
}

const PLAYER_RADIUS: f32 = 50.;
const OBSTACLE_RADIUS: f32 = PLAYER_RADIUS / 1.5;
const PROJECTILE_RADIUS: f32 = PLAYER_RADIUS / 10.;
const EXPLOSION_PARTICLE_RADIUS_LOWER: f32 = PLAYER_RADIUS / 10.;
const EXPLOSION_PARTICLE_RADIUS_UPPER: f32 = PLAYER_RADIUS / 5.;
const PLAYER_LAUNCHER_LENGTH: f32 = PLAYER_RADIUS / 1.5;
const PLAYER_LAUNCHER_WIDTH: f32 = PROJECTILE_RADIUS * 2.;
const INDICATOR_THICKNESS: f32 = PLAYER_RADIUS / 10.;
const INDICATOR_RADIUS: f32 = 1.5 * PLAYER_RADIUS;
const INDICATOR_LONG_RECTANGLE_LENGTH: f32 = INDICATOR_THICKNESS * 3.;

const TRACKING_Z_INDEX: f32 = 2.;
const OBSTACLE_Z_INDEX: f32 = 1.;
const EXPLOSION_Z_INDEX: f32 = 0.;
const EXPLOSION_Z_INDEX_RANGE: f32 = 0.1;
const SUMMONER_Z_INDEX: f32 = 3.;
const INDICATOR_Z_INDEX: f32 = 5.;
const PLAYER_Z_INDEX: f32 = 4.;
const VIEWPORT_HEIGHT: f32 = 1000.;

static TITLE_FONT: LazyLock<TextFont> = LazyLock::new(|| TextFont {
    font_size: 60.,
    ..default()
});
static FONT: LazyLock<TextFont> = LazyLock::new(|| TextFont {
    font_size: 30.,
    ..default()
});
const SCORE_TEXT_PADDING: f32 = 10.;

const EXPLOSION_PARTICLE_MAX_LIFETIME: f32 = 1.;
const EXPLOSION_PARTICLE_INITIAL_ALPHA: f32 = 0.7;

const GAME_OVER_SLOWDOWN_REAL_TIME: f32 = 3.;
const TIME_BEFORE_RESUME: f32 = 3.;

const HIGH_SCORE_KEY: &str = "high_score";
const VOLUME_KEY: &str = "volume";
const PHYSICAL_KEYBOARD_LAYOUT_KEY: &str = "physical_keyboard_layout_key";
const LOGICAL_KEYBOARD_LAYOUT_KEY: &str = "logical_keyboard_layout_key";
const MAX_DIFFICULTY_KEY: &str = "max_difficulty";

mod palette {
    use bevy::color::Color;
    pub const BLACK: Color = Color::srgb(0., 0., 0.);
    pub const WHITE: Color = Color::srgb(1., 1., 1.);
    pub const RED: Color = Color::srgb(1., 0., 0.);
    pub const GREEN: Color = Color::srgb(0., 1., 0.);
    pub const BLUE: Color = Color::srgb(0., 0., 1.);
    pub const YELLOW: Color = Color::srgb(1., 1., 0.);
    pub const MAGENTA: Color = Color::srgb(0.5, 0., 0.5);
    pub const CYAN: Color = Color::srgb(0., 0.5, 0.5);
    pub const GREY: Color = Color::srgb(0.5, 0.5, 0.5);
    pub const CHARCOAL: Color = Color::srgb(0.28, 0.28, 0.28);
}

mod colors {
    use crate::palette;
    use bevy::color::{Color, Srgba};
    pub const BACKGROUND: Color = palette::BLACK;
    pub const TRIANGLE: Color = palette::RED;
    pub const RHOMBUS: Color = palette::BLUE;
    pub const PENTAGON: Color = palette::MAGENTA;
    pub const INDICATOR: Color = palette::YELLOW;
    pub const PROJECTILE: Color = palette::WHITE;
    pub const OBSTACLE: Color = palette::CYAN;
    pub const PLAYER: Color = palette::WHITE;
    pub const LAUNCHER: Color = palette::GREY;
    pub const TEXT_NEXT: Color = palette::GREEN;
    pub const TEXT_DONE: Color = palette::BLACK;
    pub const TEXT_FUTURE: Color = palette::WHITE;
    pub const IN_GAME_MENU: Color = match palette::CHARCOAL {
        Color::Srgba(x) => Color::Srgba(Srgba { alpha: 0.7, ..x }),
        _ => Color::srgb(0., 0., 0.),
    };
    pub const SELECTED_OUTLINE: Color = palette::WHITE;
    pub const UNSELECTED_OUTLINE: Color = palette::GREY;
    pub const VOLUME_BAR: Color = palette::WHITE;
}

#[derive(Resource)]
struct Config {
    high_score: usize,
    volume: u8,
    physical_keyboard_layout: KeyboardLayouts,
    logical_keyboard_layout: KeyboardLayouts,
    max_difficulty: bool,
}

impl Config {
    pub fn load(pkv: &mut PkvStore) -> Self {
        let mut config = Config {
            high_score: 0,
            volume: 100,
            physical_keyboard_layout: KeyboardLayouts::Qwerty,
            logical_keyboard_layout: KeyboardLayouts::Qwerty,
            max_difficulty: false,
        };
        load_or_set(pkv, HIGH_SCORE_KEY, &mut config.high_score);
        load_or_set(pkv, VOLUME_KEY, &mut config.volume);
        load_or_set(
            pkv,
            PHYSICAL_KEYBOARD_LAYOUT_KEY,
            &mut config.physical_keyboard_layout,
        );
        load_or_set(
            pkv,
            LOGICAL_KEYBOARD_LAYOUT_KEY,
            &mut config.logical_keyboard_layout,
        );
        load_or_set(pkv, MAX_DIFFICULTY_KEY, &mut config.max_difficulty);
        config
    }

    pub fn right(&self) -> KeyCode {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => KeyCode::KeyD,
            KeyboardLayouts::Dvorak => KeyCode::KeyE,
            KeyboardLayouts::Colemak => KeyCode::KeyS,
        }
    }

    pub fn right_char(&self) -> char {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => 'd',
            KeyboardLayouts::Dvorak => 'e',
            KeyboardLayouts::Colemak => 's',
        }
    }

    pub fn left(&self) -> KeyCode {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => KeyCode::KeyA,
            KeyboardLayouts::Dvorak => KeyCode::KeyA,
            KeyboardLayouts::Colemak => KeyCode::KeyA,
        }
    }

    pub fn left_char(&self) -> char {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => 'a',
            KeyboardLayouts::Dvorak => 'a',
            KeyboardLayouts::Colemak => 'a',
        }
    }

    pub fn up(&self) -> KeyCode {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => KeyCode::KeyW,
            KeyboardLayouts::Dvorak => KeyCode::Comma,
            KeyboardLayouts::Colemak => KeyCode::KeyW,
        }
    }

    pub fn up_char(&self) -> char {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => 'w',
            KeyboardLayouts::Dvorak => ',',
            KeyboardLayouts::Colemak => 'w',
        }
    }

    pub fn down(&self) -> KeyCode {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => KeyCode::KeyS,
            KeyboardLayouts::Dvorak => KeyCode::KeyO,
            KeyboardLayouts::Colemak => KeyCode::KeyR,
        }
    }

    pub fn down_char(&self) -> char {
        match self.physical_keyboard_layout {
            KeyboardLayouts::Qwerty => 's',
            KeyboardLayouts::Dvorak => 'o',
            KeyboardLayouts::Colemak => 'r',
        }
    }

    pub fn keypool(&self) -> [char; 16] {
        match self.logical_keyboard_layout {
            KeyboardLayouts::Qwerty => keys::QWERTY_POOL,
            KeyboardLayouts::Dvorak => keys::DVORAK_POOL,
            KeyboardLayouts::Colemak => keys::COLEMAK_POOL,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq)]
enum KeyboardLayouts {
    Qwerty,
    Dvorak,
    Colemak,
}

fn main() {
    let mut pkv = PkvStore::new("timware", env!("CARGO_PKG_NAME"));
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        title: env!("CARGO_PKG_NAME").to_string(),
                        ..default()
                    }),
                    ..default()
                })
                .set(AssetPlugin {
                    meta_check: AssetMetaCheck::Never,
                    ..default()
                }),
            menu_plugin,
            game_plugin,
        ))
        .add_message::<PauseMsg>()
        .add_systems(Startup, setup)
        .init_state::<Screen>()
        .insert_resource(Config::load(&mut pkv))
        .insert_resource(ClearColor(colors::BACKGROUND))
        .insert_resource(pkv)
        .run();
}

fn load_or_set<T: serde::de::DeserializeOwned + serde::Serialize>(
    pkv: &mut PkvStore,
    key: &str,
    field: &mut T,
) {
    if let Ok(val) = pkv.get::<T>(key) {
        *field = val;
    } else {
        let _ = pkv.set(key, field);
    };
}

fn setup(
    mut commands: Commands,
    mut config: ResMut<Config>,
    mut global_volume: ResMut<GlobalVolume>,
) {
    commands.spawn((
        Camera2d,
        Projection::from(OrthographicProjection {
            scaling_mode: bevy::camera::ScalingMode::FixedVertical {
                viewport_height: VIEWPORT_HEIGHT,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));
    let vol = config.volume;
    config.set_vol(vol, &mut global_volume);
}

fn menu_plugin(app: &mut App) {
    app.add_systems(OnExit(Screen::MainMenu), menu::despawn_screen::<MenuScreen>)
        .add_systems(OnEnter(Screen::MainMenu), menu::menu_setup)
        .add_systems(OnEnter(Screen::Settings), menu::settings_setup)
        .add_systems(
            OnExit(Screen::Settings),
            menu::despawn_screen::<SettingsScreen>,
        )
        .add_systems(OnEnter(Screen::Help), menu::help_setup)
        .add_systems(OnExit(Screen::Help), menu::despawn_screen::<HelpScreen>)
        .add_systems(OnEnter(Screen::Credits), menu::credits_setup)
        .add_systems(
            OnExit(Screen::Credits),
            menu::despawn_screen::<CreditsScreen>,
        )
        .add_systems(
            Update,
            (
                menu::mouse,
                menu::on_selection,
                menu::on_active,
                menu::keypress,
            )
                .run_if(in_menu),
        )
        .add_systems(
            Update,
            (
                menu::volume_drag_control,
                menu::volume_start_drag,
                menu::update_volume_bar,
            )
                .run_if(in_state(Screen::Settings).or(in_state(GameScreen::Settings))),
        )
        .insert_resource(VolumeDrag(false));
}

fn in_menu(state: Res<State<Screen>>, game_state: Res<State<GameScreen>>) -> bool {
    matches!(
        state.get(),
        Screen::MainMenu | Screen::Credits | Screen::Settings | Screen::Help,
    ) || matches!(
        game_state.get(),
        GameScreen::Pause | GameScreen::End | GameScreen::Settings
    )
}

fn game_plugin(app: &mut App) {
    let exit = (
        game::default_state,
        game::despawn::<Player>,
        game::despawn::<Foe>,
        game::despawn::<Obstacle>,
        game::despawn::<ExplosionParticle>,
        game::despawn::<Projectile>,
        game::despawn::<Indicator>,
        game::despawn::<Slowdown>,
        game::despawn::<Stats>,
    );
    app.add_systems(OnEnter(Screen::Game), game::setup)
        .add_systems(OnExit(Screen::Game), exit)
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
                game::enemy_collisions,
                game::update_spawned_relations,
                game::player_collisions,
                game::obstacle_collisions,
                game::spawner,
                game::spawn_obstacles,
                game::track_selected_enemy,
                game::update_clock,
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(
            Update,
            (
                game::player_movement,
                game::keypress,
                game::slowdown_time,
                game::projectile,
                game::tracking_foe,
                game::summoner_foe,
                game::summoner,
                game::obstacle,
                game::explosion_system,
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(
            Update,
            menu::resume_countdown.run_if(in_state(GameScreen::ResumeCountdown)),
        )
        .add_systems(
            Update,
            toggle_pause.run_if(input_just_pressed(KeyCode::Escape).and(in_state(Screen::Game))),
        )
        .add_systems(
            PostUpdate,
            (
                msg::on_msg,
                msg::on_audio,
                game::reset_collisions.after(msg::on_msg),
                game::despawner.after(msg::on_msg),
                game::lock_enemy_text.before(TransformSystems::Propagate),
                game::lock_enemy_text.after(msg::on_msg),
            )
                .run_if(in_state(Screen::Game)),
        )
        .add_systems(PostUpdate, msg::on_toggle_pause);
}

#[macro_export]
macro_rules! points {
    ($shape:expr, $transform:expr) => {{
        match $shape {
            Shape::Triangle => &TRIANGLE_LOCAL_POINTS.map(|p| $transform.transform_point(p)),
            Shape::Rhombus => &RHOMBUS_LOCAL_POINTS.map(|p| $transform.transform_point(p)),
            Shape::Pentagon => &PENTAGON_LOCAL_POINTS.map(|p| $transform.transform_point(p)),
        }
    }};
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
struct ResumeCountdownScreen;

#[derive(Component)]
struct CreditsScreen;

#[derive(Component)]
struct MenuScreen;

#[derive(Clone, Copy, Default, Eq, PartialEq, Debug, Hash, States)]
enum Screen {
    #[default]
    MainMenu,
    Game,
    Settings,
    Help,
    Credits,
}

#[derive(Clone, Copy, Default, Eq, PartialEq, Debug, Hash, States)]
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
}

#[derive(Resource)]
struct MovementControls {
    v: Option<bool>,
    h: Option<bool>,
}

#[derive(Message)]
enum GameMsg {
    Explosion(Vec2),
    Despawn(Entity),
    DespawnChildren(Entity),
    ReplaceShape(Entity, Shape),
    AddText(Entity),
    SpawnFoe(Shape, Vec2, Option<Entity>, Option<f32>),
    SpawnObstacle(Vec2, Vec2),
    Invisible(Entity),
    Visible(Entity),
    Select(Entity),
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
    foe_spawns_since: [usize; NUM_SHAPES],
    foe_delay: f32,
    foe_delay_mu: f32,
    obstacle_delay: f32,
    obstacle_delay_mu: f32,
    last_side: usize,
}

#[derive(Component)]
struct Summoner {
    since: f32,
    delay: f32,
    foe_dist: WeightedIndex<f32>,
    leading_vertex: usize,
}

#[derive(Clone, Copy)]
enum Shape {
    Triangle,
    Rhombus,
    Pentagon,
}

#[derive(Component)]
struct Tracking;

#[derive(Component)]
struct Targeted;

#[derive(Component)]
struct ToDespawn;

#[derive(Component)]
struct Indicator {
    active: bool,
}

#[derive(Component)]
struct Launcher;

#[derive(Component)]
struct Foe {
    shape: Shape,
    keys: [char; FOE_MAX_NUM_KEYS],
    cleared: usize,
    skipped: usize,
    next_index: usize,
    orig_len: usize,
    bounce_velocity: Option<Vec2>,
    bounce_timer: f32,
    colliding: bool,
    spawned_by: Option<Entity>,
}

#[derive(Component)]
struct EnemyText;

#[derive(Component)]
struct Slowdown {
    time: f32,
}

#[derive(Component)]
struct Obstacle {
    direction: Vec2,
    time_to_enter_viewport: f32,
    entered_view: bool,
    colliding: bool,
}

#[derive(Resource)]
struct AudioAssets {
    pub projectile_launch: Handle<AudioSource>,
    pub unmatched_keypress: Handle<AudioSource>,
    pub explosion: Handle<AudioSource>,
}

#[derive(Message)]
pub enum AudioMsg {
    ProjectileLaunch,
    Explosion,
    UnmatchedKeypress,
}

impl AudioMsg {
    fn sound(&self, sounds: &Res<AudioAssets>) -> Handle<AudioSource> {
        match self {
            AudioMsg::ProjectileLaunch => sounds.projectile_launch.clone(),
            AudioMsg::Explosion => sounds.explosion.clone(),
            AudioMsg::UnmatchedKeypress => sounds.unmatched_keypress.clone(),
        }
    }
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
            bounce_velocity: None,
            bounce_timer: 0.,
            colliding: false,
            spawned_by,
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
struct Projectile {
    target: Entity,
}

#[derive(Component)]
struct Stats {
    score: usize,
    running: bool,
}

#[derive(Component)]
struct ResumeCountdown {
    counter: f32,
    displayed: usize,
}

#[derive(Resource)]
struct VolumeDrag(bool);

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
pub struct Active;

#[derive(Component)]
pub struct KeyboardOption;

#[derive(Resource)]
struct Clock(f32);

fn toggle_pause(mut msg: MessageWriter<PauseMsg>) {
    msg.write(PauseMsg::TogglePause);
}
