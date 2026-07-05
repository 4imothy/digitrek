// SPDX-License-Identifier: MIT

use crate::*;
use bevy::{
    ecs::relationship::RelatedSpawnerCommands,
    input::{ButtonState, keyboard::KeyboardInput},
    input_focus::{FocusCause, InputFocus},
    prelude::*,
    text::{EditableText, EditableTextFilter, LineHeight, TextCursorStyle, TextEdit},
    ui::{RelativeCursorPosition, UiGlobalTransform},
};

#[derive(Clone, Copy, PartialEq)]
pub enum RebindKind {
    Up,
    Down,
    Left,
    Right,
    Select,
    Back,
    Deselect,
    ThrustLeft,
    ThrustRight,
}

impl RebindKind {
    fn get(self, config: &Config) -> KeyCode {
        match self {
            RebindKind::Up => config.up,
            RebindKind::Down => config.down,
            RebindKind::Left => config.left,
            RebindKind::Right => config.right,
            RebindKind::Select => config.select,
            RebindKind::Back => config.back,
            RebindKind::Deselect => config.deselect,
            RebindKind::ThrustLeft => config.rotate_left,
            RebindKind::ThrustRight => config.rotate_right,
        }
    }
    fn set(self, config: &mut Config, code: KeyCode) {
        match self {
            RebindKind::Up => config.up = code,
            RebindKind::Down => config.down = code,
            RebindKind::Left => config.left = code,
            RebindKind::Right => config.right = code,
            RebindKind::Select => config.select = code,
            RebindKind::Back => config.back = code,
            RebindKind::Deselect => config.deselect = code,
            RebindKind::ThrustLeft => config.rotate_left = code,
            RebindKind::ThrustRight => config.rotate_right = code,
        }
    }
    fn name(self) -> &'static str {
        match self {
            RebindKind::Up => "up",
            RebindKind::Down => "down",
            RebindKind::Left => "left",
            RebindKind::Right => "right",
            RebindKind::Select => "select",
            RebindKind::Back => "back",
            RebindKind::Deselect => "deselect",
            RebindKind::ThrustLeft => "left thruster",
            RebindKind::ThrustRight => "right thruster",
        }
    }
}

#[derive(Resource, Default)]
pub struct Rebinding {
    pub kind: Option<RebindKind>,
    ready: bool,
}

impl Rebinding {
    fn arm(&mut self, kind: RebindKind) {
        self.kind = Some(kind);
        self.ready = false;
    }
}

#[derive(Component)]
pub struct RebindDisplay(RebindKind);

fn key_name(code: KeyCode) -> String {
    let raw = format!("{code:?}");
    let raw = raw.strip_prefix("Key").unwrap_or(&raw);
    let mut out = String::new();
    for (i, c) in raw.chars().enumerate() {
        if c.is_uppercase() && i != 0 {
            out.push(' ');
        }
        out.push(c.to_ascii_lowercase());
    }
    out
}

const VOLUME_LABEL_PREFIX: &str = "volume:";

const FONT_DROP_LABEL: &str = "drop font";
const FONT_ERROR_LABEL: &str = "not a font";

const COLOR_LABEL_WIDTH: f32 = 14.;
const COLOR_SWATCH_SIZE: f32 = SMALL_FONT_SIZE;
const COLOR_CHANNEL_WIDTH: f32 = 5.6;
const COLOR_GRID_GAP: f32 = 1.;
const COLOR_COLUMNS_GAP: f32 = 30.;
const COLOR_ROW_MARGIN: f32 = 4.;
const COLOR_RADIUS: f32 = 4.;
const COLOR_SWATCH_BORDER: f32 = 1.;
const COLOR_CELL_BORDER: f32 = 2.;
const COLOR_CELL_PAD_X: f32 = 6.;
const COLOR_CELL_PAD_Y: f32 = 4.;
const ERROR_COLOR: Color = Color::srgb_u8(255, 85, 85);
const ERROR_SECS: f32 = 2.;

#[derive(Component, PartialEq, Clone, Copy)]
pub enum Label {
    Play,
    Settings,
    Credits,
    Volume,
    Keybinds,
    MoveUp,
    MoveDown,
    MoveLeft,
    MoveRight,
    SelectKey,
    BackKey,
    DeselectKey,
    RotateLeft,
    RotateRight,
    RestoreKeybinds,
    MaxDifficulty,
    RestoreFont,
    Colors,
    RestoreColors,
    ProgrammingLanguage,
    GameEngine,
    Palette,
    SpaceGrotesk,
    FontForge,
    FFmpeg,
    Csound,
    Source,
    Wordfreq,
    GWordList,
    Help,
    Resume,
    GameSettings,
    PlayAgain,
    Back,
}

impl Label {
    fn to_link(self) -> &'static str {
        match self {
            Label::ProgrammingLanguage => credits::PROGRAMMING_LANGUAGE_LINK,
            Label::GameEngine => credits::GAME_ENGINE_LINK,
            Label::Palette => credits::PALETTE_LINK,
            Label::SpaceGrotesk => credits::SPACE_GROTESK_LINK,
            Label::Csound => credits::CSOUND_LINK,
            Label::FontForge => credits::FONTFORGE_LINK,
            Label::FFmpeg => credits::FFMPEG_LINK,
            Label::Source => credits::SOURCE_LINK,
            Label::Wordfreq => credits::WORDS_LINK,
            Label::GWordList => credits::GWORDLIST_LINK,
            _ => unreachable!(),
        }
    }
}

fn button() -> Node {
    Node {
        padding: UiRect::new(Val::Vw(1.), Val::Vw(1.), Val::Vh(0.5), Val::Vh(0.5)),
        margin: UiRect::new(Val::Vw(1.), Val::Vw(1.), Val::Vh(0.5), Val::Vh(0.5)),
        justify_content: JustifyContent::Center,
        align_items: AlignItems::Center,
        border: UiRect::all(Val::Vh(0.3)),
        border_radius: BorderRadius::MAX,
        ..default()
    }
}

fn spawn_button(
    cmd: &mut RelatedSpawnerCommands<ChildOf>,
    action: Label,
    text: &str,
    selected: bool,
    font: TextFont,
    palette: &ColorPalette,
) {
    let mut e = cmd.spawn((
        Button,
        button(),
        BorderColor::all(palette.unselected_outline()),
        action,
        Selectable,
    ));
    e.with_children(|p| {
        p.spawn((Text::new(text), font, TextColor(palette.text)));
    });
    if selected {
        e.insert(Selected);
    }
}

pub fn menu_setup(
    mut commands: Commands,
    config: Res<Config>,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    let title_font = title_font(&app_font.0);
    let font = text_font(&app_font.0);
    let title_chars = palette.title_chars();
    commands
        .spawn(screen_with(MenuScreen))
        .with_children(|cmd| {
            cmd.spawn((Node {
                justify_content: JustifyContent::SpaceBetween,
                align_items: AlignItems::Center,
                ..default()
            },))
                .with_children(|title| {
                    let name = env!("CARGO_PKG_NAME");
                    let len = name.chars().count();
                    let angle_span = PI / 4.;
                    let angle_start = -angle_span / 2.;
                    for (i, c) in name.chars().enumerate() {
                        let t = i as f32 / (len - 1) as f32;
                        let angle = angle_start + t * angle_span;
                        title.spawn((
                            Text::new(c),
                            title_font.clone(),
                            UiTransform {
                                translation: Val2::new(Val::ZERO, Val::Vh(-5. * (t * PI).sin())),
                                rotation: Rot2::radians(angle),
                                ..default()
                            },
                            TextColor(title_chars[i]),
                            Node {
                                padding: UiRect::horizontal(Val::Vh(0.5)),
                                ..default()
                            },
                        ));
                    }
                });

            cmd.spawn((
                Text::new(format!("highscore: {}", config.high_score)),
                font.clone(),
                TextColor(palette.label()),
            ));

            spawn_button(cmd, Label::Play, "play", true, font.clone(), &palette);
            spawn_button(cmd, Label::Help, "help", false, font.clone(), &palette);
            spawn_button(
                cmd,
                Label::Settings,
                "settings",
                false,
                font.clone(),
                &palette,
            );
            spawn_button(
                cmd,
                Label::Credits,
                "credits",
                false,
                font.clone(),
                &palette,
            );
        });
}

pub fn on_selection(
    selection: Single<Entity, Added<Selected>>,
    mut removed: RemovedComponents<Selected>,
    mut borders: Query<&mut BorderColor>,
    palette: Res<ColorPalette>,
) {
    for entity in removed.read() {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor::all(palette.unselected_outline());
        }
    }
    if let Ok(mut new) = borders.get_mut(*selection) {
        *new = BorderColor::all(palette.selected_outline());
    }
}

pub fn mouse(
    mut interaction_query: Query<(&Interaction, &Label), (Changed<Interaction>, With<Button>)>,
    mut hovers: Query<
        (&Interaction, &mut BorderColor),
        (Changed<Interaction>, With<Button>, Without<Selected>),
    >,
    mut max_difficulty_toggle: Query<&mut BackgroundColor, With<MaxDifToggle>>,
    mut pause_msg: MessageWriter<PauseMsg>,
    screen: Res<State<Screen>>,
    game_screen: Res<State<GameScreen>>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut next_game_screen: ResMut<NextState<GameScreen>>,
    mut vtime: ResMut<Time<Virtual>>,
    mut config: ResMut<Config>,
    mut palette: ResMut<ColorPalette>,
    mut rebinding: ResMut<Rebinding>,
) {
    for (interaction, menu_button_action) in interaction_query.iter_mut() {
        if *interaction == Interaction::Pressed {
            do_action(
                *menu_button_action,
                &mut pause_msg,
                &screen,
                &game_screen,
                &mut next_screen,
                &mut next_game_screen,
                &mut max_difficulty_toggle,
                &mut vtime,
                &mut config,
                &mut palette,
                &mut rebinding,
            );
        }
    }
    for (interaction, mut border_color) in hovers.iter_mut() {
        if *interaction == Interaction::Hovered {
            *border_color = BorderColor::all(palette.selected_outline());
        } else if *interaction == Interaction::None {
            *border_color = BorderColor::all(palette.unselected_outline());
        }
    }
}

fn do_action(
    action: Label,
    pause_msg: &mut MessageWriter<PauseMsg>,
    screen: &State<Screen>,
    game_screen: &State<GameScreen>,
    next_screen: &mut ResMut<NextState<Screen>>,
    next_game_screen: &mut ResMut<NextState<GameScreen>>,
    max_difficulty_toggle: &mut Query<&mut BackgroundColor, With<MaxDifToggle>>,
    vtime: &mut ResMut<Time<Virtual>>,
    config: &mut ResMut<Config>,
    palette: &mut ResMut<ColorPalette>,
    rebinding: &mut ResMut<Rebinding>,
) {
    match action {
        Label::MoveUp => rebinding.arm(RebindKind::Up),
        Label::MoveDown => rebinding.arm(RebindKind::Down),
        Label::MoveLeft => rebinding.arm(RebindKind::Left),
        Label::MoveRight => rebinding.arm(RebindKind::Right),
        Label::SelectKey => rebinding.arm(RebindKind::Select),
        Label::BackKey => rebinding.arm(RebindKind::Back),
        Label::DeselectKey => rebinding.arm(RebindKind::Deselect),
        Label::RotateLeft => rebinding.arm(RebindKind::ThrustLeft),
        Label::RotateRight => rebinding.arm(RebindKind::ThrustRight),
        Label::RestoreKeybinds => {
            let d = Config::default();
            config.up = d.up;
            config.down = d.down;
            config.left = d.left;
            config.right = d.right;
            config.select = d.select;
            config.back = d.back;
            config.deselect = d.deselect;
            config.rotate_left = d.rotate_left;
            config.rotate_right = d.rotate_right;
        }
        Label::Play => {
            #[cfg(target_arch = "wasm32")]
            web_audio::resume_audio();
            next_screen.set(Screen::Game);
        }
        Label::Settings => next_screen.set(Screen::Settings),
        Label::Help => next_screen.set(Screen::Help),
        Label::Credits => next_screen.set(Screen::Credits),
        Label::Colors => next_screen.set(Screen::ColorSettings),
        Label::Keybinds => next_screen.set(Screen::Keybinds),
        Label::Back => match (**screen, **game_screen) {
            (Screen::Settings, _) => {
                next_screen.set(Screen::MainMenu);
            }
            (Screen::Credits | Screen::Help, _) => {
                next_screen.set(Screen::MainMenu);
            }
            (Screen::ColorSettings | Screen::Keybinds, _) => {
                next_screen.set(Screen::Settings);
            }
            (Screen::Game, GameScreen::Settings) => {
                next_game_screen.set(GameScreen::Pause);
            }
            (Screen::Game, GameScreen::Pause | GameScreen::End) => {
                vtime.unpause();
                next_screen.set(Screen::MainMenu);
            }
            _ => {}
        },
        Label::ProgrammingLanguage
        | Label::GameEngine
        | Label::Palette
        | Label::SpaceGrotesk
        | Label::FontForge
        | Label::FFmpeg
        | Label::Csound
        | Label::Source
        | Label::Wordfreq
        | Label::GWordList => {
            #[cfg(not(target_arch = "wasm32"))]
            let _ = open::that(action.to_link());

            #[cfg(target_arch = "wasm32")]
            let _ = web_sys::window().unwrap().open_with_url(action.to_link());
        }
        Label::Resume => {
            pause_msg.write(PauseMsg::TogglePause);
        }
        Label::GameSettings => {
            next_game_screen.set(GameScreen::Settings);
        }
        Label::PlayAgain => {
            vtime.unpause();
            next_screen.set(Screen::Game);
        }
        Label::MaxDifficulty => {
            config.max_difficulty = !config.max_difficulty;
            *max_difficulty_toggle.single_mut().unwrap() =
                BackgroundColor(if config.max_difficulty {
                    palette.selected_outline()
                } else {
                    Color::NONE
                });
        }
        Label::RestoreFont => {
            font_store::restore_default();
        }
        Label::RestoreColors => {
            config.reset_colors();
            **palette = ColorPalette::default();
        }
        _ => {}
    }
}

pub fn despawn_screen<T: Component>(screen: Single<Entity, With<T>>, mut commands: Commands) {
    commands.entity(*screen).despawn();
}

pub fn pause_setup(mut commands: Commands, app_font: Res<AppFont>, palette: Res<ColorPalette>) {
    let font = text_font(&app_font.0);
    commands
        .spawn(ingame_screen(PauseScreen, &palette))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new("game is paused"),
                TextLayout::justify(Justify::Center),
                font.clone(),
            ));
            spawn_button(cmd, Label::Resume, "resume", true, font.clone(), &palette);
            spawn_button(
                cmd,
                Label::GameSettings,
                "settings",
                false,
                font.clone(),
                &palette,
            );
            spawn_button(cmd, Label::Back, "exit", false, font.clone(), &palette);
        });
}

pub fn end_setup(
    mut commands: Commands,
    stats: Single<&mut Stats>,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    let font = text_font(&app_font.0);
    commands
        .spawn(ingame_screen(EndScreen, &palette))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new(format!("game over\nscore: {}", stats.score)),
                TextLayout::justify(Justify::Center),
                font.clone(),
            ));
            spawn_button(
                cmd,
                Label::PlayAgain,
                "play again",
                true,
                font.clone(),
                &palette,
            );
            spawn_button(cmd, Label::Back, "exit", false, font.clone(), &palette);
        });
}

pub fn resume_countdown_setup(
    mut commands: Commands,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    commands.spawn((
        ResumeCountdown {
            counter: TIME_BEFORE_RESUME,
            displayed: TIME_BEFORE_RESUME as usize,
        },
        Node {
            justify_self: JustifySelf::Center,
            align_self: AlignSelf::Center,
            margin: UiRect::bottom(Val::Px(PLAYER_RADIUS * 2.)),
            padding: UiRect::new(
                Val::Percent(1.),
                Val::Percent(1.),
                Val::Percent(0.),
                Val::Percent(0.),
            ),
            border_radius: BorderRadius::all(Val::Percent(100.)),
            ..default()
        },
        BackgroundColor(palette.in_game_menu()),
        Text::new((TIME_BEFORE_RESUME as usize).to_string()),
        text_font(&app_font.0),
        TextColor(palette.text),
        TextLayout::justify(Justify::Center),
    ));
}

pub fn help_setup(mut commands: Commands, app_font: Res<AppFont>, palette: Res<ColorPalette>) {
    let font = text_font(&app_font.0);
    commands
        .spawn(screen_with(HelpScreen))
        .with_children(|screen| {
            screen.spawn((
                Text::new(
                    "avoid polygons and circles\n\
                     type the word on a polygon to destroy it\n\
                     press the deselect key to target a different polygon\n\
                     power a thruster to rotate, power both to fly forward\n\
                     powering a thruster uses fuel, typing and time restore it",
                ),
                font.clone(),
                TextColor(palette.label()),
                TextLayout {
                    justify: Justify::Center,
                    ..default()
                },
            ));
            spawn_button(screen, Label::Back, "back", true, font.clone(), &palette);
        });
}

pub fn game_settings_setup(
    mut commands: Commands,
    config: Res<Config>,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    let font = text_font(&app_font.0);
    let (c, n, mut bg) = ingame_screen(GameSettingsScreen, &palette);
    bg.0 = palette.background;
    commands.spawn((c, n, bg)).with_children(|s| {
        add_volume(s, &config, &app_font.0, &palette);
        spawn_button(s, Label::Back, "back", false, font.clone(), &palette);
    });
}

pub fn settings_setup(
    mut commands: Commands,
    config: Res<Config>,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    let font = text_font(&app_font.0);
    commands
        .spawn(screen_with(SettingsScreen))
        .with_children(|screen| {
            add_volume(screen, &config, &app_font.0, &palette);

            screen
                .spawn((
                    Label::MaxDifficulty,
                    Button,
                    BorderColor::all(palette.unselected_outline()),
                    Node {
                        flex_direction: FlexDirection::Row,
                        ..button()
                    },
                    Selectable,
                ))
                .with_children(|button| {
                    button.spawn((
                        Text::new("max difficulty"),
                        font.clone(),
                        TextColor(palette.text),
                        TextLayout::no_wrap(),
                    ));
                    button.spawn((
                        MaxDifToggle,
                        Node {
                            height: Val::Percent(80.),
                            width: Val::Vw(2.),
                            border: UiRect::all(Val::Vh(0.3)),
                            margin: UiRect::left(Val::Vw(1.)),
                            border_radius: BorderRadius::all(Val::Percent(100.)),
                            ..default()
                        },
                        BorderColor::all(palette.selected_outline()),
                        BackgroundColor(if config.max_difficulty {
                            palette.selected_outline()
                        } else {
                            Color::NONE
                        }),
                    ));
                });

            screen.spawn(Node::default()).with_children(|row| {
                let mut e = row.spawn((
                    DropZone,
                    Node {
                        border_radius: BorderRadius::ZERO,
                        ..button()
                    },
                    BorderColor::all(palette.unselected_outline()),
                ));
                e.with_children(|zone| {
                    zone.spawn((
                        Text::new(FONT_DROP_LABEL),
                        font.clone(),
                        TextColor(palette.text),
                    ));
                });
                spawn_button(
                    row,
                    Label::RestoreFont,
                    "restore font",
                    false,
                    font.clone(),
                    &palette,
                );
            });
            screen.spawn(Node::default()).with_children(|row| {
                spawn_button(row, Label::Colors, "colors", false, font.clone(), &palette);
                spawn_button(
                    row,
                    Label::RestoreColors,
                    "restore colors",
                    false,
                    font.clone(),
                    &palette,
                );
            });
            screen.spawn(Node::default()).with_children(|row| {
                spawn_button(
                    row,
                    Label::Keybinds,
                    "keybinds",
                    false,
                    font.clone(),
                    &palette,
                );
                spawn_button(
                    row,
                    Label::RestoreKeybinds,
                    "restore keybinds",
                    false,
                    font.clone(),
                    &palette,
                );
            });
            spawn_button(screen, Label::Back, "back", false, font.clone(), &palette);
        });
}

pub fn keybinds_setup(
    mut commands: Commands,
    config: Res<Config>,
    app_font: Res<AppFont>,
    palette: Res<ColorPalette>,
) {
    let font = text_font(&app_font.0);
    let binds = [
        (Label::MoveUp, RebindKind::Up),
        (Label::MoveDown, RebindKind::Down),
        (Label::MoveLeft, RebindKind::Left),
        (Label::MoveRight, RebindKind::Right),
        (Label::SelectKey, RebindKind::Select),
        (Label::BackKey, RebindKind::Back),
        (Label::RotateLeft, RebindKind::ThrustLeft),
        (Label::RotateRight, RebindKind::ThrustRight),
        (Label::DeselectKey, RebindKind::Deselect),
    ];
    commands
        .spawn(screen_with(KeybindsScreen))
        .with_children(|screen| {
            for (i, pair) in binds.chunks(2).enumerate() {
                screen
                    .spawn(Node {
                        flex_direction: FlexDirection::Row,
                        ..default()
                    })
                    .with_children(|row| {
                        for (j, (label, kind)) in pair.iter().enumerate() {
                            let mut e = row.spawn((
                                *label,
                                Button,
                                button(),
                                BorderColor::all(palette.unselected_outline()),
                                Selectable,
                            ));
                            e.with_children(|button| {
                                button.spawn((
                                    Text::new(format!(
                                        "{}: {}",
                                        kind.name(),
                                        key_name(kind.get(&config))
                                    )),
                                    font.clone(),
                                    TextColor(palette.text),
                                    RebindDisplay(*kind),
                                ));
                            });
                            if i == 0 && j == 0 {
                                e.insert(Selected);
                            }
                        }
                    });
            }
            spawn_button(screen, Label::Back, "back", false, font.clone(), &palette);
        });
}

pub fn update_drop_zone(
    mut drop_zone: Query<(&mut BorderColor, &Children), With<DropZone>>,
    mut texts: Query<&mut Text>,
    time: Res<Time<Real>>,
    mut error: Local<f32>,
    palette: Res<ColorPalette>,
) {
    let Ok((mut border, children)) = drop_zone.single_mut() else {
        return;
    };
    let was_error = *error > 0.;
    if font_store::take_rejected() {
        *error = ERROR_SECS;
    } else if *error > 0. {
        *error = (*error - time.delta_secs()).max(0.);
    }
    let now_error = *error > 0.;
    if now_error != was_error {
        let label = if now_error {
            FONT_ERROR_LABEL
        } else {
            FONT_DROP_LABEL
        };
        for child in children.iter() {
            if let Ok(mut text) = texts.get_mut(child) {
                text.0 = label.into();
            }
        }
    }
    *border = BorderColor::all(if now_error {
        ERROR_COLOR
    } else {
        palette.unselected_outline()
    });
}

fn add_volume(
    par: &mut RelatedSpawnerCommands<ChildOf>,
    config: &Config,
    font: &Handle<Font>,
    palette: &ColorPalette,
) {
    par.spawn((
        Node {
            margin: UiRect::right(Val::Percent(1.)),
            ..default()
        },
        Text::new(format!("{} {}", VOLUME_LABEL_PREFIX, config.volume)),
        text_font(font),
        TextColor(palette.label()),
        VolumeDisplay,
    ));
    par.spawn((
        Node {
            width: Val::Vw(40.),
            height: Val::Vh(3.),
            justify_content: JustifyContent::FlexStart,
            align_items: AlignItems::Center,
            ..button()
        },
        VolumeControl,
        BorderColor::all(palette.unselected_outline()),
        Button,
        Label::Volume,
        RelativeCursorPosition::default(),
        Selectable,
        Selected,
    ))
    .with_children(|volume_bar| {
        volume_bar.spawn((
            Node {
                width: Val::Percent(config.volume as f32),
                height: Val::Vh(1.),
                ..default()
            },
            BackgroundColor(palette.volume_bar()),
            VolumeControlBar,
        ));
    });
}

pub fn credits_setup(mut commands: Commands, app_font: Res<AppFont>, palette: Res<ColorPalette>) {
    let section_font = text_font(&app_font.0);
    let font = small_font(&app_font.0);
    commands
        .spawn(screen_with(CreditsScreen))
        .with_children(|screen| {
            screen
                .spawn(Node {
                    flex_direction: FlexDirection::Row,
                    justify_content: JustifyContent::Center,
                    align_items: AlignItems::Start,
                    ..default()
                })
                .with_children(|row| {
                    row.spawn(Node {
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        ..default()
                    })
                    .with_children(|left| {
                        left.spawn(Node {
                            flex_direction: FlexDirection::Column,
                            align_items: AlignItems::Center,
                            ..default()
                        })
                        .with_children(|col| {
                            col.spawn((
                                Text::new("programming"),
                                section_font.clone(),
                                TextColor(palette.label()),
                                Node {
                                    margin: UiRect::bottom(Val::Vh(1.)),
                                    ..default()
                                },
                            ));
                            spawn_button(
                                col,
                                Label::ProgrammingLanguage,
                                credits::PROGRAMMING_LANGUAGE_TEXT,
                                true,
                                font.clone(),
                                &palette,
                            );
                            spawn_button(
                                col,
                                Label::GameEngine,
                                credits::GAME_ENGINE_TEXT,
                                false,
                                font.clone(),
                                &palette,
                            );
                            spawn_button(
                                col,
                                Label::Source,
                                credits::SOURCE_TEXT,
                                false,
                                font.clone(),
                                &palette,
                            );
                        });
                        left.spawn(Node {
                            flex_direction: FlexDirection::Column,
                            align_items: AlignItems::Center,
                            ..default()
                        })
                        .with_children(|col| {
                            col.spawn((
                                Text::new("visuals"),
                                section_font.clone(),
                                TextColor(palette.label()),
                                Node {
                                    margin: UiRect::bottom(Val::Vh(1.)),
                                    ..default()
                                },
                            ));
                            spawn_button(
                                col,
                                Label::SpaceGrotesk,
                                credits::SPACE_GROTESK_TEXT,
                                false,
                                font.clone(),
                                &palette,
                            );
                            spawn_button(
                                col,
                                Label::Palette,
                                credits::PALETTE_TEXT,
                                false,
                                font.clone(),
                                &palette,
                            );
                        });
                    });
                    row.spawn(Node {
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        ..default()
                    })
                    .with_children(|col| {
                        col.spawn((
                            Text::new("tools"),
                            section_font.clone(),
                            TextColor(palette.label()),
                            Node {
                                margin: UiRect::bottom(Val::Vh(1.)),
                                ..default()
                            },
                        ));
                        spawn_button(
                            col,
                            Label::Csound,
                            credits::CSOUND_TEXT,
                            false,
                            font.clone(),
                            &palette,
                        );
                        spawn_button(
                            col,
                            Label::FontForge,
                            credits::FONTFORGE_TEXT,
                            false,
                            font.clone(),
                            &palette,
                        );
                        spawn_button(
                            col,
                            Label::Wordfreq,
                            credits::WORDS_TEXT,
                            false,
                            font.clone(),
                            &palette,
                        );
                        spawn_button(
                            col,
                            Label::GWordList,
                            credits::GWORDLIST_TEXT,
                            false,
                            font.clone(),
                            &palette,
                        );
                        spawn_button(
                            col,
                            Label::FFmpeg,
                            credits::FFMPEG_TEXT,
                            false,
                            font.clone(),
                            &palette,
                        );
                    });
                });
            spawn_button(screen, Label::Back, "back", false, font.clone(), &palette);
        });
}

impl Config {
    pub fn set_vol(&mut self, val: u8, global_volume: &mut GlobalVolume) {
        self.volume = val;
        global_volume.volume = bevy::audio::Volume::Linear(self.volume as f32 / 100.);
    }
    fn inc_vol(&mut self, val: u8, gv: &mut GlobalVolume) {
        self.set_vol((self.volume + val).min(100), gv);
    }
    fn dec_vol(&mut self, val: u8, gv: &mut GlobalVolume) {
        self.set_vol(self.volume.saturating_sub(val), gv);
    }
}

fn nav_pick(
    sel: Entity,
    origin: Vec2,
    dir: Vec2,
    cells: &Query<(Entity, &UiGlobalTransform), With<Selectable>>,
) -> Option<Entity> {
    const EPS: f32 = 1.0;
    const ROW_TOL: f32 = 30.0;
    let component = |tf: &UiGlobalTransform| {
        let delta = tf.translation - origin;
        let along = delta.dot(dir);
        (along, (delta - dir * along).length())
    };
    let horizontal = dir.x.abs() > 0.5;

    let ahead = cells
        .iter()
        .filter(|(e, _)| *e != sel)
        .filter_map(|(e, tf)| {
            let (along, perp) = component(tf);
            if horizontal {
                (along > EPS && perp < ROW_TOL).then_some((e, along))
            } else {
                (along > EPS).then_some((e, along + 0.7 * perp))
            }
        });
    if let Some((e, _)) = ahead.min_by(|a, b| a.1.total_cmp(&b.1)) {
        return Some(e);
    }

    cells
        .iter()
        .filter(|(e, _)| *e != sel)
        .filter_map(|(e, tf)| {
            let (along, perp) = component(tf);
            (!horizontal || perp < ROW_TOL).then_some((e, along))
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .map(|(e, _)| e)
}

pub fn grid_navigate(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
    mut global_volume: ResMut<GlobalVolume>,
    input_focus: Res<InputFocus>,
    color_inputs: Query<(), With<ColorChannel>>,
    key: Res<KeyState>,
    rebinding: Res<Rebinding>,
    selected: Single<(Entity, Option<&Label>), With<Selected>>,
    cells: Query<(Entity, &UiGlobalTransform), With<Selectable>>,
) {
    if rebinding.kind.is_some() || input_focus.get().is_some_and(|f| color_inputs.contains(f)) {
        return;
    }
    let (sel_ent, sel_label) = *selected;

    let right = keys.any_just_pressed([KeyCode::ArrowRight, config.right])
        || (key.should_repeat && key.h == Some(true));
    let left = keys.any_just_pressed([KeyCode::ArrowLeft, config.left])
        || (key.should_repeat && key.h == Some(false));
    let down = keys.any_just_pressed([KeyCode::ArrowDown, config.down])
        || (key.should_repeat && key.v == Some(true));
    let up = keys.any_just_pressed([KeyCode::ArrowUp, config.up])
        || (key.should_repeat && key.v == Some(false));

    let captures_h = matches!(sel_label, Some(Label::Volume));
    if captures_h && (left || right) {
        if right {
            config.inc_vol(5, &mut global_volume);
        } else {
            config.dec_vol(5, &mut global_volume);
        }
    }

    let dir = if up {
        Vec2::NEG_Y
    } else if down {
        Vec2::Y
    } else if right && !captures_h {
        Vec2::X
    } else if left && !captures_h {
        Vec2::NEG_X
    } else {
        return;
    };

    let Ok((_, sel_tf)) = cells.get(sel_ent) else {
        return;
    };
    let origin = sel_tf.translation;

    if let Some(target) = nav_pick(sel_ent, origin, dir, &cells) {
        commands.entity(sel_ent).remove::<Selected>();
        commands.entity(target).insert(Selected);
    }
}

pub fn grid_action(
    keys: Res<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
    mut palette: ResMut<ColorPalette>,
    mut input_focus: ResMut<InputFocus>,
    mut rebinding: ResMut<Rebinding>,
    mut pause_msg: MessageWriter<PauseMsg>,
    mut max_difficulty_toggle: Query<&mut BackgroundColor, With<MaxDifToggle>>,
    mut vtime: ResMut<Time<Virtual>>,
    screen: Res<State<Screen>>,
    game_screen: Res<State<GameScreen>>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut next_game_screen: ResMut<NextState<GameScreen>>,
    mut cells: Query<(
        Entity,
        Option<&Label>,
        Option<&ColorChannel>,
        Option<&mut EditableText>,
        Has<Selected>,
    )>,
) {
    if rebinding.kind.is_some() {
        return;
    }
    let enter = keys.just_pressed(KeyCode::Enter);
    let back = keys.just_pressed(config.back) || keys.just_pressed(KeyCode::Escape);

    let editing = input_focus
        .get()
        .filter(|&f| cells.get(f).map(|t| t.2.is_some()).unwrap_or(false));
    if let Some(focused) = editing {
        if enter || back {
            if back
                && let Ok((_, _, Some(&ColorChannel { field, channel }), Some(mut editable), _)) =
                    cells.get_mut(focused)
            {
                let v = channel_u8(palette.color_for_field(field), channel);
                editable.editor.set_text(&v.to_string());
            }
            input_focus.clear();
        }
        return;
    }

    let action = if back && **game_screen != GameScreen::Pause {
        Some(Label::Back)
    } else if enter || keys.just_pressed(config.select) {
        let sel = cells.iter().find_map(|(e, _, _, _, s)| s.then_some(e));
        let mut label_action = None;
        if let Some(sel) = sel
            && let Ok((_, label, channel, editable, _)) = cells.get_mut(sel)
        {
            if channel.is_some() {
                if let Some(mut e) = editable {
                    e.queue_edit(TextEdit::SelectAll);
                }
                input_focus.set(sel, FocusCause::Navigated);
            } else {
                label_action = label.copied();
            }
        }
        label_action
    } else {
        None
    };

    if let Some(label) = action {
        do_action(
            label,
            &mut pause_msg,
            &screen,
            &game_screen,
            &mut next_screen,
            &mut next_game_screen,
            &mut max_difficulty_toggle,
            &mut vtime,
            &mut config,
            &mut palette,
            &mut rebinding,
        );
    }
}

pub fn capture_rebind(
    mut rebinding: ResMut<Rebinding>,
    mut keyboard: MessageReader<KeyboardInput>,
    mut keys: ResMut<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
) {
    let Some(kind) = rebinding.kind else {
        keyboard.clear();
        return;
    };
    if !rebinding.ready {
        rebinding.ready = true;
        keyboard.clear();
        return;
    }
    for ev in keyboard.read() {
        if ev.state != ButtonState::Pressed {
            continue;
        }
        kind.set(&mut config, ev.key_code);
        keys.reset(ev.key_code);
        rebinding.kind = None;
        break;
    }
}

pub fn sync_rebind_text(
    rebinding: Res<Rebinding>,
    config: Res<Config>,
    mut displays: Query<(&RebindDisplay, &mut Text)>,
) {
    for (display, mut text) in &mut displays {
        let value = if rebinding.kind == Some(display.0) {
            "press a key".to_string()
        } else {
            key_name(display.0.get(&config))
        };
        let s = format!("{}: {value}", display.0.name());
        if text.0 != s {
            *text = Text::new(s);
        }
    }
}

pub fn volume_start_drag(
    mut drag: ResMut<VolumeDrag>,
    buttons: Res<ButtonInput<MouseButton>>,
    interactions: Query<&Interaction, (Changed<Interaction>, With<VolumeControl>)>,
) {
    for interaction in interactions {
        if let Interaction::Pressed = *interaction {
            drag.0 = true;
        }
    }
    if drag.0 {
        drag.0 = !buttons.just_released(MouseButton::Left);
    }
}

pub fn volume_drag_control(
    mut config: ResMut<Config>,
    drag: Res<VolumeDrag>,
    bar_rcp: Single<&RelativeCursorPosition, With<VolumeControl>>,
    mut global_volume: ResMut<GlobalVolume>,
) {
    if !drag.0 {
        return;
    }

    if let Some(p) = bar_rcp.normalized {
        let percent = ((p.x + 0.5) * 100.).clamp(0., 100.);
        config.set_vol(percent as u8, &mut global_volume);
    }
}

pub fn sync_settings_colors(
    palette: Res<ColorPalette>,
    config: Res<Config>,
    mut borders: Query<(Has<Selected>, &mut BorderColor), Without<MaxDifToggle>>,
    mut toggle: Query<(&mut BorderColor, &mut BackgroundColor), With<MaxDifToggle>>,
    mut volume_bar: Query<&mut BackgroundColor, (With<VolumeControlBar>, Without<MaxDifToggle>)>,
) {
    if !palette.is_changed() {
        return;
    }
    for (selected, mut border) in &mut borders {
        *border = BorderColor::all(if selected {
            palette.selected_outline()
        } else {
            palette.unselected_outline()
        });
    }
    if let Ok((mut border, mut bg)) = toggle.single_mut() {
        *border = BorderColor::all(palette.selected_outline());
        bg.0 = if config.max_difficulty {
            palette.selected_outline()
        } else {
            Color::NONE
        };
    }
    if let Ok(mut bg) = volume_bar.single_mut() {
        bg.0 = palette.volume_bar();
    }
}

pub fn update_volume_bar(
    config: Res<Config>,
    bar: Single<&mut Node, With<VolumeControlBar>>,
    display: Single<&mut Text, With<VolumeDisplay>>,
) {
    if config.is_changed() {
        let bar: &mut Node = &mut bar.into_inner();
        bar.width = Val::Percent(config.volume as f32);
        let display_text: &mut Text = &mut display.into_inner();
        **display_text = format!("{} {}", VOLUME_LABEL_PREFIX, config.volume);
    }
}

pub fn resume_countdown(
    countdown: Single<(&mut ResumeCountdown, &mut Text)>,
    rtime: Res<Time<Real>>,
    mut next_screen: ResMut<NextState<GameScreen>>,
    mut vtime: ResMut<Time<Virtual>>,
) {
    let (mut c, mut t) = countdown.into_inner();
    c.counter -= rtime.delta_secs();
    if c.counter < (c.displayed - 1) as f32 {
        if c.displayed <= 1 {
            next_screen.set(GameScreen::Running);
            vtime.unpause();
        } else {
            c.displayed -= 1;
            **t = c.displayed.to_string();
        }
    }
}

fn screen_node() -> Node {
    Node {
        justify_self: JustifySelf::Center,
        align_self: AlignSelf::Center,
        justify_content: JustifyContent::Center,
        align_items: AlignItems::Center,
        flex_direction: FlexDirection::Column,
        ..default()
    }
}

fn screen_with<T: Component>(c: T) -> (Node, T) {
    (screen_node(), c)
}

fn ingame_screen<T: Component>(c: T, palette: &ColorPalette) -> (T, Node, BackgroundColor) {
    (
        c,
        Node {
            padding: UiRect::axes(Val::Percent(1.), Val::Percent(0.)),
            border_radius: BorderRadius::all(Val::Percent(5.)),
            ..screen_node()
        },
        BackgroundColor(palette.in_game_menu()),
    )
}

#[derive(Component, Clone, Copy)]
pub struct ColorChannel {
    pub field: ColorField,
    pub channel: usize,
}

fn color_row_node() -> Node {
    Node {
        flex_direction: FlexDirection::Row,
        align_items: AlignItems::Center,
        column_gap: Val::Vw(COLOR_GRID_GAP),
        margin: UiRect::vertical(Val::Px(COLOR_ROW_MARGIN)),
        ..default()
    }
}

fn channel_header(
    parent: &mut RelatedSpawnerCommands<ChildOf>,
    font: &TextFont,
    palette: &ColorPalette,
) {
    parent.spawn(color_row_node()).with_children(|row| {
        row.spawn(Node {
            width: Val::Vw(COLOR_LABEL_WIDTH),
            ..default()
        });
        row.spawn(Node {
            width: Val::Px(COLOR_SWATCH_SIZE),
            ..default()
        });
        for label in CHANNELS {
            row.spawn((
                Text::new(label),
                font.clone(),
                TextColor(palette.label()),
                TextLayout::justify(Justify::Center),
                Node {
                    width: Val::Vw(COLOR_CHANNEL_WIDTH),
                    ..default()
                },
            ));
        }
    });
}

fn color_row(
    parent: &mut RelatedSpawnerCommands<ChildOf>,
    field: ColorField,
    font: &TextFont,
    palette: &ColorPalette,
) {
    parent.spawn(color_row_node()).with_children(|row| {
        row.spawn((
            Text::new(field.label()),
            font.clone(),
            TextColor(palette.label()),
            TextLayout::no_wrap(),
            Node {
                width: Val::Vw(COLOR_LABEL_WIDTH),
                ..default()
            },
        ));
        row.spawn((
            ColorSwatch,
            field,
            Node {
                width: Val::Px(COLOR_SWATCH_SIZE),
                height: Val::Px(COLOR_SWATCH_SIZE),
                border: UiRect::all(Val::Px(COLOR_SWATCH_BORDER)),
                border_radius: BorderRadius::all(Val::Px(COLOR_RADIUS)),
                ..default()
            },
            BackgroundColor(palette.color_for_field(field)),
            BorderColor::all(palette.unselected_outline()),
        ));
        let color = palette.color_for_field(field);
        for channel in 0..CHANNELS.len() {
            let mut editable = EditableText::new(channel_u8(color, channel).to_string());
            editable.max_characters = Some(3);
            let mut cell = row.spawn((
                ColorChannel { field, channel },
                editable,
                EditableTextFilter::new(|c| c.is_ascii_digit()),
                TextCursorStyle {
                    color: palette.text_next,
                    selection_color: palette.highlight,
                    unfocused_selection_color: Color::NONE,
                    selected_text_color: Some(palette.background),
                },
                font.clone(),
                LineHeight::RelativeToFont(1.0),
                TextColor(palette.label()),
                Node {
                    width: Val::Vw(COLOR_CHANNEL_WIDTH),
                    border: UiRect::all(Val::Px(COLOR_CELL_BORDER)),
                    border_radius: BorderRadius::all(Val::Px(COLOR_RADIUS)),
                    padding: UiRect::axes(Val::Px(COLOR_CELL_PAD_X), Val::Px(COLOR_CELL_PAD_Y)),
                    ..default()
                },
                BorderColor::all(palette.unselected_outline()),
                BackgroundColor(Color::NONE),
                Selectable,
                Interaction::default(),
            ));
            if field.index() == 0 && channel == 0 {
                cell.insert(Selected);
            }
        }
    });
}

pub fn color_settings_setup(
    mut commands: Commands,
    palette: Res<ColorPalette>,
    app_font: Res<AppFont>,
    mut input_focus: ResMut<InputFocus>,
) {
    input_focus.clear();
    let font = small_font(&app_font.0);
    let half = ColorField::ALL.len().div_ceil(2);
    commands
        .spawn(screen_with(ColorSettingsScreen))
        .with_children(|screen| {
            screen
                .spawn(Node {
                    flex_direction: FlexDirection::Row,
                    column_gap: Val::Px(COLOR_COLUMNS_GAP),
                    align_items: AlignItems::Start,
                    ..default()
                })
                .with_children(|cols| {
                    for chunk in [&ColorField::ALL[..half], &ColorField::ALL[half..]] {
                        cols.spawn(Node {
                            flex_direction: FlexDirection::Column,
                            align_items: AlignItems::Start,
                            ..default()
                        })
                        .with_children(|col| {
                            channel_header(col, &font, &palette);
                            for &field in chunk {
                                color_row(col, field, &font, &palette);
                            }
                        });
                    }
                });
            spawn_button(screen, Label::Back, "back", false, font.clone(), &palette);
        });
}

pub fn sync_swatches(
    palette: Res<ColorPalette>,
    mut swatches: Query<(&ColorField, &mut BackgroundColor), With<ColorSwatch>>,
) {
    if !palette.is_changed() {
        return;
    }
    for (&field, mut bg) in &mut swatches {
        bg.0 = palette.color_for_field(field);
    }
}

pub fn sync_text_color(palette: Res<ColorPalette>, mut texts: Query<&mut TextColor>) {
    if !palette.is_changed() {
        return;
    }
    for mut color in &mut texts {
        color.0 = palette.text;
    }
}

pub fn sync_cursor_style(
    palette: Res<ColorPalette>,
    mut cursors: Query<&mut TextCursorStyle, With<ColorChannel>>,
) {
    if !palette.is_changed() {
        return;
    }
    for mut style in &mut cursors {
        style.color = palette.text_next;
        style.selection_color = palette.highlight;
        style.selected_text_color = Some(palette.background);
    }
}

pub fn color_input_borders(
    palette: Res<ColorPalette>,
    input_focus: Res<InputFocus>,
    mut inputs: Query<(Entity, &Interaction, Has<Selected>, &mut BorderColor), With<ColorChannel>>,
) {
    let focused = input_focus.get();
    for (entity, interaction, selected, mut border) in &mut inputs {
        let color = if Some(entity) == focused {
            palette.text_next
        } else if selected || *interaction != Interaction::None {
            palette.selected_outline()
        } else {
            palette.unselected_outline()
        };
        *border = BorderColor::all(color);
    }
}

pub fn color_click(
    mut commands: Commands,
    mut input_focus: ResMut<InputFocus>,
    mut clicked: Query<
        (Entity, &Interaction, &mut EditableText),
        (Changed<Interaction>, With<ColorChannel>),
    >,
    selected: Query<Entity, With<Selected>>,
) {
    for (entity, interaction, mut editable) in &mut clicked {
        if *interaction == Interaction::Pressed {
            for sel in &selected {
                commands.entity(sel).remove::<Selected>();
            }
            commands.entity(entity).insert(Selected);
            editable.queue_edit(TextEdit::SelectAll);
            input_focus.set(entity, FocusCause::Pressed);
        }
    }
}

pub fn commit_on_focus_change(
    input_focus: Res<InputFocus>,
    mut prev: Local<Option<Entity>>,
    mut inputs: Query<(&ColorChannel, &mut EditableText)>,
    mut palette: ResMut<ColorPalette>,
    mut config: ResMut<Config>,
) {
    let current = input_focus.get();
    if *prev == current {
        return;
    }
    if let Some(old) = *prev
        && let Ok((&ColorChannel { field, channel }, mut editable)) = inputs.get_mut(old)
    {
        if let Ok(val) = editable.value().to_string().parse::<u32>() {
            let color = with_channel(palette.color_for_field(field), channel, val.min(255) as u8);
            palette.set_color_for_field(field, color);
            config.set_color_field(field, color);
        }
        let v = channel_u8(palette.color_for_field(field), channel);
        editable.editor.set_text(&v.to_string());
    }
    *prev = current;
}

#[cfg(not(target_arch = "wasm32"))]
pub fn handle_file_drop(mut events: MessageReader<FileDragAndDrop>) {
    for event in events.read() {
        if let FileDragAndDrop::DroppedFile { path_buf, .. } = event {
            let Ok(bytes) = std::fs::read(path_buf) else {
                continue;
            };
            font_store::accept_drop(bytes);
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn setup_web_drag_drop() {
    use wasm_bindgen::{JsCast, closure::Closure};
    use web_sys::{DragEvent, Event, FileReader};
    let document = web_sys::window().unwrap().document().unwrap();
    let dragover = Closure::<dyn FnMut(DragEvent)>::new(|e: DragEvent| {
        e.prevent_default();
    });
    document
        .add_event_listener_with_callback("dragover", dragover.as_ref().unchecked_ref())
        .unwrap();
    dragover.forget();
    let drop = Closure::<dyn FnMut(DragEvent)>::new(|e: DragEvent| {
        e.prevent_default();
        let Some(dt) = e.data_transfer() else { return };
        let Some(files) = dt.files() else { return };
        let Some(file) = files.get(0) else { return };
        let reader = FileReader::new().unwrap();
        let reader2 = reader.clone();
        let cb = Closure::<dyn FnMut(Event)>::new(move |_: Event| {
            let buf = reader2.result().unwrap();
            let bytes = js_sys::Uint8Array::new(&buf).to_vec();
            font_store::accept_drop(bytes);
        });
        reader.set_onload(Some(cb.as_ref().unchecked_ref()));
        reader.read_as_array_buffer(&file).unwrap();
        cb.forget();
    });
    document
        .add_event_listener_with_callback("drop", drop.as_ref().unchecked_ref())
        .unwrap();
    drop.forget();
}
