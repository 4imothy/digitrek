// SPDX-License-Identifier: MIT

use crate::*;
use bevy::{ecs::relationship::RelatedSpawnerCommands, prelude::*, ui::RelativeCursorPosition};

const VOLUME_LABEL_PREFIX: &str = "volume:";

#[derive(Component, PartialEq, Clone, Copy)]
pub enum Label {
    Play,
    Settings,
    Credits,
    Volume,
    PhysicalKeyboardLayout,
    PhysicalQwerty,
    PhysicalDvorak,
    PhysicalColemak,
    LogicalKeyboardLayout,
    LogicalQwerty,
    LogicalDvorak,
    LogicalColemak,
    MaxDifficulty,
    ProgrammingLanguage,
    GameEngine,
    Palette,
    LaunchProjectileSound,
    ExplosionSound,
    MistypeSound,
    SpaceGrotesk,
    FontForge,
    FFmpeg,
    Source,
    Help,
    Resume,
    GameSettings,
    PlayAgain,
    Back,
    Quit,
}

impl Label {
    fn to_layout(self) -> Option<KeyboardLayouts> {
        match self {
            Label::PhysicalQwerty | Label::LogicalQwerty => Some(KeyboardLayouts::Qwerty),
            Label::PhysicalDvorak | Label::LogicalDvorak => Some(KeyboardLayouts::Dvorak),
            Label::PhysicalColemak | Label::LogicalColemak => Some(KeyboardLayouts::Colemak),
            _ => None,
        }
    }

    fn to_link(self) -> &'static str {
        match self {
            Label::ProgrammingLanguage => credits::PROGRAMMING_LANGUAGE_LINK,
            Label::GameEngine => credits::GAME_ENGINE_LINK,
            Label::Palette => credits::PALETTE_LINK,
            Label::LaunchProjectileSound => credits::LAUNCH_PROJECTILE_SOUND_LINK,
            Label::ExplosionSound => credits::EXPLOSION_SOUND_LINK,
            Label::MistypeSound => credits::MISTYPE_SOUND_LINK,
            Label::SpaceGrotesk => credits::SPACE_GROTESK_LINK,
            Label::FontForge => credits::FONTFORGE_LINK,
            Label::FFmpeg => credits::FFMPEG_LINK,
            Label::Source => credits::SOURCE_LINK,
            _ => unreachable!(),
        }
    }
}

impl KeyboardLayouts {
    fn nav(&self, logical: bool, right: bool) -> Label {
        match (self, logical, right) {
            (KeyboardLayouts::Qwerty, false, false) => Label::PhysicalColemak,
            (KeyboardLayouts::Qwerty, false, true) => Label::PhysicalDvorak,
            (KeyboardLayouts::Qwerty, true, false) => Label::LogicalColemak,
            (KeyboardLayouts::Qwerty, true, true) => Label::LogicalDvorak,
            (KeyboardLayouts::Dvorak, false, false) => Label::PhysicalQwerty,
            (KeyboardLayouts::Dvorak, false, true) => Label::PhysicalColemak,
            (KeyboardLayouts::Dvorak, true, false) => Label::LogicalQwerty,
            (KeyboardLayouts::Dvorak, true, true) => Label::LogicalColemak,
            (KeyboardLayouts::Colemak, false, false) => Label::PhysicalDvorak,
            (KeyboardLayouts::Colemak, false, true) => Label::PhysicalQwerty,
            (KeyboardLayouts::Colemak, true, false) => Label::LogicalDvorak,
            (KeyboardLayouts::Colemak, true, true) => Label::LogicalQwerty,
        }
    }
    fn right(&self, logical: bool) -> Label {
        self.nav(logical, true)
    }
    fn left(&self, logical: bool) -> Label {
        self.nav(logical, false)
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
) {
    let mut e = cmd.spawn((
        Button,
        button(),
        BorderColor::all(colors::UNSELECTED_OUTLINE),
        action,
    ));
    e.with_children(|p| {
        p.spawn((Text::new(text), font, TextColor(colors::BUTTON_TEXT)));
    });
    if selected {
        e.insert(Selected);
    }
}

pub fn menu_setup(mut commands: Commands, config: Res<Config>) {
    let title_font = title_font();
    let font = text_font();
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
                            TextColor(colors::TITLE_CHARS[i]),
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
                TextColor(colors::LABEL),
            ));

            spawn_button(cmd, Label::Play, "play", true, font.clone());
            spawn_button(cmd, Label::Help, "help", false, font.clone());
            spawn_button(cmd, Label::Settings, "settings", false, font.clone());
            spawn_button(cmd, Label::Credits, "credits", false, font.clone());
            #[cfg(not(target_arch = "wasm32"))]
            spawn_button(cmd, Label::Quit, "quit", false, font.clone());
        });
}

pub fn on_selection(
    selection: Single<Entity, Added<Selected>>,
    mut removed: RemovedComponents<Selected>,
    mut borders: Query<&mut BorderColor>,
) {
    for entity in removed.read() {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor::all(colors::UNSELECTED_OUTLINE);
        }
    }
    if let Ok(mut new) = borders.get_mut(*selection) {
        *new = BorderColor::all(colors::SELECTED_OUTLINE);
    }
}

pub fn on_active(
    active: Query<Entity, Added<Active>>,
    mut removed: RemovedComponents<Active>,
    mut borders: Query<&mut BorderColor>,
) {
    for entity in removed.read() {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor::all(colors::UNSELECTED_OUTLINE);
        }
    }
    for entity in active {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor::all(colors::SELECTED_OUTLINE);
        }
    }
}

pub fn mouse(
    mut commands: Commands,
    mut interaction_query: Query<(&Interaction, &Label), (Changed<Interaction>, With<Button>)>,
    mut hovers: Query<
        (&Interaction, &mut BorderColor),
        (
            Changed<Interaction>,
            With<Button>,
            Without<Selected>,
            Without<Active>,
        ),
    >,
    keyboard_options: Query<(Entity, &Label), With<KeyboardOption>>,
    active: Query<(Entity, &Label), With<Active>>,
    mut max_difficulty_toggle: Query<&mut BackgroundColor, With<MaxDifToggle>>,
    mut app_exit_msg: MessageWriter<AppExit>,
    mut pause_msg: MessageWriter<PauseMsg>,
    screen: Res<State<Screen>>,
    game_screen: Res<State<GameScreen>>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut next_game_screen: ResMut<NextState<GameScreen>>,
    mut vtime: ResMut<Time<Virtual>>,
    mut pkv: ResMut<PkvStore>,
    mut config: ResMut<Config>,
) {
    for (interaction, menu_button_action) in interaction_query.iter_mut() {
        if *interaction == Interaction::Pressed {
            do_action(
                *menu_button_action,
                &mut commands,
                &mut app_exit_msg,
                &mut pause_msg,
                &screen,
                &game_screen,
                &mut next_screen,
                &mut next_game_screen,
                &active,
                &keyboard_options,
                &mut max_difficulty_toggle,
                &mut vtime,
                &mut pkv,
                &mut config,
            );
        }
    }
    for (interaction, mut border_color) in hovers.iter_mut() {
        if *interaction == Interaction::Hovered {
            *border_color = BorderColor::all(colors::SELECTED_OUTLINE);
        } else if *interaction == Interaction::None {
            *border_color = BorderColor::all(colors::UNSELECTED_OUTLINE);
        }
    }
}

fn do_action(
    action: Label,
    commands: &mut Commands,
    app_exit_msg: &mut MessageWriter<AppExit>,
    pause_msg: &mut MessageWriter<PauseMsg>,
    screen: &State<Screen>,
    game_screen: &State<GameScreen>,
    next_screen: &mut ResMut<NextState<Screen>>,
    next_game_screen: &mut ResMut<NextState<GameScreen>>,
    active: &Query<(Entity, &Label), With<Active>>,
    keyboard_options: &Query<(Entity, &Label), With<KeyboardOption>>,
    max_difficulty_toggle: &mut Query<&mut BackgroundColor, With<MaxDifToggle>>,
    vtime: &mut ResMut<Time<Virtual>>,
    pkv: &mut ResMut<PkvStore>,
    config: &mut ResMut<Config>,
) {
    match action {
        Label::Quit => {
            app_exit_msg.write(AppExit::Success);
        }
        Label::Play => {
            #[cfg(target_arch = "wasm32")]
            web_audio::resume_audio();
            next_screen.set(Screen::Game);
        }
        Label::Settings => next_screen.set(Screen::Settings),
        Label::Help => next_screen.set(Screen::Help),
        Label::Credits => next_screen.set(Screen::Credits),
        Label::Back => match (**screen, **game_screen) {
            (Screen::Settings, _) => {
                next_screen.set(Screen::MainMenu);
                let _ = pkv.set(VOLUME_KEY, &config.volume);
                let _ = pkv.set(
                    PHYSICAL_KEYBOARD_LAYOUT_KEY,
                    &config.physical_keyboard_layout,
                );
                let _ = pkv.set(LOGICAL_KEYBOARD_LAYOUT_KEY, &config.logical_keyboard_layout);
                let _ = pkv.set(MAX_DIFFICULTY_KEY, &config.max_difficulty);
            }
            (Screen::Credits | Screen::Help, _) => {
                next_screen.set(Screen::MainMenu);
            }
            (Screen::Game, GameScreen::Settings) => {
                let _ = pkv.set(VOLUME_KEY, &config.volume);
                next_game_screen.set(GameScreen::Pause);
            }
            (Screen::Game, GameScreen::Pause | GameScreen::End) => {
                vtime.unpause();
                next_screen.set(Screen::MainMenu);
            }
            _ => {}
        },
        Label::PhysicalQwerty | Label::PhysicalDvorak | Label::PhysicalColemak => {
            switch_keyboard_layout(
                commands,
                active,
                keyboard_options,
                action,
                Label::PhysicalKeyboardLayout,
                config,
            )
        }
        Label::LogicalQwerty | Label::LogicalDvorak | Label::LogicalColemak => {
            switch_keyboard_layout(
                commands,
                active,
                keyboard_options,
                action,
                Label::LogicalKeyboardLayout,
                config,
            )
        }
        Label::ProgrammingLanguage
        | Label::GameEngine
        | Label::Palette
        | Label::LaunchProjectileSound
        | Label::ExplosionSound
        | Label::MistypeSound
        | Label::SpaceGrotesk
        | Label::FontForge
        | Label::FFmpeg
        | Label::Source => {
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
            *max_difficulty_toggle.single_mut().ok().unwrap() =
                BackgroundColor(if config.max_difficulty {
                    colors::SELECTED_OUTLINE
                } else {
                    colors::BASE
                });
        }
        _ => {}
    }
}

pub fn despawn_screen<T: Component>(screen: Single<Entity, With<T>>, mut commands: Commands) {
    commands.entity(*screen).despawn();
}

pub fn pause_setup(mut commands: Commands) {
    let font = text_font();
    commands
        .spawn(ingame_screen(PauseScreen))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new("game is paused"),
                TextLayout::new_with_justify(Justify::Center),
                font.clone(),
            ));
            spawn_button(cmd, Label::Resume, "resume", true, font.clone());
            spawn_button(cmd, Label::GameSettings, "settings", false, font.clone());
            spawn_button(cmd, Label::Back, "exit", false, font.clone());
        });
}

pub fn end_setup(mut commands: Commands, stats: Single<&mut Stats>) {
    let font = text_font();
    commands
        .spawn(ingame_screen(EndScreen))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new(format!("game over\nscore: {}", stats.score)),
                TextLayout::new_with_justify(Justify::Center),
                font.clone(),
            ));
            spawn_button(cmd, Label::PlayAgain, "play again", true, font.clone());
            spawn_button(cmd, Label::Back, "exit", false, font.clone());
        });
}

pub fn resume_countdown_setup(mut commands: Commands) {
    commands.spawn((
        ResumeCountdown {
            counter: TIME_BEFORE_RESUME,
            displayed: TIME_BEFORE_RESUME as usize,
        },
        Node {
            justify_self: JustifySelf::Center,
            align_self: AlignSelf::Center,
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            padding: UiRect::new(
                Val::Percent(1.),
                Val::Percent(1.),
                Val::Percent(0.),
                Val::Percent(0.),
            ),
            border_radius: BorderRadius::all(Val::Percent(100.)),
            ..default()
        },
        BackgroundColor(colors::IN_GAME_MENU),
        Text::new((TIME_BEFORE_RESUME as usize).to_string()),
        text_font(),
        TextLayout::new_with_justify(Justify::Center),
    ));
}

pub fn help_setup(mut commands: Commands, config: Res<Config>) {
    let font = text_font();
    commands
        .spawn(screen_with(HelpScreen))
        .with_children(|screen| {
            screen.spawn((
                Text::new(format!(
                    "avoid polygons and circles\n\
                     use {up}{left}{down}{right} to move\n\
                     use the rest of the keyboard to defeat polygons\n\
                     press space or backspace to stop targeting a polygon",
                    up = config.up_char(),
                    left = config.left_char(),
                    down = config.down_char(),
                    right = config.right_char(),
                )),
                font.clone(),
                TextColor(colors::LABEL),
                TextLayout {
                    justify: Justify::Center,
                    ..default()
                },
            ));
            spawn_button(screen, Label::Back, "back", true, font.clone());
        });
}

pub fn game_settings_setup(mut commands: Commands, config: Res<Config>) {
    let font = text_font();
    let (c, n, mut bg) = ingame_screen(GameSettingsScreen);
    bg.0 = colors::IN_GAME_MENU.with_alpha(1.);
    commands.spawn((c, n, bg)).with_children(|s| {
        add_volume(s, &config);
        spawn_button(s, Label::Back, "back", false, font.clone());
    });
}
pub fn settings_setup(mut commands: Commands, config: Res<Config>) {
    let font = text_font();
    commands
        .spawn(screen_with(SettingsScreen))
        .with_children(|screen| {
            add_volume(screen, &config);
            screen.spawn((
                Node {
                    margin: UiRect::right(Val::Percent(1.)),
                    ..default()
                },
                Text::new("keyboard layout"),
                font.clone(),
                TextColor(colors::LABEL),
            ));

            let layout_groups = [
                (
                    Label::LogicalKeyboardLayout,
                    "logical:",
                    [
                        (
                            Label::LogicalQwerty,
                            "qwerty",
                            matches!(config.logical_keyboard_layout, KeyboardLayouts::Qwerty),
                        ),
                        (
                            Label::LogicalDvorak,
                            "dvorak",
                            matches!(config.logical_keyboard_layout, KeyboardLayouts::Dvorak),
                        ),
                        (
                            Label::LogicalColemak,
                            "colemak",
                            matches!(config.logical_keyboard_layout, KeyboardLayouts::Colemak),
                        ),
                    ],
                ),
                (
                    Label::PhysicalKeyboardLayout,
                    "physical:",
                    [
                        (
                            Label::PhysicalQwerty,
                            "qwerty",
                            matches!(config.physical_keyboard_layout, KeyboardLayouts::Qwerty),
                        ),
                        (
                            Label::PhysicalDvorak,
                            "dvorak",
                            matches!(config.physical_keyboard_layout, KeyboardLayouts::Dvorak),
                        ),
                        (
                            Label::PhysicalColemak,
                            "colemak",
                            matches!(config.physical_keyboard_layout, KeyboardLayouts::Colemak),
                        ),
                    ],
                ),
            ];

            for (group_label, group_text, buttons) in layout_groups {
                let font = font.clone();
                screen
                    .spawn((
                        group_label,
                        Button,
                        Node {
                            flex_direction: FlexDirection::Row,
                            ..button()
                        },
                        BorderColor::all(colors::UNSELECTED_OUTLINE),
                    ))
                    .with_children(|row| {
                        row.spawn((
                            Text::new(group_text),
                            font.clone(),
                            TextColor(colors::LABEL),
                        ));

                        for (button_label, button_text, active) in buttons {
                            let font = font.clone();
                            let mut option = row.spawn((
                                Button,
                                button_label,
                                Node { ..button() },
                                BorderColor::all(colors::UNSELECTED_OUTLINE),
                                KeyboardOption,
                            ));
                            if active {
                                option.insert(Active);
                            }
                            option.with_children(|b| {
                                b.spawn((
                                    Text::new(button_text),
                                    font,
                                    TextColor(colors::BUTTON_TEXT),
                                ));
                            });
                        }
                    });
            }

            screen
                .spawn((
                    Label::MaxDifficulty,
                    Button,
                    BorderColor::all(colors::UNSELECTED_OUTLINE),
                    Node {
                        flex_direction: FlexDirection::Row,
                        ..button()
                    },
                ))
                .with_children(|button| {
                    button.spawn((
                        Text::new("max difficulty"),
                        font.clone(),
                        TextColor(colors::BUTTON_TEXT),
                        TextLayout::new_with_no_wrap(),
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
                        BorderColor::all(colors::SELECTED_OUTLINE),
                        BackgroundColor(if config.max_difficulty {
                            colors::SELECTED_OUTLINE
                        } else {
                            colors::BASE
                        }),
                    ));
                });

            spawn_button(screen, Label::Back, "back", false, font.clone());
        });
}

fn add_volume(par: &mut RelatedSpawnerCommands<ChildOf>, config: &Config) {
    par.spawn((
        Node {
            margin: UiRect::right(Val::Percent(1.)),
            ..default()
        },
        Text::new(format!("{} {}", VOLUME_LABEL_PREFIX, config.volume)),
        text_font(),
        TextColor(colors::LABEL),
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
        BorderColor::all(colors::UNSELECTED_OUTLINE),
        Button,
        Label::Volume,
        RelativeCursorPosition::default(),
        Selected,
    ))
    .with_children(|volume_bar| {
        volume_bar.spawn((
            Node {
                width: Val::Percent(config.volume as f32),
                height: Val::Vh(1.),
                ..default()
            },
            BackgroundColor(colors::VOLUME_BAR),
            VolumeControlBar,
        ));
    });
}

pub fn credits_setup(mut commands: Commands) {
    let title_font = text_font();
    let font = small_font();
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
                    .with_children(|col| {
                        col.spawn((
                            Text::new("tools"),
                            title_font.clone(),
                            TextColor(colors::LABEL),
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
                        );
                        spawn_button(
                            col,
                            Label::GameEngine,
                            credits::GAME_ENGINE_TEXT,
                            false,
                            font.clone(),
                        );
                        spawn_button(
                            col,
                            Label::FontForge,
                            credits::FONTFORGE_TEXT,
                            false,
                            font.clone(),
                        );
                        spawn_button(
                            col,
                            Label::FFmpeg,
                            credits::FFMPEG_TEXT,
                            false,
                            font.clone(),
                        );
                    });
                    row.spawn(Node {
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        ..default()
                    })
                    .with_children(|col| {
                        col.spawn((
                            Text::new("visuals"),
                            title_font.clone(),
                            TextColor(colors::LABEL),
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
                        );
                        spawn_button(
                            col,
                            Label::Palette,
                            credits::PALETTE_TEXT,
                            false,
                            font.clone(),
                        );
                        col.spawn((
                            Text::new("sounds"),
                            title_font.clone(),
                            TextColor(colors::LABEL),
                            Node {
                                margin: UiRect::new(Val::ZERO, Val::ZERO, Val::Vh(2.), Val::Vh(1.)),
                                ..default()
                            },
                        ));
                        spawn_button(
                            col,
                            Label::LaunchProjectileSound,
                            credits::LAUNCH_PROJECTILE_SOUND_TEXT,
                            false,
                            font.clone(),
                        );
                        spawn_button(
                            col,
                            Label::ExplosionSound,
                            credits::EXPLOSION_SOUND_TEXT,
                            false,
                            font.clone(),
                        );
                        spawn_button(
                            col,
                            Label::MistypeSound,
                            credits::MISTYPE_SOUND_TEXT,
                            false,
                            font.clone(),
                        );
                    });
                });
            spawn_button(
                screen,
                Label::Source,
                credits::SOURCE_TEXT,
                false,
                font.clone(),
            );
            spawn_button(screen, Label::Back, "back", false, font.clone());
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

fn switch_keyboard_layout(
    commands: &mut Commands,
    active: &Query<(Entity, &Label), With<Active>>,
    keyboard_options: &Query<(Entity, &Label), With<KeyboardOption>>,
    to: Label,
    parent_label: Label,
    config: &mut ResMut<Config>,
) {
    let from_ent = active
        .iter()
        .find(|(_, label)| {
            matches!(
                (parent_label, label),
                (Label::PhysicalKeyboardLayout, Label::PhysicalQwerty)
                    | (Label::PhysicalKeyboardLayout, Label::PhysicalDvorak)
                    | (Label::PhysicalKeyboardLayout, Label::PhysicalColemak)
                    | (Label::LogicalKeyboardLayout, Label::LogicalQwerty)
                    | (Label::LogicalKeyboardLayout, Label::LogicalDvorak)
                    | (Label::LogicalKeyboardLayout, Label::LogicalColemak)
            )
        })
        .map(|(e, _)| e);

    match parent_label {
        Label::PhysicalKeyboardLayout => config.physical_keyboard_layout = to.to_layout().unwrap(),
        Label::LogicalKeyboardLayout => config.logical_keyboard_layout = to.to_layout().unwrap(),
        _ => {}
    }

    let to_ent = keyboard_options
        .iter()
        .find(|(_, label)| **label == to)
        .map(|(e, _)| e);

    if let Some(from_ent) = from_ent {
        commands.entity(from_ent).remove::<Active>();
    }
    if let Some(to_ent) = to_ent {
        commands.entity(to_ent).insert(Active);
    }
}

pub fn keypress_navigate(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
    active: Query<(Entity, &Label), With<Active>>,
    mut global_volume: ResMut<GlobalVolume>,
    selected: Single<(Entity, &Label), With<Selected>>,
    elements: Query<(Entity, &Label)>,
    keyboard_options: Query<(Entity, &Label), With<KeyboardOption>>,
    screen: Res<State<Screen>>,
    game_screen: Res<State<GameScreen>>,
    key: Res<KeyState>,
) {
    let (selected_ent, selected_label) = *selected;

    let nav_keys_right = [KeyCode::ArrowRight, config.right()];
    let nav_keys_left = [KeyCode::ArrowLeft, config.left()];
    let nav_keys_down = [KeyCode::ArrowDown, config.down()];
    let nav_keys_up = [KeyCode::ArrowUp, config.up()];

    let right_active =
        keys.any_just_pressed(nav_keys_right) || (key.should_repeat && key.h == Some(true));
    let left_active =
        keys.any_just_pressed(nav_keys_left) || (key.should_repeat && key.h == Some(false));
    let down_active =
        keys.any_just_pressed(nav_keys_down) || (key.should_repeat && key.v == Some(true));
    let up_active =
        keys.any_just_pressed(nav_keys_up) || (key.should_repeat && key.v == Some(false));

    if right_active {
        match selected_label {
            Label::Volume => config.inc_vol(5, &mut global_volume),
            Label::PhysicalKeyboardLayout => {
                switch_keyboard_layout(
                    &mut commands,
                    &active,
                    &keyboard_options,
                    config.physical_keyboard_layout.right(false),
                    *selected_label,
                    &mut config,
                );
            }
            Label::LogicalKeyboardLayout => {
                switch_keyboard_layout(
                    &mut commands,
                    &active,
                    &keyboard_options,
                    config.logical_keyboard_layout.right(true),
                    *selected_label,
                    &mut config,
                );
            }
            _ => {}
        }
    }
    if left_active {
        match selected_label {
            Label::Volume => config.dec_vol(5, &mut global_volume),
            Label::PhysicalKeyboardLayout => {
                switch_keyboard_layout(
                    &mut commands,
                    &active,
                    &keyboard_options,
                    config.physical_keyboard_layout.left(false),
                    *selected_label,
                    &mut config,
                );
            }
            Label::LogicalKeyboardLayout => {
                switch_keyboard_layout(
                    &mut commands,
                    &active,
                    &keyboard_options,
                    config.logical_keyboard_layout.left(true),
                    *selected_label,
                    &mut config,
                );
            }
            _ => {}
        }
    }
    let v_dir = if down_active {
        1
    } else if up_active {
        -1
    } else {
        0
    };
    let h_dir = if right_active {
        1
    } else if left_active {
        -1
    } else {
        0
    };

    let next_selection = match (selected_label, v_dir, h_dir) {
        (Label::ProgrammingLanguage, 0, 1) => Some(Label::SpaceGrotesk),
        (Label::SpaceGrotesk, 0, -1) => Some(Label::ProgrammingLanguage),
        (Label::GameEngine, 0, 1) => Some(Label::Palette),
        (Label::Palette, 0, -1) => Some(Label::GameEngine),
        (Label::FontForge, 0, 1) => Some(Label::LaunchProjectileSound),
        (Label::LaunchProjectileSound, 0, -1) => Some(Label::FontForge),
        (Label::FFmpeg, 0, 1) => Some(Label::ExplosionSound),
        (Label::ExplosionSound, 0, -1) => Some(Label::FFmpeg),
        (Label::Play, 1, _) => Some(Label::Help),
        (Label::Play, -1, _) => {
            if cfg!(target_arch = "wasm32") {
                Some(Label::Credits)
            } else {
                Some(Label::Quit)
            }
        }
        (Label::Help, 1, _) => Some(Label::Settings),
        (Label::Help, -1, _) => Some(Label::Play),
        (Label::Settings, 1, _) => Some(Label::Credits),
        (Label::Settings, -1, _) => Some(Label::Help),
        (Label::Credits, 1, _) => {
            if cfg!(target_arch = "wasm32") {
                Some(Label::Play)
            } else {
                Some(Label::Quit)
            }
        }
        (Label::Credits, -1, _) => Some(Label::Settings),
        #[cfg(not(target_arch = "wasm32"))]
        (Label::Quit, 1, _) => Some(Label::Play),
        #[cfg(not(target_arch = "wasm32"))]
        (Label::Quit, -1, _) => Some(Label::Credits),
        (Label::Volume, 1, _) => {
            if let Screen::Game = **screen {
                Some(Label::Back)
            } else {
                Some(Label::LogicalKeyboardLayout)
            }
        }
        (Label::Volume, -1, _) => Some(Label::Back),
        (Label::LogicalKeyboardLayout, 1, _) => Some(Label::PhysicalKeyboardLayout),
        (Label::LogicalKeyboardLayout, -1, _) => Some(Label::Volume),
        (Label::PhysicalKeyboardLayout, 1, _) => Some(Label::MaxDifficulty),
        (Label::PhysicalKeyboardLayout, -1, _) => Some(Label::LogicalKeyboardLayout),
        (Label::MaxDifficulty, 1, _) => Some(Label::Back),
        (Label::MaxDifficulty, -1, _) => Some(Label::PhysicalKeyboardLayout),
        (Label::ProgrammingLanguage, 1, _) => Some(Label::GameEngine),
        (Label::ProgrammingLanguage, -1, _) => Some(Label::Back),
        (Label::GameEngine, 1, _) => Some(Label::FontForge),
        (Label::GameEngine, -1, _) => Some(Label::ProgrammingLanguage),
        (Label::FontForge, 1, _) => Some(Label::FFmpeg),
        (Label::FontForge, -1, _) => Some(Label::GameEngine),
        (Label::FFmpeg, 1, _) => Some(Label::Source),
        (Label::FFmpeg, -1, _) => Some(Label::FontForge),
        (Label::SpaceGrotesk, 1, _) => Some(Label::Palette),
        (Label::SpaceGrotesk, -1, _) => Some(Label::Back),
        (Label::Palette, 1, _) => Some(Label::LaunchProjectileSound),
        (Label::Palette, -1, _) => Some(Label::SpaceGrotesk),
        (Label::LaunchProjectileSound, 1, _) => Some(Label::ExplosionSound),
        (Label::LaunchProjectileSound, -1, _) => Some(Label::Palette),
        (Label::ExplosionSound, 1, _) => Some(Label::MistypeSound),
        (Label::ExplosionSound, -1, _) => Some(Label::LaunchProjectileSound),
        (Label::MistypeSound, 1, _) => Some(Label::Source),
        (Label::MistypeSound, -1, _) => Some(Label::ExplosionSound),
        (Label::Source, 1, _) => Some(Label::Back),
        (Label::Source, -1, _) => Some(Label::MistypeSound),
        (Label::Resume, 1, _) => Some(Label::GameSettings),
        (Label::Resume, -1, _) => Some(Label::Back),
        (Label::GameSettings, 1, _) => Some(Label::Back),
        (Label::GameSettings, -1, _) => Some(Label::Resume),
        (Label::PlayAgain, 1, _) => Some(Label::Back),
        (Label::PlayAgain, -1, _) => Some(Label::Back),
        (Label::Back, 1, _) => match (**screen, **game_screen) {
            (Screen::Settings, _) => Some(Label::Volume),
            (Screen::Credits, _) => Some(Label::ProgrammingLanguage),
            (Screen::Game, GameScreen::Settings) => Some(Label::Volume),
            (Screen::Game, GameScreen::Pause) => Some(Label::Resume),
            (Screen::Game, GameScreen::End) => Some(Label::PlayAgain),
            _ => None,
        },
        (Label::Back, -1, _) => match (**screen, **game_screen) {
            (Screen::Settings, _) => Some(Label::MaxDifficulty),
            (Screen::Credits, _) => Some(Label::Source),
            (Screen::Game, GameScreen::Settings) => Some(Label::Volume),
            (Screen::Game, GameScreen::Pause) => Some(Label::Resume),
            (Screen::Game, GameScreen::End) => Some(Label::PlayAgain),
            _ => None,
        },
        _ => None,
    };

    if let Some(a) = next_selection
        && let Some((next, _)) = elements
            .iter()
            .find(|(_, button_action)| **button_action == a)
    {
        if let Ok(mut ent) = commands.get_entity(selected_ent) {
            ent.remove::<Selected>();
        }
        commands.entity(next).insert(Selected);
    }
}

pub fn keypress_action(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
    active: Query<(Entity, &Label), With<Active>>,
    selected: Single<&Label, With<Selected>>,
    keyboard_options: Query<(Entity, &Label), With<KeyboardOption>>,
    mut app_exit_msg: MessageWriter<AppExit>,
    mut pause_msg: MessageWriter<PauseMsg>,
    mut max_difficulty_toggle: Query<&mut BackgroundColor, With<MaxDifToggle>>,
    mut vtime: ResMut<Time<Virtual>>,
    screen: Res<State<Screen>>,
    game_screen: Res<State<GameScreen>>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut next_game_screen: ResMut<NextState<GameScreen>>,
    mut pkv: ResMut<PkvStore>,
) {
    let selected = if keys.just_pressed(KeyCode::Escape) && **game_screen != GameScreen::Pause {
        Some(Label::Back)
    } else if keys.any_just_pressed([KeyCode::Enter, KeyCode::Space]) {
        Some(**selected)
    } else {
        None
    };
    if let Some(s) = selected {
        do_action(
            s,
            &mut commands,
            &mut app_exit_msg,
            &mut pause_msg,
            &screen,
            &game_screen,
            &mut next_screen,
            &mut next_game_screen,
            &active,
            &keyboard_options,
            &mut max_difficulty_toggle,
            &mut vtime,
            &mut pkv,
            &mut config,
        );
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
        c.displayed = c.displayed.saturating_sub(1);
        **t = c.displayed.to_string();
    }
    if c.displayed == 0 {
        next_screen.set(GameScreen::Running);
        vtime.unpause();
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

fn ingame_screen<T: Component>(c: T) -> (T, Node, BackgroundColor) {
    (
        c,
        Node {
            padding: UiRect::axes(Val::Percent(1.), Val::Percent(0.)),
            border_radius: BorderRadius::all(Val::Percent(5.)),
            ..screen_node()
        },
        BackgroundColor(colors::IN_GAME_MENU),
    )
}
