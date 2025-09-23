use crate::*;
use bevy::{prelude::*, ui::RelativeCursorPosition};
use core::f32;

const VOLUME_LABEL_PREFIX: &str = "volume:";

#[derive(Component)]
pub struct VolumeControl;

#[derive(Component)]
pub struct VolumeControlBar;

#[derive(Component)]
pub struct VolumeDisplay;

#[derive(Component)]
pub struct Selected;

#[derive(Component)]
pub struct Active;

#[derive(Component)]
pub struct KeyboardOption;

#[derive(Component, Clone, Copy, PartialEq)]
pub enum Label {
    Game,
    Settings,
    SettingsBack,
    Credits,
    CreditsBack,
    Volume,
    PhysicalKeyboardLayout,
    PhysicalQwerty,
    PhysicalDvorak,
    PhysicalColemak,
    LogicalKeyboardLayout,
    LogicalQwerty,
    LogicalDvorak,
    LogicalColemak,
    ProgrammingLanguage,
    GameEngine,
    LaunchProjectileSound,
    ExplosionSound,
    MistypeSound,
    FriendSpawnSound,
    CollectFriendSound,
    Help,
    HelpBack,
    Resume,
    PauseBack,
    PlayAgain,
    EndBack,
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

    fn to_link(self) -> Option<&'static str> {
        match self {
            Label::ProgrammingLanguage => Some(credits::PROGRAMMING_LANGUAGE_LINK),
            Label::GameEngine => Some(credits::GAME_ENGINE_LINK),
            Label::LaunchProjectileSound => Some(credits::LAUNCH_PROJECTILE_SOUND_LINK),
            Label::ExplosionSound => Some(credits::EXPLOSION_SOUND_LINK),
            Label::MistypeSound => Some(credits::MISTYPE_SOUND_LINK),
            Label::FriendSpawnSound => Some(credits::FRIEND_SPAWN_SOUND_LINK),
            Label::CollectFriendSound => Some(credits::COLLECT_FRIEND_SOUND_LINK),
            _ => None,
        }
    }
}

impl KeyboardLayouts {
    fn right(&self, logical: bool) -> Option<Label> {
        match (self, logical) {
            (KeyboardLayouts::Qwerty, true) => Some(Label::LogicalDvorak),
            (KeyboardLayouts::Qwerty, false) => Some(Label::PhysicalDvorak),
            (KeyboardLayouts::Dvorak, true) => Some(Label::LogicalColemak),
            (KeyboardLayouts::Dvorak, false) => Some(Label::PhysicalColemak),
            (KeyboardLayouts::Colemak, _) => None,
        }
    }
    fn left(&self, logical: bool) -> Option<Label> {
        match (self, logical) {
            (KeyboardLayouts::Dvorak, true) => Some(Label::LogicalQwerty),
            (KeyboardLayouts::Dvorak, false) => Some(Label::PhysicalQwerty),
            (KeyboardLayouts::Colemak, true) => Some(Label::LogicalDvorak),
            (KeyboardLayouts::Colemak, false) => Some(Label::PhysicalDvorak),
            (KeyboardLayouts::Qwerty, _) => None,
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
        ..default()
    }
}

pub fn menu_setup(mut commands: Commands, config: Res<Config>) {
    commands
        .spawn(screen_with(MenuScreen))
        .with_children(|cmd| {
            cmd.spawn(Node {
                justify_content: JustifyContent::SpaceBetween,
                align_items: AlignItems::Center,
                margin: UiRect::bottom(Val::Vh(-6.0)),
                ..default()
            })
            .with_children(|title| {
                let angle_span = PI / 2.5;
                let angle_start = -angle_span / 2.0;
                for (i, c) in env!("CARGO_PKG_NAME").chars().enumerate() {
                    let angle = angle_start + i as f32 * (angle_span / (8 - 1) as f32);
                    title.spawn((
                        Text::new(c),
                        TITLE_FONT.clone(),
                        Transform::from_rotation(Quat::from_rotation_z(angle)),
                        Node {
                            padding: UiRect::new(
                                Val::Vh(0.),
                                Val::Vw(1.),
                                Val::Vh(0.),
                                Val::Vh(10. * (PI / 7. * i as f32).sin()),
                            ),
                            ..default()
                        },
                    ));
                }
            });

            cmd.spawn((
                Text::new(format!("highscore: {}", config.high_score)),
                FONT.clone(),
                TextColor(TEXT_FUTURE_COLOR),
            ));

            for (action, label) in [
                (Label::Game, "play"),
                (Label::Help, "help"),
                (Label::Settings, "settings"),
                (Label::Credits, "credits"),
                #[cfg(not(target_arch = "wasm32"))]
                (Label::Quit, "quit"),
            ] {
                let mut entity = cmd.spawn((
                    Button,
                    Node { ..button() },
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    action,
                ));
                entity.with_children(|parent| {
                    parent.spawn((Text::new(label), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
                if let Label::Game = action {
                    entity.insert(Selected);
                }
            }
        });
}

pub fn on_selection(
    selection: Single<Entity, Added<Selected>>,
    mut removed: RemovedComponents<Selected>,
    mut borders: Query<&mut BorderColor>,
) {
    for entity in removed.read() {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor(GREY);
        }
    }
    if let Ok(mut new) = borders.get_mut(*selection) {
        new.0 = WHITE;
    }
}

pub fn on_active(
    active: Query<Entity, Added<Active>>,
    mut removed: RemovedComponents<Active>,
    mut borders: Query<&mut BorderColor>,
) {
    for entity in removed.read() {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor(GREY);
        }
    }
    for entity in active {
        if let Ok(mut border) = borders.get_mut(entity) {
            *border = BorderColor(WHITE);
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
    mut app_exit_events: EventWriter<AppExit>,
    mut pause_event: EventWriter<PauseEvent>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut vtime: ResMut<Time<Virtual>>,
    mut pkv: ResMut<PkvStore>,
    mut config: ResMut<Config>,
) {
    for (interaction, menu_button_action) in interaction_query.iter_mut() {
        if *interaction == Interaction::Pressed {
            do_action(
                *menu_button_action,
                &mut commands,
                &mut app_exit_events,
                &mut pause_event,
                &mut next_screen,
                &active,
                &keyboard_options,
                &mut vtime,
                &mut pkv,
                &mut config,
            );
        }
    }
    for (interaction, mut border_color) in hovers.iter_mut() {
        if *interaction == Interaction::Hovered {
            border_color.0 = WHITE;
        } else if *interaction == Interaction::None {
            border_color.0 = GREY;
        }
    }
}

fn do_action(
    action: Label,
    commands: &mut Commands,
    app_exit_events: &mut EventWriter<AppExit>,
    pause_event: &mut EventWriter<PauseEvent>,
    next_screen: &mut ResMut<NextState<Screen>>,
    active: &Query<(Entity, &Label), With<Active>>,
    keyboard_options: &Query<(Entity, &Label), With<KeyboardOption>>,
    vtime: &mut ResMut<Time<Virtual>>,
    pkv: &mut ResMut<PkvStore>,
    config: &mut ResMut<Config>,
) {
    match action {
        Label::Quit => {
            app_exit_events.write(AppExit::Success);
        }
        Label::Game => {
            next_screen.set(Screen::Game);
        }
        Label::Settings => next_screen.set(Screen::Settings),
        Label::Help => next_screen.set(Screen::Help),
        Label::Credits => next_screen.set(Screen::Credits),
        Label::SettingsBack => {
            next_screen.set(Screen::MainMenu);
            let _ = pkv.set(VOLUME_KEY, &config.volume);
            let _ = pkv.set(
                PHYSICAL_KEYBOARD_LAYOUT_KEY,
                &config.physical_keyboard_layout,
            );
            let _ = pkv.set(LOGICAL_KEYBOARD_LAYOUT_KEY, &config.logical_keyboard_layout);
        }
        Label::CreditsBack | Label::HelpBack => next_screen.set(Screen::MainMenu),
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
        | Label::LaunchProjectileSound
        | Label::ExplosionSound
        | Label::MistypeSound
        | Label::FriendSpawnSound
        | Label::CollectFriendSound => {
            #[cfg(not(target_arch = "wasm32"))]
            let _ = open::that(action.to_link().unwrap());

            #[cfg(target_arch = "wasm32")]
            let _ = web_sys::window()
                .unwrap()
                .open_with_url(action.to_link().unwrap());
        }
        Label::Resume => {
            pause_event.write(PauseEvent::TogglePause);
        }
        Label::PauseBack | Label::EndBack => {
            vtime.unpause();
            vtime.set_relative_speed(1.0);
            next_screen.set(Screen::MainMenu);
        }
        Label::PlayAgain => {
            vtime.unpause();
            vtime.set_relative_speed(1.0);
            next_screen.set(Screen::Game);
        }
        _ => {}
    }
}

pub fn despawn_screen<T: Component>(screen: Single<Entity, With<T>>, mut commands: Commands) {
    commands.entity(*screen).despawn();
}

pub fn pause_setup(mut commands: Commands) {
    commands
        .spawn((
            PauseScreen,
            Node {
                padding: UiRect {
                    left: Val::Percent(1.),
                    right: Val::Percent(1.),
                    top: Val::Percent(0.),
                    bottom: Val::Percent(0.),
                },
                ..screen_node()
            },
            BorderRadius::all(Val::Percent(5.)),
            BackgroundColor(IN_GAME_MENU_COLOR),
        ))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new("Game Is Paused"),
                TextLayout::new_with_justify(JustifyText::Center),
                FONT.clone(),
            ));
            for (action, label) in [(Label::Resume, "resume"), (Label::PauseBack, "exit")] {
                let mut entity = cmd.spawn((
                    Button,
                    Node { ..button() },
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    action,
                ));
                entity.with_children(|parent| {
                    parent.spawn((Text::new(label), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
                if let Label::Resume = action {
                    entity.insert(Selected);
                }
            }
        });
}

pub fn end_setup(mut commands: Commands, stats: Single<&mut Stats>) {
    commands
        .spawn((
            EndScreen,
            Node {
                padding: UiRect {
                    left: Val::Percent(1.),
                    right: Val::Percent(1.),
                    top: Val::Percent(0.),
                    bottom: Val::Percent(0.),
                },
                ..screen_node()
            },
            BorderRadius::all(Val::Percent(5.)),
            BackgroundColor(IN_GAME_MENU_COLOR),
        ))
        .with_children(|cmd| {
            cmd.spawn((
                Text::new(format!("Game Over\nScore: {}", stats.score)),
                TextLayout::new_with_justify(JustifyText::Center),
                FONT.clone(),
            ));
            for (action, label) in [(Label::PlayAgain, "play again"), (Label::EndBack, "exit")] {
                let mut entity = cmd.spawn((
                    Button,
                    Node { ..button() },
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    action,
                ));
                entity.with_children(|parent| {
                    parent.spawn((Text::new(label), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
                if let Label::PlayAgain = action {
                    entity.insert(Selected);
                }
            }
        });
}

pub fn resume_countdown_setup(mut commands: Commands) {
    commands.spawn((
        ResumeCountdownScreen,
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
            ..default()
        },
        BackgroundColor(IN_GAME_MENU_COLOR),
        BorderRadius::all(Val::Percent(100.)),
        Text::new((TIME_BEFORE_RESUME as usize).to_string()),
        FONT.clone(),
        TextLayout::new_with_justify(JustifyText::Center),
    ));
}

pub fn help_setup(mut commands: Commands, config: Res<Config>) {
    commands
        .spawn(screen_with(HelpScreen))
        .with_children(|screen| {
            screen.spawn((
                Text::new(format!(
                    "avoid the polygons while collecting circles\nuse {}{}{}{} to move\nuse the right side of the keyboard to defeat polygons",
                    config.up_char(),
                    config.left_char(),
                    config.down_char(),
                    config.right_char()
                )),
                FONT.clone(),
                TextColor(TEXT_FUTURE_COLOR),
                TextLayout {
                    justify: JustifyText::Center,
                    ..default()
                }
            ));
            screen
                .spawn((
                    Label::HelpBack,
                    Button,
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    Node { ..button() },
                    Selected,
                ))
                .with_children(|button| {
                    button.spawn((Text::new("back"), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
        });
}

pub fn settings_setup(mut commands: Commands, config: Res<Config>) {
    commands
        .spawn(screen_with(SettingsScreen))
        .with_children(|screen| {
            screen.spawn((
                Node {
                    margin: UiRect::right(Val::Percent(1.)),
                    ..default()
                },
                Text::new(format!("{} {}", VOLUME_LABEL_PREFIX, config.volume)),
                FONT.clone(),
                TextColor(TEXT_FUTURE_COLOR),
                VolumeDisplay,
            ));
            screen
                .spawn((
                    Node {
                        width: Val::Vw(40.),
                        height: Val::Vh(3.0),
                        justify_content: JustifyContent::FlexStart,
                        align_items: AlignItems::Center,
                        ..button()
                    },
                    VolumeControl,
                    BorderColor(GREY),
                    BorderRadius::MAX,
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
                        BackgroundColor(WHITE),
                        VolumeControlBar,
                    ));
                });
            screen.spawn((
                Node {
                    margin: UiRect::right(Val::Percent(1.)),
                    ..default()
                },
                Text::new("keyboard layout"),
                FONT.clone(),
                TextColor(TEXT_FUTURE_COLOR),
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
                screen
                    .spawn((
                        group_label,
                        Node {
                            flex_direction: FlexDirection::Row,
                            align_items: AlignItems::Center,
                            ..button()
                        },
                        BorderRadius::MAX,
                        BorderColor(GREY),
                    ))
                    .with_children(|row| {
                        row.spawn((
                            Text::new(group_text),
                            FONT.clone(),
                            TextColor(TEXT_FUTURE_COLOR),
                        ));

                        for (button_label, button_text, active) in buttons {
                            let mut option = row.spawn((
                                Button,
                                button_label,
                                Node { ..button() },
                                BorderColor(GREY),
                                BorderRadius::MAX,
                                KeyboardOption,
                            ));
                            if active {
                                option.insert(Active);
                            }
                            option.with_children(|b| {
                                b.spawn((
                                    Text::new(button_text),
                                    FONT.clone(),
                                    TextColor(TEXT_NEXT_COLOR),
                                ));
                            });
                        }
                    });
            }

            screen
                .spawn((
                    Label::SettingsBack,
                    Button,
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    Node { ..button() },
                ))
                .with_children(|button| {
                    button.spawn((Text::new("back"), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
        });
}

pub fn credits_setup(mut commands: Commands) {
    commands
        .spawn(screen_with(CreditsScreen))
        .with_children(|screen| {
            for (label, text) in [
                (
                    Label::ProgrammingLanguage,
                    credits::PROGRAMMING_LANGUAGE_TEXT,
                ),
                (Label::GameEngine, credits::GAME_ENGINE_TEXT),
                (
                    Label::LaunchProjectileSound,
                    credits::LAUNCH_PROJECTILE_SOUND_TEXT,
                ),
                (Label::ExplosionSound, credits::EXPLOSION_SOUND_TEXT),
                (Label::MistypeSound, credits::MISTYPE_SOUND_TEXT),
                (Label::FriendSpawnSound, credits::FRIEND_SPAWN_SOUND_TEXT),
                (
                    Label::CollectFriendSound,
                    credits::COLLECT_FRIEND_SOUND_TEXT,
                ),
            ] {
                let mut ent = screen.spawn((
                    label,
                    Button,
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    Node { ..button() },
                ));
                if let Label::ProgrammingLanguage = label {
                    ent.insert(Selected);
                }
                ent.with_children(|button| {
                    button.spawn((Text::new(text), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
            }
            screen
                .spawn((
                    Label::CreditsBack,
                    Button,
                    BorderColor(GREY),
                    BorderRadius::MAX,
                    Node { ..button() },
                ))
                .with_children(|button| {
                    button.spawn((Text::new("back"), FONT.clone(), TextColor(TEXT_NEXT_COLOR)));
                });
        });
}

impl Config {
    fn set_vol(&mut self, val: u8) {
        self.volume = val;
    }
    fn inc_vol(&mut self, val: u8, global_volume: &mut GlobalVolume) {
        self.volume = (self.volume + val).min(100);
        global_volume.volume = bevy::audio::Volume::Linear(self.volume as f32 / 100.);
    }
    fn dec_vol(&mut self, val: u8, global_volume: &mut GlobalVolume) {
        self.volume = self.volume.saturating_sub(val);
        global_volume.volume = bevy::audio::Volume::Linear(self.volume as f32 / 100.);
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

pub fn keypress(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    mut config: ResMut<Config>,
    active: Query<(Entity, &Label), With<Active>>,
    mut global_volume: ResMut<GlobalVolume>,
    selected: Single<(Entity, &Label), With<Selected>>,
    elements: Query<(Entity, &Label)>,
    keyboard_options: Query<(Entity, &Label), With<KeyboardOption>>,
    mut app_exit_events: EventWriter<AppExit>,
    mut pause_event: EventWriter<PauseEvent>,
    screen: Res<State<Screen>>,
    mut vtime: ResMut<Time<Virtual>>,
    mut next_screen: ResMut<NextState<Screen>>,
    mut pkv: ResMut<PkvStore>,
) {
    let (selected_ent, selected_label) = *selected;
    if keys.any_just_pressed([KeyCode::ArrowRight, config.right()]) {
        match selected_label {
            Label::Volume => config.inc_vol(5, &mut global_volume),
            Label::PhysicalKeyboardLayout => {
                if let Some(next) = config.physical_keyboard_layout.right(false) {
                    switch_keyboard_layout(
                        &mut commands,
                        &active,
                        &keyboard_options,
                        next,
                        *selected_label,
                        &mut config,
                    );
                }
            }
            Label::LogicalKeyboardLayout => {
                if let Some(next) = config.logical_keyboard_layout.right(true) {
                    switch_keyboard_layout(
                        &mut commands,
                        &active,
                        &keyboard_options,
                        next,
                        *selected_label,
                        &mut config,
                    );
                }
            }
            _ => {}
        }
    } else if keys.any_just_pressed([KeyCode::ArrowLeft, config.left()]) {
        match selected_label {
            Label::Volume => config.dec_vol(5, &mut global_volume),
            Label::PhysicalKeyboardLayout => {
                if let Some(next) = config.physical_keyboard_layout.left(false) {
                    switch_keyboard_layout(
                        &mut commands,
                        &active,
                        &keyboard_options,
                        next,
                        *selected_label,
                        &mut config,
                    );
                }
            }
            Label::LogicalKeyboardLayout => {
                if let Some(next) = config.logical_keyboard_layout.left(true) {
                    switch_keyboard_layout(
                        &mut commands,
                        &active,
                        &keyboard_options,
                        next,
                        *selected_label,
                        &mut config,
                    );
                }
            }
            _ => {}
        }
    }
    if keys.just_pressed(KeyCode::Escape) {
        match **screen {
            Screen::Settings | Screen::Credits | Screen::Help => next_screen.set(Screen::MainMenu),
            _ => {}
        }
    }
    let direction = if keys.any_just_pressed([KeyCode::ArrowDown, config.down()]) {
        1
    } else if keys.any_just_pressed([KeyCode::ArrowUp, config.up()]) {
        -1
    } else {
        0
    };

    let next_selection = match (selected_label, direction) {
        (Label::Game, 1) => Some(Label::Help),
        (Label::Help, 1) => Some(Label::Settings),
        (Label::Help, -1) => Some(Label::Game),
        (Label::Settings, 1) => Some(Label::Credits),
        (Label::Settings, -1) => Some(Label::Help),
        #[cfg(not(target_arch = "wasm32"))]
        (Label::Credits, 1) => Some(Label::Quit),
        (Label::Credits, -1) => Some(Label::Settings),
        #[cfg(not(target_arch = "wasm32"))]
        (Label::Quit, -1) => Some(Label::Credits),
        (Label::Volume, 1) => Some(Label::LogicalKeyboardLayout),
        (Label::LogicalKeyboardLayout, 1) => Some(Label::PhysicalKeyboardLayout),
        (Label::PhysicalKeyboardLayout, 1) => Some(Label::SettingsBack),
        (Label::SettingsBack, -1) => Some(Label::PhysicalKeyboardLayout),
        (Label::PhysicalKeyboardLayout, -1) => Some(Label::LogicalKeyboardLayout),
        (Label::LogicalKeyboardLayout, -1) => Some(Label::Volume),
        (Label::ProgrammingLanguage, 1) => Some(Label::GameEngine),
        (Label::GameEngine, 1) => Some(Label::LaunchProjectileSound),
        (Label::GameEngine, -1) => Some(Label::ProgrammingLanguage),
        (Label::LaunchProjectileSound, 1) => Some(Label::ExplosionSound),
        (Label::LaunchProjectileSound, -1) => Some(Label::GameEngine),
        (Label::ExplosionSound, 1) => Some(Label::MistypeSound),
        (Label::ExplosionSound, -1) => Some(Label::LaunchProjectileSound),
        (Label::MistypeSound, 1) => Some(Label::FriendSpawnSound),
        (Label::MistypeSound, -1) => Some(Label::ExplosionSound),
        (Label::FriendSpawnSound, 1) => Some(Label::CollectFriendSound),
        (Label::FriendSpawnSound, -1) => Some(Label::MistypeSound),
        (Label::CollectFriendSound, -1) => Some(Label::FriendSpawnSound),
        (Label::CollectFriendSound, 1) => Some(Label::CreditsBack),
        (Label::CreditsBack, -1) => Some(Label::CollectFriendSound),
        (Label::Resume, 1) => Some(Label::PauseBack),
        (Label::PauseBack, -1) => Some(Label::Resume),
        (Label::PlayAgain, 1) => Some(Label::EndBack),
        (Label::EndBack, -1) => Some(Label::PlayAgain),
        _ => None,
    };
    if let Some(a) = next_selection {
        if let Some((next, _)) = elements
            .iter()
            .find(|(_, button_action)| **button_action == a)
        {
            if let Ok(mut ent) = commands.get_entity(selected_ent) {
                ent.remove::<Selected>();
            }
            commands.entity(next).insert(Selected);
        }
    }
    if keys.just_pressed(KeyCode::Enter) {
        do_action(
            *selected_label,
            &mut commands,
            &mut app_exit_events,
            &mut pause_event,
            &mut next_screen,
            &active,
            &keyboard_options,
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
) {
    if !drag.0 {
        return;
    }

    if let Some(p) = bar_rcp.normalized {
        let percent = (p.x * 100.0).clamp(0.0, 100.0);
        config.set_vol(percent as u8);
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

fn screen_with<T: Component>(component: T) -> (Node, T) {
    (screen_node(), component)
}
