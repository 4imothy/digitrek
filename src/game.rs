// SPDX-License-Identifier: MIT

use crate::*;
use bevy::{
    input::{
        ButtonState,
        keyboard::{Key, KeyboardInput},
    },
    prelude::*,
    render::render_resource::PrimitiveTopology,
};
use rand::{self, distr::Distribution};
use std::{
    f32::consts::{FRAC_PI_2, TAU},
    sync::Arc,
};

pub fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut vtime: ResMut<Time<Virtual>>,
    mut audio: ResMut<Assets<AudioSource>>,
    mut mode: ResMut<Mode>,
    mut keyboard_input: ResMut<Messages<KeyboardInput>>,
) {
    *mode = Mode::default();
    keyboard_input.clear();
    vtime.set_relative_speed(TIME_MULTIPLIER);
    commands
        .spawn((
            Mesh2d(meshes.add(CircularSector::new(PLAYER_RADIUS, PLAYER_HALF_ANGLE))),
            MeshMaterial2d(materials.add(colors::PLAYER)),
            Transform {
                translation: Vec3::new(0., PLAYER_RADIUS, PLAYER_Z_INDEX),
                rotation: Quat::from_rotation_z(PI),
                ..default()
            },
            Player { selected: None },
        ))
        .with_children(|cmds| {
            cmds.spawn((
                Transform::from_xyz(0., PLAYER_RADIUS / 2., 0.),
                Visibility::Inherited,
                PlayerLauncher,
            ))
            .with_children(|c| {
                c.spawn((
                    Mesh2d(meshes.add(Circle::new(PLAYER_NOTCH_RADIUS))),
                    MeshMaterial2d(materials.add(colors::LAUNCHER)),
                    Transform::from_xyz(0., PLAYER_RING_RADIUS, 1.),
                    LauncherNotch,
                ));
            });
            cmds.spawn((
                Mesh2d(meshes.add(Annulus::new(
                    PLAYER_RING_RADIUS - PLAYER_RING_THICKNESS / 2.,
                    PLAYER_RING_RADIUS + PLAYER_RING_THICKNESS / 2.,
                ))),
                MeshMaterial2d(materials.add(colors::LAUNCHER)),
                Transform::from_xyz(0., PLAYER_RADIUS / 2., 0.5),
            ));
            cmds.spawn((
                LauncherRing,
                Mesh2d(meshes.add(launcher_ring_mesh(
                    PLAYER_RING_RADIUS - PLAYER_RING_THICKNESS / 2.,
                    PLAYER_RING_RADIUS + PLAYER_RING_THICKNESS / 2.,
                    0.,
                ))),
                MeshMaterial2d(materials.add(colors::GLOW)),
                Transform::from_xyz(0., PLAYER_RADIUS / 2., 0.6),
                Visibility::Hidden,
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    PLAYER_ENGINE_WIDTH,
                    PLAYER_ENGINE_WIDTH / 2.,
                ))),
                MeshMaterial2d(materials.add(colors::LAUNCHER)),
                Transform::from_xyz(0., PLAYER_RADIUS, -1.),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(CircularSector::new(
                    0.8 * PLAYER_ENGINE_WIDTH / 2.,
                    FRAC_PI_2,
                ))),
                MeshMaterial2d(materials.add(colors::GLOW)),
                Transform::from_xyz(0., PLAYER_RADIUS + PLAYER_ENGINE_WIDTH / 4., 2.),
                Visibility::Hidden,
                EngineGlow,
            ));
            if SHOW_LOCAL_POINTS {
                let circle = Circle::new(10.);
                let circle_mesh = meshes.add(Mesh::from(circle));
                let circle_material = materials.add(Color::WHITE);
                for p in PLAYER_LOCAL_TRIANGLE {
                    cmds.spawn((
                        Mesh2d(circle_mesh.clone()),
                        MeshMaterial2d(circle_material.clone()),
                        Transform::from_xyz(p.x, p.y, PLAYER_Z_INDEX),
                    ));
                }
            }
        });

    commands
        .spawn((
            Indicator { active: false },
            Mesh2d(meshes.add(Annulus::new(
                INDICATOR_RADIUS - INDICATOR_THICKNESS / 2.,
                INDICATOR_RADIUS + INDICATOR_THICKNESS / 2.,
            ))),
            MeshMaterial2d(materials.add(colors::INDICATOR)),
            Transform::from_xyz(0., 0., INDICATOR_Z_INDEX),
            Visibility::Hidden,
        ))
        .with_children(|c| {
            let h = meshes.add(Rectangle::new(
                INDICATOR_LONG_RECTANGLE_LENGTH,
                INDICATOR_THICKNESS,
            ));
            let v = meshes.add(Rectangle::new(
                INDICATOR_THICKNESS,
                INDICATOR_LONG_RECTANGLE_LENGTH,
            ));
            let mat = materials.add(colors::INDICATOR);
            for (m, x, y) in [
                (h.clone(), -INDICATOR_RADIUS, 0.),
                (h, INDICATOR_RADIUS, 0.),
                (v.clone(), 0., INDICATOR_RADIUS),
                (v, 0., -INDICATOR_RADIUS),
            ] {
                c.spawn((
                    Mesh2d(m),
                    MeshMaterial2d(mat.clone()),
                    Transform::from_xyz(x, y, PLAYER_Z_INDEX),
                ));
            }
        });

    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(SCORE_TEXT_PADDING),
            left: Val::Px(SCORE_TEXT_PADDING),
            ..default()
        },
        Text::new("0"),
        text_font(),
        Stats {
            score: 0,
            running: true,
        },
    ));
    commands.insert_resource(Fuel(0.));
    commands.insert_resource(Spawner {
        foe_dist: WeightedIndex::new(PHASE_WEIGHTS[0]).unwrap(),
        foe_delay: FIRST_FOE_SPAWN_DELAY,
        foe_delay_mu: 0.,
        last_side: 0,
    });
    commands.insert_resource(AudioAssets {
        unmatched_keypress: audio.add(AudioSource {
            bytes: Arc::from(*include_bytes!("../assets/miss.ogg")),
        }),
        explosion: audio.add(AudioSource {
            bytes: Arc::from(*include_bytes!("../assets/boom.ogg")),
        }),
        foe_launch: audio.add(AudioSource {
            bytes: Arc::from(*include_bytes!("../assets/launch.ogg")),
        }),
    });
    commands.insert_resource(ShapeAssets {
        meshes: [
            meshes.add(TRIANGLE),
            meshes.add(RHOMBUS),
            meshes.add(PENTAGON),
            meshes.add(HEXAGON),
        ],
        materials: colors::SHAPE_COLORS.map(|c| materials.add(c)),
    });
    commands.insert_resource(Clock(0.));
}

pub fn default_state(
    mut next_screen: ResMut<NextState<GameScreen>>,
    mut camera: Single<&mut Transform, With<Camera2d>>,
) {
    next_screen.set(GameScreen::default());
    camera.translation.x = 0.;
    camera.translation.y = PLAYER_RADIUS;
}

impl Shape {
    pub const fn id(&self) -> usize {
        match self {
            Shape::Triangle => 0,
            Shape::Rhombus => 1,
            Shape::Pentagon => 2,
            Shape::Hexagon => 3,
        }
    }

    pub fn downgraded_shape(&self) -> Option<Self> {
        match self {
            Shape::Triangle => None,
            Shape::Rhombus => Some(Shape::Triangle),
            Shape::Pentagon => Some(Shape::Rhombus),
            Shape::Hexagon => Some(Shape::Pentagon),
        }
    }

    pub fn local_points(&self) -> &[Vec3] {
        match self {
            Shape::Triangle => &TRIANGLE_LOCAL_POINTS,
            Shape::Rhombus => &RHOMBUS_LOCAL_POINTS,
            Shape::Pentagon => &*PENTAGON_LOCAL_POINTS,
            Shape::Hexagon => &*HEXAGON_LOCAL_POINTS,
        }
    }
}

pub fn update_clock(mut clock: ResMut<Clock>, time: Res<Time<Virtual>>) {
    clock.0 += time.delta_secs();
}

pub fn foe_points(query: Query<(Entity, &Foe, &Transform)>, mut cache: ResMut<FoePointsCache>) {
    cache.0.clear();
    for (entity, foe, transform) in &query {
        cache
            .0
            .insert(entity, FoePoints::compute(foe.shape, transform));
    }
}

fn explosion_in_viewport(
    pos: Vec3,
    viewport_width: f32,
    center: Vec2,
    msg: &mut MessageWriter<GameMsg>,
    audio: &mut MessageWriter<AudioMsg>,
) {
    if in_viewport(pos, viewport_width, Vec2::ZERO, center) {
        msg.write(GameMsg::Explosion(pos.xy()));
        audio.write(AudioMsg::Explosion);
    }
}

impl Foe {
    pub fn add_collision(&mut self, collision_normal: Vec2) {
        self.knockback = Some(collision_normal * self.mov_speed() * KNOCKBACK_MULTIPLIER);
    }

    pub fn collision(
        &mut self,
        msg: &mut MessageWriter<GameMsg>,
        audio: &mut MessageWriter<AudioMsg>,
        entity: Entity,
        pos: Vec3,
        normal: Vec2,
        viewport_width: f32,
        center: Vec2,
    ) {
        self.colliding = true;
        if let Some(shape) = self.shape.downgraded_shape() {
            self.shape = shape;
            self.skipped += 1;
            if self.cleared + self.skipped >= self.orig_len {
                explosion_in_viewport(pos, viewport_width, center, msg, audio);
                msg.write(GameMsg::Despawn(entity));
            } else {
                self.add_collision(normal);
                match shape {
                    Shape::Pentagon | Shape::Rhombus => {
                        msg.write(GameMsg::DespawnChildren(entity));
                    }
                    _ => {
                        msg.write(GameMsg::DespawnText(entity));
                    }
                }
                msg.write(GameMsg::ReplaceShape(entity, shape));
                msg.write(GameMsg::AddText(entity));
            }
        } else {
            explosion_in_viewport(pos, viewport_width, center, msg, audio);
            msg.write(GameMsg::Despawn(entity));
        }
    }

    pub fn mov_speed(&self) -> f32 {
        SHAPE_MOV_SPEEDS[self.shape.id()]
    }

    pub fn rot_speed(&self) -> f32 {
        SHAPE_ROT_SPEEDS[self.shape.id()]
    }

    pub fn num_points(&self) -> usize {
        self.shape.id() + 3
    }

    pub fn keys_to_type(&self) -> usize {
        self.orig_len.saturating_sub(self.next_index + self.skipped)
    }

    pub fn keys_not_hit(&self) -> usize {
        self.orig_len.saturating_sub(self.cleared + self.skipped)
    }
}

impl Stats {
    pub fn show(&self, text: &mut Text) {
        **text = format!("{}", self.score);
    }
    pub fn inc_score(&mut self, dif: usize) {
        if self.running {
            self.score += dif;
        }
    }
    pub fn save_high_score(&self, config: &mut ResMut<Config>, pkv: &mut ResMut<PkvStore>) {
        if self.score > config.high_score {
            config.high_score = self.score;
            let _ = pkv.set(HIGH_SCORE_KEY, &config.high_score);
        }
    }
}

impl FoePoints {
    fn as_slice(&self) -> &[Vec3] {
        &self.data[..self.len]
    }

    fn compute(shape: Shape, transform: &Transform) -> Self {
        let local_points = shape.local_points();
        let mut data = [Vec3::ZERO; FOE_MAX_SIDES];
        for (i, p) in local_points.iter().enumerate() {
            data[i] = transform.transform_point(*p);
        }
        Self {
            data,
            len: local_points.len(),
        }
    }
}

pub fn keypress(
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    mut keyboard_input: MessageReader<KeyboardInput>,
    player: Single<(&mut Player, &Transform), Without<PlayerLauncher>>,
    launcher: Single<&mut Transform, (With<PlayerLauncher>, Without<Player>)>,
    notch_material: Single<&MeshMaterial2d<ColorMaterial>, With<LauncherNotch>>,
    mut color_materials: ResMut<Assets<ColorMaterial>>,
    indicator: Single<Entity, (With<Indicator>, Without<Player>)>,
    mut enemy_query: Query<
        (Entity, &mut Foe, &mut Transform),
        (Without<Player>, Without<Indicator>, Without<PlayerLauncher>),
    >,
    time: ResMut<Time<Virtual>>,
    stats: Single<&mut Stats>,
    mut mode: ResMut<Mode>,
    mut fuel: ResMut<Fuel>,
    config: Res<Config>,
) {
    let (mut player, player_transform) = player.into_inner();
    if !stats.running {
        keyboard_input.clear();
        return;
    }
    let mut found_selected = false;
    let mut launcher_transform = launcher.into_inner();

    for key in keyboard_input.read() {
        if key.state != ButtonState::Pressed {
            continue;
        }
        if time.is_paused() {
            continue;
        }
        if key.key_code == config.deselect() && *mode == Mode::Movement {
            msg.write(GameMsg::Invisible(*indicator));
            if let Some(selected) = player.selected {
                msg.write(GameMsg::DeSelect(selected));
            }
            player.selected = None;
            continue;
        }
        match &key.logical_key {
            Key::Space if !key.repeat => {
                *mode = match *mode {
                    Mode::Movement => Mode::Typing,
                    Mode::Typing => Mode::Movement,
                };
                if let Some(mat) = color_materials.get_mut(notch_material.id()) {
                    mat.color = if *mode == Mode::Typing {
                        colors::ACTIVE_NOTCH
                    } else {
                        colors::LAUNCHER
                    };
                }
            }
            Key::Backspace => {
                msg.write(GameMsg::Invisible(*indicator));
                if let Some(selected) = player.selected {
                    msg.write(GameMsg::DeSelect(selected));
                }
                player.selected = None;
            }
            Key::Character(str) if *mode == Mode::Typing => {
                if let Some(key) = str.chars().next().map(|c| c.to_ascii_lowercase()) {
                    if let Some(selected) = player.selected
                        && let Ok((selected_entity, mut selected_enemy, _)) =
                            enemy_query.get_mut(selected)
                    {
                        found_selected = true;
                        if key == selected_enemy.keys[selected_enemy.next_index] {
                            fuel.0 = (fuel.0 + FUEL_PER_KEY).min(1.);
                            selected_enemy.next_index += 1;
                            msg.write(GameMsg::Projectile(
                                selected_entity,
                                projectile_spawn_location(player_transform, &launcher_transform),
                            ));
                            if selected_enemy.keys_to_type() == 0 {
                                msg.write(GameMsg::Invisible(*indicator));
                                msg.write(GameMsg::DeSelect(selected));
                                player.selected = None;
                            }
                            msg.write(GameMsg::DespawnText(selected_entity));
                            msg.write(GameMsg::AddText(selected_entity));
                        } else {
                            audio.write(AudioMsg::UnmatchedKeypress);
                        }
                    }
                    if !found_selected {
                        player.selected = None;
                        let mut closest_distance = f32::MAX;
                        let mut closest_entity = None;
                        let mut closest_enemy = None;
                        let mut closest_enemy_transform = None;

                        for (entity, enemy, enemy_transform) in enemy_query.iter_mut() {
                            if enemy.next_index < enemy.orig_len
                                && enemy.entered_viewport
                                && key == enemy.keys[enemy.next_index]
                            {
                                let distance = player_transform
                                    .translation
                                    .distance(enemy_transform.translation);
                                if distance < closest_distance {
                                    closest_distance = distance;
                                    closest_entity = Some(entity);
                                    closest_enemy = Some(enemy);
                                    closest_enemy_transform = Some(enemy_transform);
                                }
                            }
                        }

                        if let Some(e) = closest_entity {
                            let mut ce = closest_enemy.unwrap();
                            if ce.keys_to_type() > 1 {
                                player.selected = closest_entity;
                                msg.write(GameMsg::Select(e));
                            } else {
                                point_launcher(
                                    player_transform,
                                    &mut launcher_transform,
                                    &closest_enemy_transform.unwrap(),
                                );
                            }

                            fuel.0 = (fuel.0 + FUEL_PER_KEY).min(1.);
                            ce.next_index += 1;
                            msg.write(GameMsg::DespawnText(e));
                            msg.write(GameMsg::AddText(e));
                            msg.write(GameMsg::Projectile(
                                e,
                                projectile_spawn_location(player_transform, &launcher_transform),
                            ));
                        } else {
                            audio.write(AudioMsg::UnmatchedKeypress);
                        }
                        if player.selected.is_none() {
                            msg.write(GameMsg::Invisible(*indicator));
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn projectile_spawn_location(player_trans: &Transform, launcher_trans: &Transform) -> Vec3 {
    player_trans
        .transform_point(launcher_trans.transform_point(Vec3::new(0., PLAYER_RING_RADIUS, 0.)))
        .with_z(EXPLOSION_Z_INDEX)
}

fn viewport_width(win: &Window) -> f32 {
    VIEWPORT_HEIGHT * (win.width() / win.height())
}

pub fn player_movement(
    time: Res<Time<Virtual>>,
    key: Res<KeyState>,
    mut fuel: ResMut<Fuel>,
    mut transform: Single<&mut Transform, With<Player>>,
    stats: Single<&Stats>,
    mode: Res<Mode>,
    mut glow: Single<&mut Visibility, With<EngineGlow>>,
) {
    let has_fuel = fuel.0 > 0.;
    let moving = stats.running
        && !time.is_paused()
        && *mode == Mode::Movement
        && has_fuel
        && (key.v.is_some() || key.h.is_some());
    let want = if moving {
        Visibility::Visible
    } else {
        Visibility::Hidden
    };
    if **glow != want {
        **glow = want;
    }
    if stats.running && *mode == Mode::Movement {
        let rotation_factor = key.h.map(|v| if v { -1. } else { 1. }).unwrap_or(0.);
        let movement_factor = key.v.map(|v| if v { 1. } else { -1. }).unwrap_or(0.);

        let dt = time.delta_secs();
        transform.rotation *= Quat::from_rotation_z(rotation_factor * PLAYER_ROTATION_SPEED * dt);

        if has_fuel && movement_factor != 0. {
            let movement =
                movement_factor * (transform.rotation * Vec3::Y) * PLAYER_MOVEMENT_SPEED * dt;
            transform.translation += movement;
            if !INVINCIBLE {
                fuel.0 = (fuel.0 - FUEL_DRAIN_RATE * dt).max(0.);
            }
        }
    }
}

pub fn summoner(
    time: Res<Time<Virtual>>,
    mut query: Query<(
        Entity,
        &mut Foe,
        &mut Summoner,
        &Transform,
        Option<&Children>,
    )>,
    mut launcher_query: Query<(&Transform, &mut FoeLauncher)>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    player: Single<&Transform, (With<Player>, Without<Foe>)>,
) {
    let dt = time.delta_secs();
    let mut rng = rand::rng();

    for (ent, mut foe, mut summoner, transform, children) in &mut query {
        summoner.since += dt;
        if summoner.since >= summoner.delay {
            let direction = player.translation - transform.translation;
            let angle = direction.y.atan2(direction.x) - FRAC_PI_2;

            let spawn_pos = children
                .and_then(|ch| ch.iter().find_map(|c| launcher_query.get(c).ok()))
                .map(|(launcher_t, _)| {
                    let tip_local = Vec3::new(PENTAGON_LAUNCHER_LENGTH, 0., 0.);
                    transform
                        .transform_point(launcher_t.transform_point(tip_local))
                        .xy()
                })
                .unwrap_or_else(|| transform.translation.xy());

            let shape = SHAPES[summoner.foe_dist.sample(&mut rng)];
            msg.write(GameMsg::SpawnFoe(shape, spawn_pos, Some(ent), Some(angle)));
            audio.write(AudioMsg::FoeLaunch);
            summoner.since = 0.;

            let recoil_dir = -direction.xy().normalize_or_zero();
            foe.knockback = Some(recoil_dir * LAUNCH_RECOIL_SPEED);

            if let Some(ch) = children {
                for c in ch.iter() {
                    if let Ok((_, mut foe_launcher)) = launcher_query.get_mut(c) {
                        foe_launcher.recoil = 1.0;
                        break;
                    }
                }
            }
        }
    }
}

pub fn summoner_foe(
    time: Res<Time<Virtual>>,
    mut query: Query<(&mut Foe, &mut Summoner, &mut Transform)>,
    player: Single<&Transform, (With<Player>, Without<Foe>)>,
    window: Single<&Window>,
) {
    let dt = time.delta_secs();
    let viewport_width = viewport_width(&window);
    let player_pos = player.translation.xy();

    for (mut foe, mut sum, mut transform) in &mut query {
        if !foe.entered_viewport
            && in_viewport(
                transform.translation,
                viewport_width,
                Vec2::splat(-FOE_SIZE),
                player_pos,
            )
        {
            foe.entered_viewport = true;
        }
        if foe.knockback.is_some() {
            knockback(&mut foe, &mut transform, dt);
        } else {
            let pos = transform.translation.xy();

            let rel = pos - player_pos;
            let dist = rel.length();
            let radial = (-rel).normalize_or_zero();
            let tangential = Vec2::new(-rel.y, rel.x).normalize_or_zero();

            let radial_weight =
                ((dist - SUMMONER_ORBIT_RADIUS) / SUMMONER_ORBIT_RADIUS).clamp(-1.0, 1.0);

            let dir = (radial * radial_weight + tangential).normalize_or_zero();
            move_nearest_vertex_towards(
                &mut transform,
                dir,
                &mut sum.leading_vertex,
                foe.rot_speed(),
                foe.mov_speed(),
                dt,
            );
        }
    }
}

pub fn launcher_foe(
    time: Res<Time<Virtual>>,
    mut query: Query<(&mut Foe, &mut Launcher, &mut Transform)>,
    window: Single<&Window>,
    player: Single<&Transform, (With<Player>, Without<Launcher>)>,
) {
    let dt = time.delta_secs();
    let viewport_width = viewport_width(&window);
    let player_pos = player.translation.xy();

    for (mut foe, mut launcher, mut transform) in &mut query {
        if !foe.entered_viewport
            && in_viewport(
                transform.translation,
                viewport_width,
                Vec2::splat(-FOE_SIZE),
                player_pos,
            )
        {
            foe.entered_viewport = true;
        }
        if foe.knockback.is_some() {
            knockback(&mut foe, &mut transform, dt);
            continue;
        }

        if launcher.stopped {
            continue;
        }

        let pos = transform.translation.xy();
        let to_target = launcher.target_pos - pos;
        let dist = to_target.length();

        if dist < foe.mov_speed() * dt {
            transform.translation.x = launcher.target_pos.x;
            transform.translation.y = launcher.target_pos.y;
            launcher.stopped = true;
        } else {
            let dir = to_target.normalize();
            transform.translation += (dir * foe.mov_speed() * dt).extend(0.);
            transform.rotate_z(foe.rot_speed() * dt);
        }
    }
}

pub fn foe_launcher(
    time: Res<Time<Virtual>>,
    mut query: Query<(&mut Foe, &mut Launcher, &Transform, &Children)>,
    mut launcher_query: Query<(&Transform, &mut FoeLauncher)>,
    player: Single<&Transform, With<Player>>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
) {
    let dt = time.delta_secs();

    for (mut foe, mut launcher, hex_transform, children) in &mut query {
        launcher.since += dt;

        if launcher.since >= launcher.delay {
            launcher.since = 0.;

            for child in children.iter() {
                if let Ok((launcher_transform, mut foe_launcher)) = launcher_query.get_mut(child) {
                    let tip_local = Vec3::new(HEXAGON_LAUNCHER_LENGTH, 0., 0.);
                    let tip_world = hex_transform
                        .transform_point(launcher_transform.transform_point(tip_local));
                    let direction = (player.translation.xy() - hex_transform.translation.xy())
                        .normalize_or_zero();
                    msg.write(GameMsg::SpawnObstacle(tip_world.xy(), direction));
                    audio.write(AudioMsg::FoeLaunch);
                    foe.knockback = Some(-direction * LAUNCH_RECOIL_SPEED);
                    launcher.stopped = false;
                    foe_launcher.recoil = 1.0;
                    break;
                }
            }
        }
    }
}

pub fn track_player_for_foe_launcher(
    mut query: Query<(&Transform, &Children), Or<(With<Launcher>, With<Summoner>)>>,
    mut launcher_query: Query<
        (&mut Transform, &mut FoeLauncher),
        (Without<Launcher>, Without<Summoner>),
    >,
    player: Single<
        &Transform,
        (
            With<Player>,
            Without<Launcher>,
            Without<Summoner>,
            Without<FoeLauncher>,
        ),
    >,
    time: Res<Time<Virtual>>,
) {
    let dt = time.delta_secs();
    for (hex_transform, children) in &mut query {
        for child in children.iter() {
            if let Ok((mut launcher_transform, mut foe_launcher)) = launcher_query.get_mut(child) {
                let player_world = player.translation.xy();
                let hex_pos = hex_transform.translation.xy();
                let to_player = player_world - hex_pos;
                let target_angle = to_player.to_angle();

                let (_, _, hex_rot_z) = hex_transform.rotation.to_euler(EulerRot::XYZ);
                let local_angle = target_angle - hex_rot_z;

                launcher_transform.rotation = Quat::from_rotation_z(local_angle);

                if foe_launcher.recoil > 0.0 {
                    foe_launcher.recoil =
                        (foe_launcher.recoil - dt / LAUNCHER_ARM_RECOIL_DURATION).max(0.0);
                }
                launcher_transform.scale.x =
                    1.0 - foe_launcher.recoil * (1.0 - LAUNCHER_ARM_RECOIL_MIN_SCALE);
            }
        }
    }
}

pub fn camera_follow(
    player: Single<&Transform, With<Player>>,
    mut camera: Single<&mut Transform, (With<Camera2d>, Without<Player>)>,
) {
    camera.translation.x = player.translation.x;
    camera.translation.y = player.translation.y;
}

fn in_viewport(pos: Vec3, viewport_width: f32, padding: Vec2, center: Vec2) -> bool {
    pos.x > center.x - viewport_width / 2. + padding.x
        && pos.x < center.x + viewport_width / 2. - padding.x
        && pos.y > center.y - VIEWPORT_HEIGHT / 2. + padding.y
        && pos.y < center.y + VIEWPORT_HEIGHT / 2. - padding.y
}

pub fn tracking_foe(
    time: Res<Time<Virtual>>,
    mut query: Query<(&mut Foe, &mut Transform), (With<Tracking>, Without<Player>)>,
    player_transform: Single<&Transform, With<Player>>,
    window: Single<&Window>,
) {
    let player_translation = player_transform.translation.xy();
    let viewport_width = viewport_width(&window);

    for (mut foe, mut transform) in &mut query {
        if !foe.entered_viewport
            && in_viewport(
                transform.translation,
                viewport_width,
                Vec2::splat(-FOE_SIZE),
                player_translation,
            )
        {
            foe.entered_viewport = true;
        }
        let dt = time.delta_secs();
        if foe.knockback.is_some() {
            knockback(&mut foe, &mut transform, dt);
        } else {
            move_and_rotate_towards(
                &mut transform,
                player_translation,
                foe.rot_speed(),
                foe.mov_speed(),
                dt,
            );
        }
    }
}

fn knockback(foe: &mut Foe, transform: &mut Transform, dt: f32) {
    if let Some(vel) = &mut foe.knockback {
        transform.translation += Vec3::new(vel.x, vel.y, 0.) * dt;
        *vel *= (1. - KNOCKBACK_DECAY * dt).clamp(0., 1.);
        if vel.length_squared() < KNOCKBACK_STOP_SPEED * KNOCKBACK_STOP_SPEED {
            foe.knockback = None;
        }
    }
}

fn move_and_rotate_towards(
    transform: &mut Transform,
    target: Vec2,
    rot_speed: f32,
    mov_speed: f32,
    dt: f32,
) {
    let current_pos = transform.translation.xy();
    let to_target = (target - current_pos).normalize_or_zero();

    let angle_to_target = (transform.rotation * Vec3::Y)
        .xy()
        .normalize_or_zero()
        .angle_to(to_target);

    let max_delta = rot_speed * dt;
    let clamped_angle = angle_to_target.clamp(-max_delta, max_delta);

    transform.rotate_z(clamped_angle);

    let forward_direction = transform.rotation * Vec3::Y;
    transform.translation += forward_direction * mov_speed * dt;
}

fn move_nearest_vertex_towards(
    transform: &mut Transform,
    target_dir: Vec2,
    leading_vertex: &mut usize,
    rot_speed: f32,
    mov_speed: f32,
    dt: f32,
) {
    if target_dir == Vec2::ZERO {
        return;
    }

    let dirs = &*PENTAGON_VERTEX_DIRS;
    let target_local = (transform.rotation.inverse() * target_dir.extend(0.)).xy();
    if dirs[*leading_vertex].dot(target_local) < COS_MIN_LEADING_VERTEX_ALIGNMENT {
        *leading_vertex = dirs
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.dot(target_local).total_cmp(&b.dot(target_local)))
            .unwrap()
            .0;
    }

    let v = dirs[*leading_vertex];
    let snap = Quat::from_rotation_z(target_dir.to_angle() - v.to_angle());
    transform.rotation = transform.rotation.rotate_towards(snap, rot_speed * dt);

    let move_dir = (transform.rotation * v.extend(0.)).xy();
    transform.translation += (move_dir * mov_speed * dt).extend(0.);
}

pub fn projectile(
    mut projectiles_query: Query<(Entity, &mut Projectile, &mut Transform), With<Projectile>>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    mut enemies_query: Query<
        (
            Entity,
            &mut Foe,
            &Transform,
            Option<&mut Summoner>,
            Option<&mut Launcher>,
        ),
        (Without<Projectile>,),
    >,
    stats: Single<(&mut Stats, &mut Text)>,
    time: Res<Time<Virtual>>,
    cache: Res<FoePointsCache>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    for (projectile_entity, projectile, mut projectile_transform) in &mut projectiles_query {
        if let Ok((target_entity, mut targeted_enemy, target_transform, summoner, launcher_foe)) =
            enemies_query.get_mut(projectile.target)
        {
            let Some(e_points) = cache.0.get(&target_entity) else {
                continue;
            };
            let e_points = e_points.as_slice();
            let direction = (target_transform.translation - projectile_transform.translation)
                .xy()
                .normalize_or_zero();
            projectile_transform.translation +=
                direction.extend(0.) * PROJECTILE_MOVEMENT_SPEED * time.delta_secs();

            if circle_polygon_collide(
                projectile_transform.translation.xy(),
                PROJECTILE_RADIUS,
                e_points,
            )
            .is_some()
            {
                if let Some(mut s) = summoner {
                    s.since = (s.since - PROJECTILE_INC_TIME).max(0.);
                }
                if let Some(mut f) = launcher_foe {
                    f.since = (f.since - PROJECTILE_INC_TIME).max(0.);
                }
                msg.write(GameMsg::Despawn(projectile_entity));
                if targeted_enemy.keys_not_hit() <= 1 {
                    msg.write(GameMsg::Explosion(target_transform.translation.xy()));
                    audio.write(AudioMsg::Explosion);
                    msg.write(GameMsg::Despawn(target_entity));

                    stats.inc_score(targeted_enemy.num_points());
                    score_change = true;
                } else {
                    msg.write(GameMsg::DespawnText(target_entity));
                    targeted_enemy.cleared += 1;
                    msg.write(GameMsg::AddText(target_entity));
                }
            }
        } else {
            msg.write(GameMsg::Despawn(projectile_entity));
        }
    }
    if score_change {
        stats.show(&mut score_text);
    }
}

fn project_onto_axis(polygon: &[Vec3], axis: Vec2) -> (f32, f32) {
    let mut min = polygon[0].xy().dot(axis);
    let mut max = min;

    for vertex in polygon.iter().skip(1) {
        let proj = vertex.xy().dot(axis);
        min = min.min(proj);
        max = max.max(proj);
    }

    (min, max)
}

fn collide(a: &[Vec3], b: &[Vec3], padding: Option<f32>) -> Option<(Vec2, bool)> {
    let edges_a = a
        .iter()
        .zip(a.iter().cycle().skip(1))
        .map(|(p1, p2)| *p2 - *p1);

    let edges_b = b
        .iter()
        .zip(b.iter().cycle().skip(1))
        .map(|(p1, p2)| *p2 - *p1);

    let mut min_overlap = f32::INFINITY;
    let mut collision_normal = Vec2::ZERO;
    let mut a_to_b = true;

    for edge in edges_a.chain(edges_b) {
        if edge.length_squared() == 0. {
            continue;
        }
        let axis = Vec2::new(-edge.y, edge.x).normalize();

        let (mut min_a, mut max_a) = project_onto_axis(a, axis);
        let (mut min_b, mut max_b) = project_onto_axis(b, axis);

        if let Some(p) = padding {
            min_a -= p;
            min_b -= p;
            max_a += p;
            max_b += p;
        }

        if max_a < min_b || max_b < min_a {
            return None;
        }

        let overlap = f32::min(max_a, max_b) - f32::max(min_a, min_b);
        if overlap < min_overlap {
            min_overlap = overlap;
            collision_normal = axis;
            a_to_b = (max_a - min_b) < (max_b - min_a);
        }
    }

    Some((collision_normal, a_to_b))
}

fn circle_polygon_collide(circle_center: Vec2, radius: f32, polygon: &[Vec3]) -> Option<Vec2> {
    let mut smallest_overlap = f32::MAX;
    let mut smallest_axis = Vec2::ZERO;

    for (p1, p2) in polygon.iter().zip(polygon.iter().cycle().skip(1)) {
        let edge = *p2 - *p1;
        if edge.length_squared() == 0. {
            continue;
        }
        let axis = Vec2::new(-edge.y, edge.x).normalize();

        let (min_poly, max_poly) = project_onto_axis(polygon, axis);
        let proj_center = circle_center.dot(axis);

        let min_circle = proj_center - radius;
        let max_circle = proj_center + radius;

        if max_circle < min_poly || max_poly < min_circle {
            return None;
        }

        let overlap = f32::min(max_poly, max_circle) - f32::max(min_poly, min_circle);
        if overlap < smallest_overlap {
            smallest_overlap = overlap;
            smallest_axis = axis;
        }
    }

    if let Some(closest) = closest_point_on_polygon(circle_center, polygon) {
        let axis = (circle_center - closest).normalize_or_zero();
        if axis.length_squared() != 0. {
            let (min_poly, max_poly) = project_onto_axis(polygon, axis);
            let proj_center = circle_center.dot(axis);

            let min_circle = proj_center - radius;
            let max_circle = proj_center + radius;

            if max_circle < min_poly || max_poly < min_circle {
                return None;
            }

            let overlap = f32::min(max_poly, max_circle) - f32::max(min_poly, min_circle);
            if overlap < smallest_overlap {
                smallest_axis = axis;
            }
        }
    }

    let centroid = polygon.iter().map(|p| p.xy()).sum::<Vec2>() / polygon.len() as f32;
    if (circle_center - centroid).dot(smallest_axis) < 0. {
        smallest_axis = -smallest_axis;
    }

    Some(smallest_axis)
}

fn closest_point_on_polygon(point: Vec2, polygon: &[Vec3]) -> Option<Vec2> {
    let mut closest = None;
    let mut min_dist_sq = f32::INFINITY;

    for (p1, p2) in polygon.iter().zip(polygon.iter().cycle().skip(1)) {
        let a = p1.xy();
        let b = p2.xy();
        let closest_point = closest_point_on_segment(point, a, b);
        let dist_sq = (point - closest_point).length_squared();

        if dist_sq < min_dist_sq {
            min_dist_sq = dist_sq;
            closest = Some(closest_point);
        }
    }

    closest
}

fn closest_point_on_segment(p: Vec2, a: Vec2, b: Vec2) -> Vec2 {
    let ab = b - a;
    let t = ((p - a).dot(ab) / ab.length_squared()).clamp(0., 1.);
    a + ab * t
}

pub fn update_spawned_relations(
    mut tracking: Query<(Entity, &mut Foe), Without<Summoner>>,
    spawning: Query<Entity, With<Summoner>>,
    cache: Res<FoePointsCache>,
) {
    for (entity, mut foe) in &mut tracking {
        if let Some(parent) = foe.spawned_by {
            if spawning.get(parent).is_ok() {
                if let (Some(points), Some(points_parent)) =
                    (cache.0.get(&entity), cache.0.get(&parent))
                    && collide(
                        points.as_slice(),
                        points_parent.as_slice(),
                        Some(SUMMONER_COLLISION_PADDING),
                    )
                    .is_none()
                {
                    foe.spawned_by = None;
                }
            } else {
                foe.spawned_by = None;
            }
        }
    }
}

pub fn enemy_collisions(
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    mut query: Query<(Entity, &mut Foe, &Transform), Without<Player>>,
    stats: Single<(&mut Stats, &mut Text)>,
    window: Single<&Window>,
    cache: Res<FoePointsCache>,
    player: Single<&Transform, With<Player>>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    let mut iter = query.iter_combinations_mut();
    let viewport_width = viewport_width(&window);
    let player_pos = player.translation.xy();

    while let Some(
        [
            (entity_a, mut enemy_a, transform_a),
            (entity_b, mut enemy_b, transform_b),
        ],
    ) = iter.fetch_next()
    {
        let (Some(points_a), Some(points_b)) = (cache.0.get(&entity_a), cache.0.get(&entity_b))
        else {
            continue;
        };

        if enemy_a.spawned_by != Some(entity_b)
            && enemy_b.spawned_by != Some(entity_a)
            && let Some((normal, a_to_b)) = collide(points_a.as_slice(), points_b.as_slice(), None)
        {
            if !enemy_a.colliding {
                enemy_a.collision(
                    &mut msg,
                    &mut audio,
                    entity_a,
                    transform_a.translation,
                    if a_to_b { -normal } else { normal },
                    viewport_width,
                    player_pos,
                );
            }
            if !enemy_b.colliding {
                enemy_b.collision(
                    &mut msg,
                    &mut audio,
                    entity_b,
                    transform_b.translation,
                    if a_to_b { normal } else { -normal },
                    viewport_width,
                    player_pos,
                );
            }
            if in_viewport(
                transform_a.translation,
                viewport_width,
                Vec2::ZERO,
                player_pos,
            ) && in_viewport(
                transform_b.translation,
                viewport_width,
                Vec2::ZERO,
                player_pos,
            ) {
                stats.inc_score(1);
                score_change = true;
            }
        }
    }
    if score_change {
        stats.show(&mut score_text);
    }
}

pub fn explosion_system(
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut query: Query<(Entity, &mut Transform, &mut ExplosionParticle)>,
    time: Res<Time<Virtual>>,
    mut msg: MessageWriter<GameMsg>,
) {
    let dt = time.delta_secs();

    for (entity, mut transform, mut particle) in &mut query {
        particle.lifetime -= dt;
        if let Some(material) = materials.get_mut(&particle.material) {
            let alpha = particle.lifetime.max(0.) / EXPLOSION_PARTICLE_MAX_LIFETIME;
            material
                .color
                .set_alpha(alpha * EXPLOSION_PARTICLE_INITIAL_ALPHA);
        }
        transform.translation.x += particle.velocity.x * dt;
        transform.translation.y += particle.velocity.y * dt;
        if particle.lifetime <= 0. {
            msg.write(GameMsg::Despawn(entity));
        }
    }
}

pub fn player_collisions(
    mut enemies: Query<(Entity, &mut Foe), Without<Player>>,
    obstacles: Query<&Transform, (With<Obstacle>, Without<Player>)>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    player: Single<(Entity, &Transform), With<Player>>,
    indicator: Single<Entity, With<Indicator>>,
    mut stats: Single<&mut Stats>,
    cache: Res<FoePointsCache>,
) {
    if !stats.running {
        return;
    }
    let (player_entity, player_transform) = player.into_inner();
    let player_points = PLAYER_LOCAL_TRIANGLE.map(|p| player_transform.transform_point(p));

    let mut hit = false;
    for (entity, mut e) in enemies.iter_mut() {
        let Some(points) = cache.0.get(&entity) else {
            continue;
        };
        if let Some((normal, a_to_b)) = collide(points.as_slice(), &player_points, None) {
            hit = true;
            e.add_collision(if a_to_b { -normal } else { normal });
        }
    }

    for o_transform in obstacles.iter() {
        if circle_polygon_collide(
            o_transform.translation.xy(),
            OBSTACLE_RADIUS,
            &player_points,
        )
        .is_some()
        {
            hit = true;
        }
    }

    if hit && !INVINCIBLE {
        msg.write(GameMsg::Explosion(player_transform.translation.xy()));
        audio.write(AudioMsg::Explosion);
        msg.write(GameMsg::Invisible(player_entity));
        msg.write(GameMsg::Invisible(*indicator));
        msg.write(GameMsg::GameEnd);
        stats.running = false;
    }
}

fn spawn_location(width: f32, spawner: &mut Spawner) -> Vec2 {
    let w = SPAWN_LOCATION_MULTIPLIER * width / 2.;
    let h = SPAWN_LOCATION_MULTIPLIER * VIEWPORT_HEIGHT / 2.;

    let mut pool = [0; 3];
    let mut count = 0;
    for i in 0..4 {
        if i != spawner.last_side {
            pool[count] = i;
            count += 1;
        }
    }

    spawner.last_side = pool[rand::random_range(0..count)];

    match spawner.last_side {
        0 => Vec2::new(-w, rand::random_range(-h..h)),
        1 => Vec2::new(w, rand::random_range(-h..h)),
        2 => Vec2::new(rand::random_range(-w..w), h),
        _ => Vec2::new(rand::random_range(-w..w), -h),
    }
}

pub fn spawner(
    time: Res<Time<Fixed>>,
    clock: Res<Clock>,
    mut msg: MessageWriter<GameMsg>,
    mut spawner: ResMut<Spawner>,
    window: Single<&Window>,
    config: Res<Config>,
    player: Single<&Transform, With<Player>>,
) {
    spawner.foe_delay -= time.delta_secs();

    if spawner.foe_delay <= 0. {
        let mut rng = rand::rng();
        let weights = foe_weights(clock.0, config.max_difficulty);
        spawner.foe_dist = WeightedIndex::new(weights).unwrap();

        let width = viewport_width(&window);

        let shape = SHAPES[spawner.foe_dist.sample(&mut rng)];

        msg.write(GameMsg::SpawnFoe(
            shape,
            spawn_location(width, &mut spawner) + player.translation.xy(),
            None,
            None,
        ));
        spawner.foe_delay_mu = spawner_foe_delay_mu(clock.0, config.max_difficulty);

        spawner.foe_delay = rand::random_range(
            spawner.foe_delay_mu - SPAWN_DELTA..spawner.foe_delay_mu + SPAWN_DELTA,
        )
    }
}

pub fn reset_collisions(mut enemies: Query<&mut Foe>, mut obstacles: Query<&mut Obstacle>) {
    for mut e in enemies.iter_mut() {
        e.colliding = false;
    }
    for mut o in obstacles.iter_mut() {
        o.colliding = false;
    }
}

pub fn track_selected_enemy(
    indicator: Single<
        (Entity, &mut Indicator, &mut Transform),
        (
            With<Indicator>,
            Without<PlayerLauncher>,
            Without<Targeted>,
            Without<Player>,
        ),
    >,
    player: Single<
        &mut Transform,
        (
            With<Player>,
            Without<PlayerLauncher>,
            Without<Indicator>,
            Without<Targeted>,
        ),
    >,
    launcher: Single<
        &mut Transform,
        (
            With<PlayerLauncher>,
            Without<Indicator>,
            Without<Targeted>,
            Without<Player>,
        ),
    >,
    selected: Query<
        &Transform,
        (
            With<Targeted>,
            Without<Indicator>,
            Without<PlayerLauncher>,
            Without<Player>,
        ),
    >,
    mut msg: MessageWriter<GameMsg>,
) {
    let (indicator_entity, mut indicator, mut indicator_transform) = indicator.into_inner();
    let mut launcher_transform = launcher.into_inner();
    let player_transform = player.into_inner();
    if let Ok(selected_transform) = selected.single() {
        point_launcher(
            &player_transform,
            &mut launcher_transform,
            selected_transform,
        );
        indicator_transform.translation.x = selected_transform.translation.x;
        indicator_transform.translation.y = selected_transform.translation.y;
        if !indicator.active {
            msg.write(GameMsg::Visible(indicator_entity));
            indicator.active = true;
        }
    } else if indicator.active {
        msg.write(GameMsg::Invisible(indicator_entity));
        indicator.active = false;
    }
}

pub fn obstacle_collisions(
    mut enemies: Query<(Entity, &mut Foe, &Transform), (Without<Obstacle>, Without<Player>)>,
    mut obstacles: Query<(&mut Obstacle, &Transform), Without<Foe>>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    window: Single<&Window>,
    cache: Res<FoePointsCache>,
    player: Single<&Transform, (With<Player>, Without<Foe>, Without<Obstacle>)>,
) {
    let viewport_width = viewport_width(&window);
    let player_pos = player.translation.xy();

    for (_, o_transform) in obstacles.iter_mut() {
        for (e_ent, mut e, e_transform) in enemies.iter_mut() {
            let Some(e_points) = cache.0.get(&e_ent) else {
                continue;
            };
            if !e.colliding
                && let Some(normal) = circle_polygon_collide(
                    o_transform.translation.xy(),
                    OBSTACLE_RADIUS,
                    e_points.as_slice(),
                )
            {
                e.collision(
                    &mut msg,
                    &mut audio,
                    e_ent,
                    e_transform.translation,
                    -normal,
                    viewport_width,
                    player_pos,
                );
            }
        }
    }
    let mut iter = obstacles.iter_combinations_mut();
    while let Some([(mut a, a_transform), (mut b, b_transform)]) = iter.fetch_next() {
        if a.colliding || b.colliding {
            continue;
        }
        if a_transform.translation.distance(b_transform.translation) < OBSTACLE_RADIUS * 2. {
            a.colliding = true;
            b.colliding = true;
            let a_to_b = (b_transform.translation - a_transform.translation)
                .xy()
                .normalize();
            let new_speed_a = (a.velocity.length() / 2.).max(OBSTACLE_MOVEMENT_SPEED / 4.);
            let new_speed_b = (b.velocity.length() / 2.).max(OBSTACLE_MOVEMENT_SPEED / 4.);
            a.velocity = -a_to_b * new_speed_a;
            b.velocity = a_to_b * new_speed_b;
        }
    }
}

fn point_launcher(
    player_transform: &Transform,
    launcher_transform: &mut Transform,
    target: &Transform,
) {
    let launcher_global_pos = player_transform.transform_point(launcher_transform.translation);
    let to_target_world = (target.translation - launcher_global_pos).normalize_or_zero();
    let to_target_local = player_transform.rotation.conjugate() * to_target_world;
    let angle = Vec2::Y.angle_to(to_target_local.xy());

    launcher_transform.rotation = Quat::from_rotation_z(angle);
}

pub fn obstacle(
    mut obstacles: Query<(Entity, &mut Obstacle, &mut Transform)>,
    mut msg: MessageWriter<GameMsg>,
    time: Res<Time<Virtual>>,
    window: Single<&Window>,
    player: Single<&Transform, (With<Player>, Without<Obstacle>)>,
) {
    let viewport_width = viewport_width(&window);
    let dt = time.delta_secs();
    let player_pos = player.translation.xy();
    for (ent, o, mut transform) in obstacles.iter_mut() {
        let in_viewport = in_viewport(
            transform.translation,
            viewport_width,
            Vec2::splat(-OBSTACLE_RADIUS),
            player_pos,
        );
        transform.translation += o.velocity.extend(0.) * dt;
        if !in_viewport {
            msg.write(GameMsg::Despawn(ent));
        }
    }
}

pub fn despawner(mut commands: Commands, despawn: Query<(Entity, &ToDespawn)>) {
    for (e, _) in despawn {
        commands.entity(e).despawn();
    }
}

pub fn slowdown_time(
    mut slowdown: Single<&mut Slowdown>,
    rtime: Res<Time<Real>>,
    mut vtime: ResMut<Time<Virtual>>,
    screen: Res<State<GameScreen>>,
    mut next_screen: ResMut<NextState<GameScreen>>,
) {
    if let GameScreen::Running = **screen {
        slowdown.time -= rtime.delta_secs();

        if slowdown.time <= 0. {
            vtime.pause();
            next_screen.set(GameScreen::End);
        } else {
            let total = GAME_OVER_SLOWDOWN_REAL_TIME;
            let linear = 1. - (slowdown.time / total).clamp(0., 1.);
            let eased = linear.powf(0.4);
            let new_speed = 1. - eased * 0.9;
            vtime.set_relative_speed(new_speed);
        }
    }
}

fn launcher_ring_mesh(inner_r: f32, outer_r: f32, progress: f32) -> Mesh {
    let filled = (RING_RESOLUTIONS as f32 * progress.clamp(0., 1.)).ceil() as usize;
    let mut positions: Vec<[f32; 3]> = Vec::with_capacity((filled + 1) * 2);
    let mut indices: Vec<u32> = Vec::with_capacity((filled + 1) * 2);
    for i in 0..=filled {
        let t = if i == filled {
            progress
        } else {
            i as f32 / RING_RESOLUTIONS as f32
        };
        let theta = FRAC_PI_2 - t * TAU;
        let (sin, cos) = theta.sin_cos();
        positions.push([outer_r * cos, outer_r * sin, 0.]);
        positions.push([inner_r * cos, inner_r * sin, 0.]);
        indices.push((i * 2) as u32);
        indices.push((i * 2 + 1) as u32);
    }
    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleStrip,
        bevy::asset::RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_indices(bevy::mesh::Indices::U32(indices));
    mesh
}

pub fn update_launcher_ring(
    fuel: Res<Fuel>,
    mut meshes: ResMut<Assets<Mesh>>,
    ring: Single<(&Mesh2d, &mut Visibility), With<LauncherRing>>,
    stats: Single<&Stats>,
    time: Res<Time<Virtual>>,
) {
    let m = fuel.0;
    let (mesh2d, mut visibility) = ring.into_inner();
    *visibility = if stats.running && !time.is_paused() && m > 0. {
        Visibility::Inherited
    } else {
        Visibility::Hidden
    };
    if fuel.is_changed()
        && let Some(mesh) = meshes.get_mut(mesh2d.id())
    {
        *mesh = launcher_ring_mesh(
            PLAYER_RING_RADIUS - PLAYER_RING_THICKNESS / 2.,
            PLAYER_RING_RADIUS + PLAYER_RING_THICKNESS / 2.,
            m,
        );
    }
}

pub fn update_countdown_indicators(
    mut indicators: Query<(&mut Transform, &ChildOf), With<CountdownIndicator>>,
    summoners: Query<&Summoner>,
    launchers: Query<&Launcher>,
) {
    for (mut transform, child_of) in &mut indicators {
        let parent = child_of.parent();
        if let Ok(summoner) = summoners.get(parent) {
            let progress = summoner.since / summoner.delay;
            transform.scale =
                Vec3::splat(COUNTDOWN_START_SCALE - (COUNTDOWN_START_SCALE - 1.0) * progress);
        } else if let Ok(launcher) = launchers.get(parent) {
            let progress = (launcher.since / launcher.delay).min(1.0);
            transform.scale =
                Vec3::splat(COUNTDOWN_START_SCALE - (COUNTDOWN_START_SCALE - 1.0) * progress);
        }
    }
}

pub fn lock_enemy_text(
    mut text_query: Query<(&mut Transform, &ChildOf), (With<EnemyText>, Without<Foe>)>,
    enemy_query: Query<&Transform, (With<Foe>, Without<EnemyText>)>,
) {
    for (mut text_transform, child_of) in text_query.iter_mut() {
        if let Ok(enemy_transform) = enemy_query.get(child_of.parent()) {
            text_transform.rotation = enemy_transform.rotation.inverse();
        }
    }
}

pub fn lock_launcher_ring(
    player: Single<&GlobalTransform, With<Player>>,
    mut ring: Query<&mut Transform, (With<LauncherRing>, Without<Player>)>,
) {
    let (_, rotation, _) = player.to_scale_rotation_translation();
    for mut t in ring.iter_mut() {
        t.rotation = rotation.inverse();
    }
}

pub fn fuel_charge(
    mut fuel: ResMut<Fuel>,
    key: Res<KeyState>,
    mode: Res<Mode>,
    stats: Single<&Stats>,
    time: Res<Time<Virtual>>,
) {
    if !stats.running || time.is_paused() {
        return;
    }
    if INVINCIBLE {
        fuel.0 = 1.;
        return;
    }
    let moving = *mode == Mode::Movement && key.v.is_some();
    if !moving {
        fuel.0 = (fuel.0 + FUEL_PASSIVE_RATE * time.delta_secs()).min(1.);
    }
}

pub fn update_stars(
    camera: Single<&Transform, (With<Camera2d>, Without<StarQuad>)>,
    mut quad: Single<&mut Transform, (With<StarQuad>, Without<Camera2d>)>,
    window: Single<&Window>,
) {
    let cam = camera.translation;
    let vw = VIEWPORT_HEIGHT * window.width() / window.height();
    quad.translation = Vec3::new(cam.x, cam.y, STAR_Z_INDEX);
    quad.scale = Vec3::new(vw, VIEWPORT_HEIGHT, 1.);
}
