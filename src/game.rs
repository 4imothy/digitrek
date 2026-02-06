// SPDX-License-Identifier: MIT

use crate::*;
use bevy::{
    input::{
        ButtonState,
        keyboard::{Key, KeyboardInput},
    },
    prelude::*,
};
use core::f32;
use rand::{self, Rng, distr::Distribution};

pub fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
) {
    let mut launcher_mesh = Mesh::new(
        bevy::render::render_resource::PrimitiveTopology::TriangleStrip,
        RenderAssetUsages::default(),
    );
    let vertices = [
        [-PLAYER_LAUNCHER_WIDTH / 2., 0., 0.],
        [PLAYER_LAUNCHER_WIDTH / 2., 0., 0.],
        [-PLAYER_LAUNCHER_WIDTH / 2., PLAYER_LAUNCHER_LENGTH, 0.],
        [PLAYER_LAUNCHER_WIDTH / 2., PLAYER_LAUNCHER_LENGTH, 0.],
    ];
    let indices = bevy::mesh::Indices::U32(vec![0, 1, 3, 0, 2, 3]);

    launcher_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices.to_vec());
    launcher_mesh.insert_indices(indices);

    commands
        .spawn((
            Mesh2d(meshes.add(CircularSector::new(PLAYER_RADIUS, PI / 6.))),
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
                Mesh2d(meshes.add(launcher_mesh)),
                MeshMaterial2d(materials.add(colors::LAUNCHER)),
                Transform {
                    rotation: Quat::from_rotation_z(PI),
                    translation: Vec3::new(0., PLAYER_RADIUS / 2., -1.),
                    ..default()
                },
                PlayerLauncher,
            ));
        })
        .with_children(|cmds| {
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

    let segments = 32;
    let inner_radius = INDICATOR_RADIUS - INDICATOR_THICKNESS / 2.;
    let outer_radius = INDICATOR_RADIUS + INDICATOR_THICKNESS / 2.;

    let mut positions = Vec::with_capacity(segments * 2);
    let mut indices = Vec::with_capacity((segments + 1) * 2);

    for i in 0..=segments {
        let theta = i as f32 / segments as f32 * std::f32::consts::TAU;
        let (sin, cos) = theta.sin_cos();

        let outer = [outer_radius * cos, outer_radius * sin, 0.];
        let inner = [inner_radius * cos, inner_radius * sin, 0.];

        positions.push(outer);
        positions.push(inner);

        indices.push((i * 2) as u32);
        indices.push((i * 2 + 1) as u32);
    }

    let mut indicator_mesh = Mesh::new(
        bevy::render::render_resource::PrimitiveTopology::TriangleStrip,
        RenderAssetUsages::default(),
    );
    indicator_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    indicator_mesh.insert_indices(bevy::mesh::Indices::U32(indices));

    commands
        .spawn((
            Indicator { active: false },
            Mesh2d(meshes.add(indicator_mesh)),
            MeshMaterial2d(materials.add(colors::INDICATOR)),
            Transform::from_xyz(0., 0., INDICATOR_Z_INDEX),
            Visibility::Hidden,
        ))
        .with_children(|cmds| {
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                    INDICATOR_THICKNESS,
                ))),
                MeshMaterial2d(materials.add(colors::INDICATOR)),
                Transform::from_xyz(-INDICATOR_RADIUS, 0., PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                    INDICATOR_THICKNESS,
                ))),
                MeshMaterial2d(materials.add(colors::INDICATOR)),
                Transform::from_xyz(INDICATOR_RADIUS, 0., PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_THICKNESS,
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                ))),
                MeshMaterial2d(materials.add(colors::INDICATOR)),
                Transform::from_xyz(0., INDICATOR_RADIUS, PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_THICKNESS,
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                ))),
                MeshMaterial2d(materials.add(colors::INDICATOR)),
                Transform::from_xyz(0., -INDICATOR_RADIUS, PLAYER_Z_INDEX),
            ));
        });
    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(SCORE_TEXT_PADDING),
            left: Val::Px(SCORE_TEXT_PADDING),
            ..default()
        },
        Text::new("0"),
        FONT.clone(),
        Stats {
            score: 0,
            running: true,
        },
    ));
    commands.insert_resource(Spawner {
        foe_dist: WeightedIndex::new(SPAWNER_FOE_WEIGHTS).unwrap(),
        foe_delay: FIRST_FOE_SPAWN_DELAY,
        foe_spawns_since: [0, 0, 0, 0],
        foe_delay_mu: 0.,
        last_side: 0,
    });
    commands.insert_resource(AudioAssets {
        projectile_launch: asset_server.load("projectile_launch.ogg"),
        unmatched_keypress: asset_server.load("unmatched_keypress.ogg"),
        explosion: asset_server.load("explosion.ogg"),
    });
    commands.insert_resource(Clock(0.));
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
}

pub fn update_clock(mut clock: ResMut<Clock>, time: Res<Time<Virtual>>) {
    clock.0 += time.delta_secs();
}

fn explosion_in_viewport(
    pos: &Vec3,
    viewport_width: f32,
    msg: &mut MessageWriter<GameMsg>,
    audio: &mut MessageWriter<AudioMsg>,
) {
    if in_viewport(pos, viewport_width, Vec2::ZERO) {
        msg.write(GameMsg::Explosion(pos.xy()));
        audio.write(AudioMsg::Explosion);
    }
}

impl Foe {
    pub fn add_collision(&mut self, collision_normal: Vec2) {
        self.bounce_velocity = Some(collision_normal * self.mov_speed() * BOUNCE_MULTIPLIER);
        self.bounce_timer = BOUNCE_DURATION;
    }

    pub fn collision(
        &mut self,
        msg: &mut MessageWriter<GameMsg>,
        audio: &mut MessageWriter<AudioMsg>,
        entity: Entity,
        pos: &Vec3,
        normal: Vec2,
        viewport_width: f32,
    ) {
        self.colliding = true;
        if let Some(shape) = self.shape.downgraded_shape() {
            self.shape = shape;
            self.skipped += 1;
            if self.cleared + self.skipped >= self.orig_len {
                explosion_in_viewport(pos, viewport_width, msg, audio);
                msg.write(GameMsg::Despawn(entity));
            } else {
                self.add_collision(normal);
                match shape {
                    Shape::Pentagon => {
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
            explosion_in_viewport(pos, viewport_width, msg, audio);
            msg.write(GameMsg::Despawn(entity));
        }
    }

    pub fn mov_speed(&self) -> f32 {
        match self.shape {
            Shape::Triangle => TRIANGLE_MOVEMENT_SPEED,
            Shape::Rhombus => RHOMBUS_MOVEMENT_SPEED,
            Shape::Pentagon => PENTAGON_MOVEMENT_SPEED,
            Shape::Hexagon => HEXAGON_MOVEMENT_SPEED,
        }
    }

    pub fn rot_speed(&self) -> f32 {
        match self.shape {
            Shape::Triangle => TRIANGLE_ROTATION_SPEED,
            Shape::Rhombus => RHOMBUS_ROTATION_SPEED,
            Shape::Pentagon => PENTAGON_ROTATION_SPEED,
            Shape::Hexagon => HEXAGON_ROTATION_SPEED,
        }
    }

    pub fn num_points(&self) -> usize {
        match self.shape {
            Shape::Triangle => 3,
            Shape::Rhombus => 4,
            Shape::Pentagon => 5,
            Shape::Hexagon => 6,
        }
    }
}

impl Stats {
    pub fn show(&self, text: &mut Text) {
        **text = format!("{}", self.score);
    }
    pub fn alter_score(&mut self, dif: isize) {
        if self.running {
            self.score = self.score.saturating_add_signed(dif);
        }
    }
    pub fn save_high_score(&self, config: &mut ResMut<Config>, pkv: &mut ResMut<PkvStore>) {
        if self.score > config.high_score {
            config.high_score = self.score;
            let _ = pkv.set(HIGH_SCORE_KEY, &config.high_score);
        }
    }
}

pub fn keypress(
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    mut evr_kbd: MessageReader<KeyboardInput>,
    player: Single<(&mut Player, &Transform), (With<Player>, Without<PlayerLauncher>)>,
    launcher: Single<&mut Transform, (With<PlayerLauncher>, Without<Player>)>,
    indicator: Single<Entity, (With<Indicator>, Without<Player>)>,
    mut enemy_query: Query<
        (Entity, &mut Foe, &mut Transform),
        (Without<Player>, Without<Indicator>, Without<PlayerLauncher>),
    >,
    time: ResMut<Time<Virtual>>,
    stats: Single<&mut Stats>,
    config: Res<Config>,
) {
    let (mut player, player_transform) = player.into_inner();
    if !stats.running {
        evr_kbd.clear();
        return;
    }
    let mut found_selected = false;
    let mut launcher_transform = launcher.into_inner();
    for ev in evr_kbd.read() {
        if ev.state != ButtonState::Pressed {
            continue;
        }
        if time.is_paused() {
            continue;
        }
        match &ev.logical_key {
            Key::Backspace | Key::Space => {
                msg.write(GameMsg::Invisible(*indicator));
                if let Some(selected) = player.selected {
                    msg.write(GameMsg::DeSelect(selected));
                }
                player.selected = None;
            }
            Key::Character(str) => {
                if let Some(key) = str.chars().next().map(|c| c.to_ascii_lowercase()) {
                    if !config.keypool().contains(&key) {
                        continue;
                    }
                    if let Some(selected) = player.selected
                        && let Ok((selected_entity, mut selected_enemy, _)) =
                            enemy_query.get_mut(selected)
                    {
                        found_selected = true;
                        if key == selected_enemy.keys[selected_enemy.next_index] {
                            selected_enemy.next_index += 1;
                            msg.write(GameMsg::Projectile(
                                selected_entity,
                                projectile_spawn_location(player_transform, &launcher_transform),
                            ));
                            audio.write(AudioMsg::ProjectileLaunch);
                            if selected_enemy
                                .orig_len
                                .saturating_sub(selected_enemy.next_index + selected_enemy.skipped)
                                == 0
                            {
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
                            if ce.orig_len.saturating_sub(ce.next_index + ce.skipped) > 1 {
                                player.selected = closest_entity;
                                msg.write(GameMsg::Select(e));
                            } else {
                                point_launcher(
                                    player_transform,
                                    &mut launcher_transform,
                                    &closest_enemy_transform.unwrap(),
                                );
                            }

                            ce.next_index += 1;
                            msg.write(GameMsg::DespawnText(e));
                            msg.write(GameMsg::AddText(e));
                            msg.write(GameMsg::Projectile(
                                e,
                                projectile_spawn_location(player_transform, &launcher_transform),
                            ));
                            audio.write(AudioMsg::ProjectileLaunch);
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
        .transform_point(launcher_trans.transform_point(Vec3::new(0., PLAYER_LAUNCHER_LENGTH, 0.)))
        .with_z(EXPLOSION_Z_INDEX)
}

fn viewport_width(win: &Window) -> f32 {
    VIEWPORT_HEIGHT * (win.width() / win.height())
}

pub fn player_movement(
    time: Res<Time<Virtual>>,
    key: Res<KeyState>,
    mut transform: Single<&mut Transform, With<Player>>,
    window: Single<&Window>,
    stats: Single<&Stats>,
) {
    if stats.running {
        let rotation_factor = key.h.map(|v| if v { -1. } else { 1. }).unwrap_or(0.);
        let movement_factor = key.v.map(|v| if v { 1. } else { -1. }).unwrap_or(0.);

        let dt = time.delta_secs();
        let rotation_delta = rotation_factor * PLAYER_ROTATION_SPEED * dt;

        let movement_direction = movement_factor * (transform.rotation * Vec3::Y);
        let movement_distance = PLAYER_MOVEMENT_SPEED * dt;
        let movement = movement_direction * movement_distance;

        transform.rotation *= Quat::from_rotation_z(rotation_delta);

        let mut translation = transform.translation + movement;

        let half_width = viewport_width(&window) / 2.;
        let half_height = VIEWPORT_HEIGHT / 2.;

        translation.x = translation.x.clamp(-half_width, half_width);
        translation.y = translation.y.clamp(-half_height, half_height);

        transform.translation = translation;
    }
}

pub fn summoner(
    time: Res<Time<Virtual>>,
    mut query: Query<(Entity, &mut Summoner, &Transform)>,
    mut msg: MessageWriter<GameMsg>,
    player: Single<&Transform, (With<Player>, Without<Foe>)>,
) {
    let dt = time.delta_secs();
    let mut rng = rand::rng();

    for (ent, mut summoner, transform) in &mut query {
        summoner.since += dt;
        if summoner.since >= summoner.delay {
            let direction = player.translation - transform.translation;
            let angle = direction.y.atan2(direction.x) - std::f32::consts::FRAC_PI_2;

            let shape = SHAPES[summoner.foe_dist.sample(&mut rng)];
            msg.write(GameMsg::SpawnFoe(
                shape,
                transform.translation.xy(),
                Some(ent),
                Some(angle),
            ));
            summoner.since = 0.;
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

    for (mut foe, mut sum, mut transform) in &mut query {
        if !foe.entered_viewport && in_viewport(&transform.translation, viewport_width, Vec2::ZERO)
        {
            foe.entered_viewport = true;
        }
        if foe.bounce_timer > 0. {
            bounce_movement(&mut foe, &mut transform, dt);
        }

        let player_pos = player.translation.truncate();
        let pos = transform.translation.truncate();

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

pub fn launcher_foe(
    time: Res<Time<Virtual>>,
    mut query: Query<(&mut Foe, &mut Launcher, &mut Transform)>,
    window: Single<&Window>,
) {
    let dt = time.delta_secs();
    let viewport_width = viewport_width(&window);

    for (mut foe, mut launcher, mut transform) in &mut query {
        if !foe.entered_viewport && in_viewport(&transform.translation, viewport_width, Vec2::ZERO)
        {
            foe.entered_viewport = true;
        }
        if foe.bounce_timer > 0. {
            bounce_movement(&mut foe, &mut transform, dt);
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
    mut query: Query<(&mut Launcher, &Transform, &Children)>,
    launcher_query: Query<&Transform, With<FoeLauncher>>,
    player: Single<&Transform, With<Player>>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
) {
    let dt = time.delta_secs();

    for (mut launcher, hex_transform, children) in &mut query {
        if !launcher.stopped {
            continue;
        }

        launcher.since += dt;
        if launcher.since >= launcher.delay {
            launcher.since = 0.;

            for child in children.iter() {
                if let Ok(launcher_transform) = launcher_query.get(child) {
                    let tip_local = Vec3::new(HEXAGON_LAUNCHER_LENGTH, 0., 0.);
                    let tip_world = hex_transform
                        .transform_point(launcher_transform.transform_point(tip_local));
                    let direction = (player.translation.xy() - hex_transform.translation.xy())
                        .normalize_or_zero();
                    msg.write(GameMsg::SpawnObstacle(tip_world.xy(), direction));
                    audio.write(AudioMsg::ProjectileLaunch);
                    break;
                }
            }
        }
    }
}

pub fn track_player_for_hexagon(
    mut query: Query<(&Transform, &Children), With<Launcher>>,
    mut launcher_query: Query<&mut Transform, (With<FoeLauncher>, Without<Launcher>)>,
    player: Single<&Transform, (With<Player>, Without<Launcher>, Without<FoeLauncher>)>,
) {
    for (hex_transform, children) in &mut query {
        for child in children.iter() {
            if let Ok(mut launcher_transform) = launcher_query.get_mut(child) {
                let player_world = player.translation.xy();
                let hex_pos = hex_transform.translation.xy();
                let to_player = player_world - hex_pos;
                let target_angle = to_player.to_angle();

                let (_, _, hex_rot_z) = hex_transform.rotation.to_euler(EulerRot::XYZ);
                let local_angle = target_angle - hex_rot_z;

                launcher_transform.rotation = Quat::from_rotation_z(local_angle);
            }
        }
    }
}

fn in_viewport(pos: &Vec3, viewport_width: f32, padding: Vec2) -> bool {
    pos.x > -viewport_width / 2. + padding.x
        && pos.x < viewport_width / 2. - padding.x
        && pos.y > -VIEWPORT_HEIGHT / 2. + padding.y
        && pos.y < VIEWPORT_HEIGHT / 2. - padding.y
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
        if !foe.entered_viewport && in_viewport(&transform.translation, viewport_width, Vec2::ZERO)
        {
            foe.entered_viewport = true;
        }
        let dt = time.delta_secs();
        if foe.bounce_timer > 0. {
            bounce_movement(&mut foe, &mut transform, dt);
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

fn bounce_movement(foe: &mut Foe, transform: &mut Transform, dt: f32) {
    if let Some(mut vel) = foe.bounce_velocity {
        transform.translation += Vec3::new(vel.x, vel.y, 0.) * dt;
        vel *= (1. - BOUNCE_DECAY * dt).clamp(0., 1.);
        if vel.length_squared() < 1. {
            foe.bounce_velocity = None;
            foe.bounce_timer = 0.;
        } else {
            foe.bounce_velocity = Some(vel);
        }
    }
    foe.bounce_timer -= dt;
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
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    for (projectile_entity, projectile, mut projectile_transform) in &mut projectiles_query {
        if let Ok((target_entity, mut targeted_enemy, target_transform, summoner, launcher_foe)) =
            enemies_query.get_mut(projectile.target)
        {
            let e_points: &[Vec3] = points!(targeted_enemy.shape, target_transform);
            let direction = (target_transform.translation - projectile_transform.translation)
                .truncate()
                .normalize_or_zero();
            projectile_transform.translation +=
                direction.extend(0.) * PROJECTILE_MOVEMENT_SPEED * time.delta_secs();

            if circle_polygon_collide(
                projectile_transform.translation.truncate(),
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
                if targeted_enemy
                    .orig_len
                    .saturating_sub(targeted_enemy.cleared + targeted_enemy.skipped)
                    <= 1
                {
                    msg.write(GameMsg::Explosion(target_transform.translation.xy()));
                    audio.write(AudioMsg::Explosion);
                    msg.write(GameMsg::Despawn(target_entity));

                    stats.alter_score(targeted_enemy.num_points() as isize);
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
    let mut min = polygon[0].truncate().dot(axis);
    let mut max = min;

    for vertex in polygon.iter().skip(1) {
        let proj = vertex.truncate().dot(axis);
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

    if (circle_center - polygon[0].xy()).dot(smallest_axis) < 0. {
        smallest_axis = -smallest_axis;
    }

    Some(smallest_axis)
}

fn closest_point_on_polygon(point: Vec2, polygon: &[Vec3]) -> Option<Vec2> {
    let mut closest = None;
    let mut min_dist_sq = f32::INFINITY;

    for (p1, p2) in polygon.iter().zip(polygon.iter().cycle().skip(1)) {
        let a = p1.truncate();
        let b = p2.truncate();
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
    tracking: Query<(&mut Foe, &Transform), Without<Summoner>>,
    spawning: Query<(Entity, &mut Foe, &Transform), With<Summoner>>,
) {
    for (mut foe, transform) in tracking {
        let points: &[Vec3] = points!(foe.shape, transform);
        if let Some(parent) = foe.spawned_by {
            if let Ok((_, parent_enemy, parent_transform)) = spawning.get(parent) {
                let points_parent: &[Vec3] = points!(parent_enemy.shape, parent_transform);
                if collide(points, points_parent, Some(SUMMONER_COLLISION_PADDING)).is_none() {
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
    mut query: Query<(Entity, &mut Foe, &Transform)>,
    stats: Single<(&mut Stats, &mut Text)>,
    window: Single<&Window>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    let mut iter = query.iter_combinations_mut();
    let viewport_width = viewport_width(&window);

    while let Some(
        [
            (entity_a, mut enemy_a, transform_a),
            (entity_b, mut enemy_b, transform_b),
        ],
    ) = iter.fetch_next()
    {
        let points_a: &[Vec3] = points!(enemy_a.shape, transform_a);
        let points_b: &[Vec3] = points!(enemy_b.shape, transform_b);

        if enemy_a.spawned_by.is_none()
            && enemy_b.spawned_by.is_none()
            && let Some((normal, a_to_b)) = collide(points_a, points_b, None)
        {
            if !enemy_a.colliding {
                enemy_a.collision(
                    &mut msg,
                    &mut audio,
                    entity_a,
                    &transform_a.translation,
                    if a_to_b { -normal } else { normal },
                    viewport_width,
                );
            }
            if !enemy_b.colliding {
                enemy_b.collision(
                    &mut msg,
                    &mut audio,
                    entity_b,
                    &transform_b.translation,
                    if a_to_b { normal } else { -normal },
                    viewport_width,
                );
            }
            if in_viewport(&transform_a.translation, viewport_width, Vec2::ZERO)
                && in_viewport(&transform_b.translation, viewport_width, Vec2::ZERO)
            {
                stats.alter_score(1);
            }
            score_change = true;
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
    mut enemies: Query<(&mut Foe, &Transform), Without<Player>>,
    mut obstacles: Query<&Transform, (With<Obstacle>, Without<Player>)>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    player: Single<(Entity, &Transform), With<Player>>,
    mut stats: Single<&mut Stats>,
) {
    if !stats.running {
        return;
    }
    let (player_entity, player_transform) = player.into_inner();
    let player_points = PLAYER_LOCAL_TRIANGLE.map(|p| player_transform.transform_point(p));

    let mut hit = false;
    for (mut e, enemy_transform) in enemies.iter_mut() {
        let points: &[Vec3] = points!(e.shape, enemy_transform);
        if let Some((normal, a_to_b)) = collide(points, &player_points, None) {
            hit = true;
            e.add_collision(if a_to_b { -normal } else { normal });
        }
    }

    for o_transform in obstacles.iter_mut() {
        if circle_polygon_collide(
            o_transform.translation.truncate(),
            OBSTACLE_RADIUS,
            &player_points,
        )
        .is_some()
        {
            hit = true;
        }
    }

    if hit && !INVINCIBLE {
        if stats.running {
            msg.write(GameMsg::Explosion(player_transform.translation.xy()));
            audio.write(AudioMsg::Explosion);
            msg.write(GameMsg::Invisible(player_entity));
            msg.write(GameMsg::GameEnd);
        }
        stats.running = false;
    }
}

fn spawn_location(width: f32, rng: &mut rand::rngs::ThreadRng, spawner: &mut Spawner) -> Vec2 {
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

    spawner.last_side = pool[rng.random_range(0..count)];

    match spawner.last_side {
        0 => Vec2::new(-w, rng.random_range(-h..h)),
        1 => Vec2::new(w, rng.random_range(-h..h)),
        2 => Vec2::new(rng.random_range(-w..w), h),
        _ => Vec2::new(rng.random_range(-w..w), -h),
    }
}

pub fn spawner(
    time: Res<Time<Fixed>>,
    clock: Res<Clock>,
    mut msg: MessageWriter<GameMsg>,
    mut spawner: ResMut<Spawner>,
    window: Single<&Window>,
    config: ResMut<Config>,
) {
    spawner.foe_delay -= time.delta_secs();

    if spawner.foe_delay <= 0. {
        let mut rng = rand::rng();

        let width = viewport_width(&window);

        let shape = SHAPES
            .iter()
            .find(|s| spawner.foe_spawns_since[s.id()] >= FOE_FORCE_SUMMONS[s.id()] as usize)
            .unwrap_or_else(|| &SHAPES[spawner.foe_dist.sample(&mut rng)]);
        for (i, count) in spawner.foe_spawns_since.iter_mut().enumerate() {
            *count = if i == shape.id() { 0 } else { *count + 1 };
        }

        msg.write(GameMsg::SpawnFoe(
            *shape,
            spawn_location(width, &mut rng, &mut spawner),
            None,
            None,
        ));
        spawner.foe_delay_mu = spawner_foe_delay_mu(clock.0, MAX_DIF || config.max_difficulty);

        spawner.foe_delay =
            rng.random_range(spawner.foe_delay_mu - SPAWN_DELTA..spawner.foe_delay_mu + SPAWN_DELTA)
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
    mut enemies: Query<(Entity, &mut Foe, &Transform), Without<Obstacle>>,
    mut obstacles: Query<(&mut Obstacle, &Transform), Without<Foe>>,
    mut msg: MessageWriter<GameMsg>,
    mut audio: MessageWriter<AudioMsg>,
    window: Single<&Window>,
) {
    let viewport_width = viewport_width(&window);

    for (_, o_transform) in obstacles.iter_mut() {
        for (e_ent, mut e, e_transform) in enemies.iter_mut() {
            let e_points: &[Vec3] = points!(e.shape, e_transform);
            if !e.colliding
                && let Some(normal) = circle_polygon_collide(
                    o_transform.translation.truncate(),
                    OBSTACLE_RADIUS,
                    e_points,
                )
            {
                e.collision(
                    &mut msg,
                    &mut audio,
                    e_ent,
                    &e_transform.translation,
                    -normal,
                    viewport_width,
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
            a.direction = -1. * a_to_b / 2.;
            b.direction = a_to_b / 2.;
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
) {
    let viewport_width = viewport_width(&window);
    let dt = time.delta_secs();
    for (ent, o, mut transform) in obstacles.iter_mut() {
        let in_viewport = in_viewport(
            &transform.translation,
            viewport_width,
            Vec2::splat(-OBSTACLE_RADIUS),
        );
        transform.translation.x += o.direction.x * OBSTACLE_MOVEMENT_SPEED * dt;
        transform.translation.y += o.direction.y * OBSTACLE_MOVEMENT_SPEED * dt;
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

pub fn despawn<T: Component>(mut commands: Commands, q: Query<Entity, With<T>>) {
    for e in q {
        commands.entity(e).despawn();
    }
}

pub fn default_state(mut next_screen: ResMut<NextState<GameScreen>>) {
    next_screen.set(GameScreen::default())
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
