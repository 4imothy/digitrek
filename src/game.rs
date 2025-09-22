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
    let indices = bevy::render::mesh::Indices::U32(vec![0, 1, 3, 0, 2, 3]);

    launcher_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices.to_vec());
    launcher_mesh.insert_indices(indices);

    commands
        .spawn((
            Mesh2d(meshes.add(CircularSector::new(PLAYER_RADIUS, PI / 6.0))),
            MeshMaterial2d(materials.add(PLAYER_COLOR)),
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
                MeshMaterial2d(materials.add(LAUNCHER_COLOR)),
                Transform {
                    rotation: Quat::from_rotation_z(PI),
                    translation: Vec3::new(0., PLAYER_RADIUS / 2., -1.),
                    ..default()
                },
                Launcher,
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
    indicator_mesh.insert_indices(bevy::render::mesh::Indices::U32(indices));

    commands
        .spawn((
            Indicator { tracking: false },
            Mesh2d(meshes.add(indicator_mesh)),
            MeshMaterial2d(materials.add(INDICATOR_COLOR)),
            Transform::from_xyz(0., 0., PLAYER_Z_INDEX),
            Visibility::Hidden,
        ))
        .with_children(|cmds| {
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                    INDICATOR_THICKNESS,
                ))),
                MeshMaterial2d(materials.add(INDICATOR_COLOR)),
                Transform::from_xyz(-INDICATOR_RADIUS, 0., PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                    INDICATOR_THICKNESS,
                ))),
                MeshMaterial2d(materials.add(INDICATOR_COLOR)),
                Transform::from_xyz(INDICATOR_RADIUS, 0., PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_THICKNESS,
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                ))),
                MeshMaterial2d(materials.add(INDICATOR_COLOR)),
                Transform::from_xyz(0., INDICATOR_RADIUS, PLAYER_Z_INDEX),
            ));
            cmds.spawn((
                Mesh2d(meshes.add(Rectangle::new(
                    INDICATOR_THICKNESS,
                    INDICATOR_LONG_RECTANGLE_LENGTH,
                ))),
                MeshMaterial2d(materials.add(INDICATOR_COLOR)),
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
        enemy_dist: WeightedIndex::new(ENEMY_SPAWN_WEIGHTS).unwrap(),
        enemy_delay: INITIAL_ENEMY_SPAWN_DELAY,
        friend_delay: FRIEND_SPAWN_DELAY_LOWER,
        enemy_spawns_since: [0, 0, 0],
        delay_lower: INITIAL_ENEMY_SPAWN_DELAY_LOWER,
        delay_upper: INITIAL_ENEMY_SPAWN_DELAY_UPPER,
    });
    commands.insert_resource(AudioAssets {
        projectile_launch: asset_server.load("projectile_launch.ogg"),
        unmatched_keypress: asset_server.load("unmatched_keypress.ogg"),
        explosion: asset_server.load("explosion.ogg"),
        friend_spawn: asset_server.load("pop.ogg"),
        friend_collect: asset_server.load("collect.ogg"),
    });
}

impl Shape {
    pub const fn id(&self) -> usize {
        match self {
            Shape::Triangle => 0,
            Shape::Rhombus => 1,
            Shape::Pentagon => 2,
        }
    }

    pub fn downgraded_shape(&self) -> Option<Self> {
        match self {
            Shape::Triangle => None,
            Shape::Rhombus => Some(Shape::Triangle),
            Shape::Pentagon => Some(Shape::Rhombus),
        }
    }
}

impl Enemy {
    pub fn add_collision(&mut self, collision_normal: Vec2) {
        self.bounce_velocity = Some(collision_normal * self.mov_speed());
        self.bounce_timer = BOUNCE_DURATION;
    }

    pub fn collision_with_enemy(
        &mut self,
        events: &mut EventWriter<GameEvent>,
        audio: &mut EventWriter<AudioEvent>,
        entity: Entity,
        pos: &Vec3,
        normal: Vec2,
    ) {
        self.colliding = true;
        if let Some(shape) = self.shape.downgraded_shape() {
            self.shape = shape;
            self.keys = remove_last(&self.keys);
            if self.keys.is_empty() {
                events.write(GameEvent::Explosion(pos.xy()));
                audio.write(AudioEvent::Explosion);
                events.write(GameEvent::Despawn(entity));
            } else {
                self.add_collision(normal);
                events.write(GameEvent::ReplaceShape(entity, shape));
                events.write(GameEvent::DespawnChildren(entity));
                events.write(GameEvent::AddText(entity));
            }
        } else {
            events.write(GameEvent::Explosion(pos.xy()));
            audio.write(AudioEvent::Explosion);
            events.write(GameEvent::Despawn(entity));
        }
    }

    pub fn mov_speed(&self) -> f32 {
        match self.shape {
            Shape::Triangle => TRIANGLE_MOVEMENT_SPEED,
            Shape::Rhombus => RHOMBUS_MOVEMENT_SPEED,
            Shape::Pentagon => PENTAGON_MOVEMENT_SPEED,
        }
    }

    pub fn rot_speed(&self) -> f32 {
        match self.shape {
            Shape::Triangle => TRIANGLE_ROTATION_SPEED,
            Shape::Rhombus => RHOMBUS_ROTATION_SPEED,
            Shape::Pentagon => PENTAGON_ROTATION_SPEED,
        }
    }

    pub fn num_points(&self) -> usize {
        match self.shape {
            Shape::Triangle => 3,
            Shape::Rhombus => 4,
            Shape::Pentagon => 5,
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
}

pub fn keypress(
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
    mut evr_kbd: EventReader<KeyboardInput>,
    player: Single<(&mut Player, &Transform), (With<Player>, Without<Launcher>)>,
    launcher: Single<&mut Transform, (With<Launcher>, Without<Player>)>,
    indicator: Single<Entity, (With<Indicator>, Without<Player>)>,
    mut enemy_query: Query<
        (Entity, &mut Enemy, &mut Transform),
        (Without<Player>, Without<Indicator>, Without<Launcher>),
    >,
    window: Single<&Window>,
    time: ResMut<Time<Virtual>>,
    stats: Single<&mut Stats>,
) {
    let (mut player, player_transform) = player.into_inner();
    if !stats.running {
        return;
    }
    let viewport_width = viewport_width(&window);

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
                events.write(GameEvent::Invisible(*indicator));
                if let Some(selected) = player.selected {
                    events.write(GameEvent::DeSelect(selected));
                }
                player.selected = None;
            }
            Key::Character(str) => {
                if let Some(key) = str.chars().next().map(|c| c.to_ascii_lowercase()) {
                    if !keys::DVORAK_POOL.contains(&key) {
                        continue;
                    }
                    if let Some(selected) = player.selected {
                        if let Ok((selected_entity, mut selected_enemy, selected_transform)) =
                            enemy_query.get_mut(selected)
                        {
                            found_selected = true;
                            if let Some(to_press) =
                                selected_enemy.keys.chars().nth(selected_enemy.next_index)
                            {
                                if key == to_press {
                                    events.write(GameEvent::Projectile(
                                        selected_entity,
                                        projectile_spawn_location(
                                            player_transform,
                                            &launcher_transform,
                                        ),
                                    ));
                                    audio.write(AudioEvent::ProjectileLaunch);
                                    if selected_enemy
                                        .keys
                                        .len()
                                        .saturating_sub(selected_enemy.next_index)
                                        == 1
                                    {
                                        events.write(GameEvent::Invisible(*indicator));
                                        events.write(GameEvent::DeSelect(selected));
                                        player.selected = None;
                                    } else {
                                        launcher_point(
                                            player_transform,
                                            &mut launcher_transform,
                                            &selected_transform,
                                        );
                                    }
                                    selected_enemy.next_index += 1;
                                    events.write(GameEvent::DespawnChildren(selected_entity));
                                    events.write(GameEvent::AddText(selected_entity));
                                } else {
                                    audio.write(AudioEvent::UnmatchedKeypress);
                                }
                            }
                        }
                    }
                    if !found_selected {
                        player.selected = None;
                        let mut closest_distance = f32::MAX;
                        let mut closest_entity = None;
                        let mut closest_enemy = None;
                        let mut closest_enemy_transform = None;

                        for (entity, enemy, enemy_transform) in enemy_query.iter_mut() {
                            if enemy.next_index < enemy.keys.len()
                                && in_viewport(&enemy_transform.translation, viewport_width, None)
                            {
                                if let Some(enemy_key) = enemy.keys.chars().next() {
                                    if enemy_key == key {
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
                            }
                        }

                        if let Some(e) = closest_entity {
                            let mut ce = closest_enemy.unwrap();
                            if ce.keys.len().saturating_sub(ce.next_index) > 1 {
                                player.selected = closest_entity;
                                events.write(GameEvent::Select(e));
                            } else {
                                launcher_point(
                                    player_transform,
                                    &mut launcher_transform,
                                    &closest_enemy_transform.unwrap(),
                                );
                            }

                            ce.next_index += 1;
                            events.write(GameEvent::DespawnChildren(e));
                            events.write(GameEvent::AddText(e));
                            events.write(GameEvent::Projectile(
                                e,
                                projectile_spawn_location(player_transform, &launcher_transform),
                            ));
                            audio.write(AudioEvent::ProjectileLaunch);
                        } else {
                            audio.write(AudioEvent::UnmatchedKeypress);
                        }
                        if player.selected.is_none() {
                            events.write(GameEvent::Invisible(*indicator));
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
        .with_z(PARTICLE_Z_INDEX)
}

fn viewport_width(win: &Window) -> f32 {
    VIEWPORT_HEIGHT * (win.width() / win.height())
}

pub fn player_movement(
    time: Res<Time>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut transform: Single<&mut Transform, With<Player>>,
    window: Single<&Window>,
    config: Res<Config>,
    stats: Single<&Stats>,
) {
    if stats.running {
        let mut rotation_factor = 0.;
        let mut movement_factor = 0.;

        if keyboard_input.pressed(config.right()) {
            rotation_factor -= 1.;
        }
        if keyboard_input.pressed(config.left()) {
            rotation_factor += 1.;
        }
        if keyboard_input.pressed(config.up()) {
            movement_factor -= 1.;
        }
        if keyboard_input.pressed(config.down()) {
            movement_factor += 1.;
        }

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

pub fn spawning_enemy(
    time: Res<Time>,
    mut query: Query<(Entity, &mut Enemy, &mut Spawning, &mut Transform)>,
    mut events: EventWriter<GameEvent>,
    window: Single<&Window>,
) {
    let dt = time.delta_secs();
    let viewport_width = viewport_width(&window);

    for (ent, mut enemy, mut spawn, mut transform) in &mut query {
        spawn.time += dt;

        if spawn.time >= SPAWNER_ENEMY_SPAWN_DELAY {
            if let Shape::Pentagon = enemy.shape {
                events.write(GameEvent::SpawnEnemy(
                    Shape::Triangle,
                    transform.translation.xy(),
                    Some(ent),
                ));
            }
            spawn.time = 0.;
        }

        let pos = transform.translation;
        if enemy.bounce_timer > 0. {
            collision_movement(&mut enemy, &mut transform, dt);
        } else if spawn.entered_view || in_viewport(&pos, viewport_width, Some(0.2)) {
            spawn.entered_view = true;
            let radius = pos.length();
            if radius > 0. {
                let dir = Vec2::new(-pos.y, pos.x).normalize();
                let movement = dir * enemy.mov_speed() * dt;

                transform.translation.x += movement.x;
                transform.translation.y += movement.y;

                let angle = movement.y.atan2(movement.x);
                transform.rotation = Quat::from_rotation_z(angle);
            }
        } else {
            move_and_rotate_towards(
                &mut transform,
                Vec2::ZERO,
                enemy.rot_speed(),
                enemy.mov_speed(),
                dt,
            );
        }
    }
}

fn in_viewport(pos: &Vec3, viewport_width: f32, padding_factor: Option<f32>) -> bool {
    let (padding_y, padding_x) = padding_factor
        .map(|p| (VIEWPORT_HEIGHT * p, viewport_width * p))
        .unwrap_or((0., 0.));
    pos.x > -viewport_width / 2. + padding_x
        && pos.x < viewport_width / 2. - padding_x
        && pos.y > -VIEWPORT_HEIGHT / 2. + padding_y
        && pos.y < VIEWPORT_HEIGHT / 2. - padding_y
}

pub fn tracking_enemy(
    time: Res<Time>,
    mut query: Query<(&mut Enemy, &mut Transform), (With<Tracking>, Without<Player>)>,
    player_transform: Single<&Transform, With<Player>>,
) {
    let player_translation = player_transform.translation.xy();

    for (mut enemy, mut enemy_transform) in &mut query {
        let dt = time.delta_secs();
        if enemy.bounce_timer > 0. {
            collision_movement(&mut enemy, &mut enemy_transform, dt);
        } else {
            move_and_rotate_towards(
                &mut enemy_transform,
                player_translation,
                enemy.rot_speed(),
                enemy.mov_speed(),
                dt,
            );
        }
    }
}

fn collision_movement(enemy: &mut Enemy, transform: &mut Transform, dt: f32) {
    if let Some(mut vel) = enemy.bounce_velocity {
        transform.translation += Vec3::new(vel.x, vel.y, 0.) * dt;
        vel *= (1. - BOUNCE_DECAY * dt).clamp(0., 1.);
        if vel.length_squared() < 1. {
            enemy.bounce_velocity = None;
            enemy.bounce_timer = 0.;
        } else {
            enemy.bounce_velocity = Some(vel);
        }
    }
    enemy.bounce_timer -= dt;
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

pub fn projectile_movement(
    mut projectiles_query: Query<(Entity, &mut Projectile, &mut Transform), With<Projectile>>,
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
    mut enemies_query: Query<(Entity, &mut Enemy, &Transform), (Without<Projectile>,)>,
    stats: Single<(&mut Stats, &mut Text)>,
    time: Res<Time>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    for (projectile_entity, projectile, mut projectile_transform) in &mut projectiles_query {
        if let Ok((target_entity, mut targeted_enemy, target_transform)) =
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
                events.write(GameEvent::Despawn(projectile_entity));
                if targeted_enemy.keys.len() <= 1 {
                    events.write(GameEvent::Explosion(target_transform.translation.xy()));
                    audio.write(AudioEvent::Explosion);
                    events.write(GameEvent::Despawn(target_entity));

                    stats.alter_score(targeted_enemy.num_points() as isize);
                    score_change = true;
                } else {
                    events.write(GameEvent::DespawnChildren(target_entity));
                    targeted_enemy.next_index -= 1;
                    targeted_enemy.keys.remove(0);
                    events.write(GameEvent::AddText(target_entity));
                }
            }
        } else {
            events.write(GameEvent::Despawn(projectile_entity));
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

pub fn spawner_collisions(
    tracking: Query<(&mut Enemy, &Transform), Without<Spawning>>,
    spawning: Query<(Entity, &mut Enemy, &Transform), With<Spawning>>,
) {
    for (mut enemy, transform) in tracking {
        let points: &[Vec3] = points!(enemy.shape, transform);
        if let Some(parent) = enemy.spawned_by {
            if let Ok((_, parent_enemy, parent_transform)) = spawning.get(parent) {
                let points_parent: &[Vec3] = points!(parent_enemy.shape, parent_transform);
                if collide(points, points_parent, Some(SPAWNER_CHILD_COLLISION_PADDING)).is_none() {
                    enemy.spawned_by = None;
                }
            } else {
                enemy.spawned_by = None;
            }
        }
    }
}

pub fn enemy_collisions(
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
    mut query: Query<(Entity, &mut Enemy, &Transform)>,
    stats: Single<(&mut Stats, &mut Text)>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    let mut iter = query.iter_combinations_mut();

    while let Some(
        [
            (entity_a, mut enemy_a, transform_a),
            (entity_b, mut enemy_b, transform_b),
        ],
    ) = iter.fetch_next()
    {
        let points_a: &[Vec3] = points!(enemy_a.shape, transform_a);
        let points_b: &[Vec3] = points!(enemy_b.shape, transform_b);

        if enemy_a.spawned_by.is_none() && enemy_b.spawned_by.is_none() {
            if let Some((normal, a_to_b)) = collide(points_a, points_b, None) {
                if !enemy_a.colliding {
                    enemy_a.collision_with_enemy(
                        &mut events,
                        &mut audio,
                        entity_a,
                        &transform_a.translation,
                        if a_to_b { -normal } else { normal },
                    );
                }
                if !enemy_b.colliding {
                    enemy_b.collision_with_enemy(
                        &mut events,
                        &mut audio,
                        entity_b,
                        &transform_b.translation,
                        if a_to_b { normal } else { -normal },
                    );
                }
                stats.alter_score(1);
                score_change = true;
            }
        }
    }
    if score_change {
        stats.show(&mut score_text);
    }
}

fn remove_last(orig: &str) -> String {
    orig[..orig.len().saturating_sub(1)].to_string()
}

pub fn explosion_system(
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut query: Query<(Entity, &mut Transform, &mut ExplosionParticle)>,
    time: Res<Time>,
    mut events: EventWriter<GameEvent>,
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
            events.write(GameEvent::Despawn(entity));
        }
    }
}

pub fn player_collisions(
    mut enemies: Query<(&mut Enemy, &Transform), Without<Player>>,
    mut friends: Query<(Entity, &mut Friend, &Transform), Without<Player>>,
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
    player: Single<(Entity, &Transform), With<Player>>,
    stats: Single<(&mut Stats, &mut Text)>,
) {
    let (player_entity, player_transform) = player.into_inner();
    let player_points = PLAYER_LOCAL_TRIANGLE.map(|p| player_transform.transform_point(p));
    let (mut stats, mut score_text) = stats.into_inner();

    for (mut e, enemy_transform) in enemies.iter_mut() {
        let points: &[Vec3] = points!(e.shape, enemy_transform);
        if let Some((normal, a_to_b)) = collide(points, &player_points, None) {
            if stats.running {
                if !PLAYER_IMMUNE {
                    stats.running = false;
                    audio.write(AudioEvent::Explosion);
                    events.write(GameEvent::Invisible(player_entity));
                    events.write(GameEvent::Explosion(player_transform.translation.xy()));
                    events.write(GameEvent::GameEnd);
                }
                e.add_collision(if a_to_b { -normal } else { normal });
            }
        }
    }
    let mut score_change = false;

    for (ent, mut f, f_transform) in friends.iter_mut() {
        if !f.colliding
            && circle_polygon_collide(
                f_transform.translation.truncate(),
                FRIEND_RADIUS,
                &player_points,
            )
            .is_some()
        {
            f.colliding = true;
            score_change = true;
            stats.alter_score(f.value as isize);
            events.write(GameEvent::Despawn(ent));
            audio.write(AudioEvent::CollectFriend);
        }
    }
    if score_change {
        stats.show(&mut score_text);
    }
}

pub fn spawn_enemies(
    time: Res<Time>,
    mut events: EventWriter<GameEvent>,
    mut spawner: ResMut<Spawner>,
    window: Single<&Window>,
) {
    spawner.enemy_delay -= time.delta_secs();

    if spawner.enemy_delay <= 0. {
        let mut rng = rand::rng();
        let angle = rng.random_range(0.0..std::f32::consts::TAU);

        let width = viewport_width(&window);
        let x = width * 0.5 * ENEMY_SPAWN_LOCATION_MULTIPLIER * angle.cos();
        let y = VIEWPORT_HEIGHT * 0.5 * ENEMY_SPAWN_LOCATION_MULTIPLIER * angle.sin();
        let shape = SHAPES
            .iter()
            .find(|s| spawner.enemy_spawns_since[s.id()] >= ENEMY_FORCE_SUMMONS[s.id()] as usize)
            .unwrap_or_else(|| &SHAPES[spawner.enemy_dist.sample(&mut rng)]);
        for (i, count) in spawner.enemy_spawns_since.iter_mut().enumerate() {
            *count = if i == shape.id() { 0 } else { *count + 1 };
        }

        events.write(GameEvent::SpawnEnemy(*shape, Vec2::new(x, y), None));

        spawner.delay_lower =
            (spawner.delay_lower * SPAWN_ENEMY_DELAY_DECAY_RATE).max(SPAWN_ENEMY_DELAY_MIN_LOWER);
        spawner.delay_upper =
            (spawner.delay_upper * SPAWN_ENEMY_DELAY_DECAY_RATE).max(SPAWN_ENEMY_DELAY_MIN_UPPER);

        let t = rng.random::<f32>().powf(2.);
        spawner.enemy_delay = spawner.delay_lower + t * (spawner.delay_upper - spawner.delay_lower);
    }
}

pub fn spawn_friends(
    time: Res<Time>,
    mut events: EventWriter<GameEvent>,
    mut spawner: ResMut<Spawner>,
    friends: Query<&Transform, Without<Enemy>>,
    mut audio: EventWriter<AudioEvent>,
    window: Single<&Window>,
) {
    spawner.friend_delay -= time.delta_secs();

    if spawner.friend_delay <= 0. {
        let mut rng = rand::rng();

        let width = viewport_width(&window);
        let half_width = (width * FRIEND_SPAWN_LOCATION_MULTIPLIER) * 0.5;
        let half_height = (VIEWPORT_HEIGHT * FRIEND_SPAWN_LOCATION_MULTIPLIER) * 0.5;
        let mut overlap = true;
        let mut pos = Vec2::ZERO;
        let mut tries = 0;
        while overlap {
            let x = rng.random_range(-half_width..half_width);
            let y = rng.random_range(-half_height..half_height);
            pos = Vec2::new(x, y);
            overlap = false;
            for t in friends {
                if pos.distance(t.translation.xy()) > FRIEND_RADIUS * 2. {
                    overlap = false;
                }
            }
            tries += 1;
            if tries >= 5 {
                overlap = false;
            }
        }
        events.write(GameEvent::SpawnFriend(pos));
        audio.write(AudioEvent::SpawnFriend);
        spawner.friend_delay = rng.random_range(FRIEND_SPAWN_DELAY_LOWER..FRIEND_SPAWN_DELAY_UPPER);
    }
}

pub fn reset_collisions(mut query: Query<&mut Enemy>) {
    for mut e in query.iter_mut() {
        e.colliding = false;
    }
}

pub fn track_selected_enemy(
    indicator: Single<
        (Entity, &mut Indicator, &mut Transform),
        (
            With<Indicator>,
            Without<Launcher>,
            Without<Selected>,
            Without<Player>,
        ),
    >,
    player: Single<
        &mut Transform,
        (
            With<Player>,
            Without<Launcher>,
            Without<Indicator>,
            Without<Selected>,
        ),
    >,
    launcher: Single<
        &mut Transform,
        (
            With<Launcher>,
            Without<Indicator>,
            Without<Selected>,
            Without<Player>,
        ),
    >,
    selected: Query<
        &Transform,
        (
            With<Selected>,
            Without<Indicator>,
            Without<Launcher>,
            Without<Player>,
        ),
    >,
    mut events: EventWriter<GameEvent>,
) {
    let (indicator_entity, mut indicator, mut indicator_transform) = indicator.into_inner();
    let mut launcher_transform = launcher.into_inner();
    let player_transform = player.into_inner();
    if let Ok(selected_transform) = selected.single() {
        launcher_point(
            &player_transform,
            &mut launcher_transform,
            selected_transform,
        );

        indicator_transform.translation = selected_transform.translation;
        if !indicator.tracking {
            events.write(GameEvent::Visible(indicator_entity));
            indicator.tracking = true;
        }
    } else if indicator.tracking {
        events.write(GameEvent::Invisible(indicator_entity));
        indicator.tracking = false;
    }
}

pub fn friend_enemy_collisions(
    mut enemies: Query<(Entity, &mut Enemy, &Transform), Without<Friend>>,
    mut friends: Query<(Entity, &mut Friend, &Transform), Without<Enemy>>,
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
) {
    for (f_ent, mut f, f_transform) in friends.iter_mut() {
        for (e_ent, mut e, e_transform) in enemies.iter_mut() {
            let e_points: &[Vec3] = points!(e.shape, e_transform);
            if !f.colliding && !e.colliding {
                if let Some(normal) = circle_polygon_collide(
                    f_transform.translation.truncate(),
                    FRIEND_RADIUS,
                    e_points,
                ) {
                    f.colliding = true;
                    events.write(GameEvent::Despawn(f_ent));
                    events.write(GameEvent::Explosion(f_transform.translation.xy()));
                    audio.write(AudioEvent::Explosion);
                    e.collision_with_enemy(
                        &mut events,
                        &mut audio,
                        e_ent,
                        &e_transform.translation,
                        -normal,
                    );
                }
            }
        }
    }
}

fn launcher_point(
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

pub fn update_friends(
    mut friends: Query<(Entity, &mut Friend, &mut Text2d, &Transform), Without<Player>>,
    mut events: EventWriter<GameEvent>,
    mut audio: EventWriter<AudioEvent>,
    stats: Single<(&mut Stats, &mut Text)>,
    time: Res<Time>,
) {
    let (mut stats, mut score_text) = stats.into_inner();
    let mut score_change = false;
    for (ent, mut f, mut text, transform) in friends.iter_mut() {
        f.lifetime += time.delta_secs();
        if f.lifetime > (FRIEND_START_VALUE + 1 - f.value) as f32 {
            if f.value == 1 {
                stats.alter_score(-(FRIEND_POINT_DEDUCTION as isize));
                score_change = true;
                events.write(GameEvent::Despawn(ent));
                events.write(GameEvent::Explosion(transform.translation.xy()));
                audio.write(AudioEvent::Explosion);
            } else {
                f.value -= 1;
                **text = format!("{}", f.value);
            }
        }
    }
    if score_change {
        stats.show(&mut score_text);
    }
}

pub fn despawner(mut commands: Commands, despawn: Query<(Entity, &Despawn)>) {
    for (e, _) in despawn {
        commands.entity(e).despawn();
    }
}

pub fn lock_enemy_text(
    mut text_query: Query<(&mut Transform, &ChildOf), (With<EnemyText>, Without<Enemy>)>,
    enemy_query: Query<&Transform, (With<Enemy>, Without<EnemyText>)>,
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
    mut next_screen: ResMut<NextState<GameScreen>>,
) {
    slowdown.time -= rtime.delta_secs();

    if slowdown.time <= 0.0 {
        vtime.pause();
        next_screen.set(GameScreen::End);
    } else {
        let total = GAME_OVER_SLOWDOWN_REAL_TIME;
        let linear = 1.0 - (slowdown.time / total).clamp(0.0, 1.0);
        let eased = linear.powf(0.4);
        let new_speed = 1.0 - eased * 0.9;
        vtime.set_relative_speed(new_speed);
    }
}
