// SPDX-License-Identifier: MIT

use crate::*;
use bevy::{camera::visibility::NoFrustumCulling, prelude::*};
use std::f32::consts::{FRAC_PI_2, TAU};

pub fn on_toggle_pause(
    mut msg: MessageReader<PauseMsg>,
    mut time: ResMut<Time<Virtual>>,
    mut next_screen: ResMut<NextState<GameScreen>>,
    game_screen: Res<State<GameScreen>>,
    mut pkv: ResMut<PkvStore>,
    mut config: ResMut<Config>,
    stats: Single<&mut Stats>,
) {
    for msg in msg.read() {
        match msg {
            PauseMsg::TogglePause => {
                if let GameScreen::ResumeCountdown = **game_screen {
                    next_screen.set(GameScreen::Pause);
                } else if let GameScreen::Pause = **game_screen {
                    next_screen.set(GameScreen::ResumeCountdown);
                } else {
                    stats.save_high_score(&mut config, &mut pkv);
                    time.pause();
                    next_screen.set(GameScreen::Pause);
                }
            }
        }
    }
}

pub fn on_msg(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut msg: MessageReader<GameMsg>,
    enemies: Query<&Foe, Without<ToDespawn>>,
    children_query: Query<&Children>,
    enemy_text_query: Query<(), With<EnemyText>>,
    mut config: ResMut<Config>,
    stats: Single<&mut Stats>,
    mut pkv: ResMut<PkvStore>,
    window: Single<&Window>,
    shape_assets: Res<ShapeAssets>,
    shockwaves: Query<(&Shockwave, &Transform)>,
) {
    let viewport_width = window.width() * VIEWPORT_HEIGHT / window.height();
    for msg in msg.read() {
        match msg {
            GameMsg::Explosion(position) => {
                spawn_explosion(&mut commands, &mut meshes, &mut materials, position);
            }
            GameMsg::Despawn(entity) => {
                commands.entity(*entity).insert(ToDespawn);
            }
            GameMsg::DespawnText(entity) => {
                if let Ok(children) = children_query.get(*entity) {
                    for child in children.iter() {
                        if enemy_text_query.contains(child) {
                            commands.entity(child).try_despawn();
                        }
                    }
                }
            }
            GameMsg::DespawnChildren(entity) => {
                commands.entity(*entity).despawn_related::<Children>();
            }
            GameMsg::ReplaceShape(entity, shape) => {
                replace_shape(
                    &mut commands,
                    &mut meshes,
                    &mut materials,
                    *entity,
                    shape,
                    &shape_assets,
                );
            }
            GameMsg::AddText(entity) => {
                let mut cmd = commands.entity(*entity);
                if let Ok(enemy) = enemies.get(*entity)
                    && !enemy.keys.is_empty()
                {
                    add_text(
                        &mut cmd,
                        &enemy.keys,
                        enemy.orig_len.saturating_sub(enemy.cleared + enemy.skipped),
                        enemy.cleared,
                        enemy.next_index,
                    );
                }
            }
            GameMsg::SpawnFoe(shape, pos, spawned_by, rot) => {
                if !inside_shockwave(*pos, &shockwaves) {
                    spawn_foe(
                        &mut commands,
                        &mut meshes,
                        &mut materials,
                        *shape,
                        *pos,
                        *rot,
                        *spawned_by,
                        viewport_width,
                        &shape_assets,
                    );
                }
            }
            GameMsg::SpawnObstacle(pos, direction) => {
                if !inside_shockwave(*pos, &shockwaves) {
                    commands.spawn((
                        Obstacle {
                            velocity: *direction * OBSTACLE_MOVEMENT_SPEED,
                            colliding: false,
                        },
                        Mesh2d(meshes.add(Circle::new(OBSTACLE_RADIUS))),
                        MeshMaterial2d(materials.add(colors::OBSTACLE)),
                        Transform::from_translation(pos.extend(OBSTACLE_Z_INDEX)),
                    ));
                }
            }
            GameMsg::Invisible(entity) => {
                commands.entity(*entity).insert(Visibility::Hidden);
            }
            GameMsg::Visible(entity) => {
                commands.entity(*entity).insert(Visibility::Visible);
            }
            GameMsg::Select(entity) => {
                commands.entity(*entity).insert(Targeted);
            }
            GameMsg::DeSelect(entity) => {
                if let Ok(mut e) = commands.get_entity(*entity) {
                    e.remove::<Targeted>();
                }
            }
            GameMsg::Projectile(entity, origin) => {
                commands.spawn((
                    Mesh2d(meshes.add(Circle::new(PROJECTILE_RADIUS))),
                    MeshMaterial2d(materials.add(colors::PROJECTILE)),
                    Transform::from_translation(*origin),
                    Projectile { target: *entity },
                ));
            }
            GameMsg::GameEnd => {
                stats.save_high_score(&mut config, &mut pkv);
                commands.spawn(Slowdown {
                    time: GAME_OVER_SLOWDOWN_REAL_TIME,
                });
            }
            GameMsg::TriggerShockwave(pos) => {
                let mat = materials.add(colors::GLOW.with_alpha(0.85));
                commands.spawn((
                    Shockwave {
                        radius: PLAYER_RING_RADIUS,
                        material: mat.clone(),
                    },
                    Mesh2d(
                        meshes.add(
                            Annulus::new(
                                (PLAYER_RING_RADIUS - SHOCKWAVE_THICKNESS / 2.).max(0.),
                                PLAYER_RING_RADIUS + SHOCKWAVE_THICKNESS / 2.,
                            )
                            .mesh()
                            .resolution(SHOCKWAVE_RESOLUTION)
                            .build(),
                        ),
                    ),
                    MeshMaterial2d(mat),
                    Transform::from_translation(pos.extend(SHOCKWAVE_Z_INDEX)),
                ));
            }
        }
    }
}

fn inside_shockwave(pos: Vec2, shockwaves: &Query<(&Shockwave, &Transform)>) -> bool {
    shockwaves
        .iter()
        .any(|(sw, t)| pos.distance(t.translation.xy()) < sw.radius + SHOCKWAVE_THICKNESS / 2.)
}

fn spawn_explosion(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    loc: &Vec2,
) {
    for _ in 0..rand::random_range(15..30) {
        let angle = rand::random_range(0.0..std::f32::consts::TAU);
        let speed = rand::random_range(50.0..150.);
        let velocity = Vec2::new(angle.cos(), angle.sin()) * speed;

        let color = colors::EXPLOSION[0]
            .mix(&colors::EXPLOSION[1], rand::random_range(0.0..1.))
            .with_alpha(EXPLOSION_PARTICLE_INITIAL_ALPHA);

        let material = materials.add(color);

        commands.spawn((
            Mesh2d(meshes.add(Circle::new(rand::random_range(
                EXPLOSION_PARTICLE_RADIUS_LOWER..EXPLOSION_PARTICLE_RADIUS_UPPER,
            )))),
            MeshMaterial2d(material.clone()),
            Transform::from_translation(loc.extend(rand::random_range(
                EXPLOSION_Z_INDEX - EXPLOSION_Z_INDEX_RANGE
                    ..EXPLOSION_Z_INDEX + EXPLOSION_Z_INDEX_RANGE,
            ))),
            ExplosionParticle {
                velocity,
                lifetime: rand::random_range(0.5..EXPLOSION_PARTICLE_MAX_LIFETIME),
                material,
            },
        ));
    }
}

fn text_color(i: usize, next: usize) -> TextColor {
    TextColor(if next == i {
        colors::TEXT_NEXT
    } else if next > i {
        colors::TEXT_DONE
    } else {
        colors::TEXT_FUTURE
    })
}

fn add_text(cmd: &mut EntityCommands, keys: &[char], to_show: usize, cleared: usize, next: usize) {
    cmd.with_children(|c| {
        let font = foe_font();
        let mut iter = keys.iter().enumerate().skip(cleared).take(to_show);
        if let Some((i, &ch)) = iter.next() {
            c.spawn((
                Text2d::new(ch),
                TextLayout::default(),
                font.clone(),
                text_color(i, next),
                EnemyText,
                NoFrustumCulling,
            ))
            .with_children(|c| {
                for (i, &ch) in iter {
                    c.spawn((TextSpan::new(ch), text_color(i, next), font.clone()));
                }
            });
        }
    });
}

fn add_foe_launcher(
    ent_cmds: &mut EntityCommands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    arm_length: f32,
    arm_width: f32,
    countdown_sides: usize,
) {
    let mut arm_mesh = Mesh::new(
        bevy::render::render_resource::PrimitiveTopology::TriangleStrip,
        RenderAssetUsages::default(),
    );
    arm_mesh.insert_attribute(
        Mesh::ATTRIBUTE_POSITION,
        vec![
            [0., -arm_width / 2., 0.],
            [0., arm_width / 2., 0.],
            [arm_length, -arm_width / 2., 0.],
            [arm_length, arm_width / 2., 0.],
        ],
    );
    arm_mesh.insert_indices(bevy::mesh::Indices::U32(vec![0, 1, 3, 0, 2, 3]));
    ent_cmds.with_children(|cmd| {
        cmd.spawn((
            FoeLauncher { recoil: 0.0 },
            Mesh2d(meshes.add(arm_mesh)),
            MeshMaterial2d(materials.add(colors::LAUNCHER)),
            Transform::from_xyz(0., 0., -1.),
        ));
    });
    let cd_mesh = meshes.add(countdown_outline_mesh(
        countdown_sides,
        FOE_SIZE,
        COUNTDOWN_THICKNESS,
    ));
    let cd_mat = materials.add(colors::OUTLINE_MESH);
    ent_cmds.with_children(|cmd| {
        cmd.spawn((
            CountdownIndicator,
            Mesh2d(cd_mesh),
            MeshMaterial2d(cd_mat),
            Transform::from_xyz(0., 0., COUNTDOWN_Z_OFFSET)
                .with_scale(Vec3::splat(COUNTDOWN_START_SCALE)),
        ));
    });
}

fn countdown_outline_mesh(sides: usize, radius: f32, thickness: f32) -> Mesh {
    let inner_radius = radius - thickness / 2.;
    let outer_radius = radius + thickness / 2.;
    let mut positions = Vec::with_capacity((sides + 1) * 2);
    let mut indices = Vec::with_capacity((sides + 1) * 2);

    for i in 0..=sides {
        let theta = FRAC_PI_2 + i as f32 / sides as f32 * TAU;
        let (sin, cos) = theta.sin_cos();
        positions.push([outer_radius * cos, outer_radius * sin, 0.]);
        positions.push([inner_radius * cos, inner_radius * sin, 0.]);
        indices.push((i * 2) as u32);
        indices.push((i * 2 + 1) as u32);
    }

    let mut mesh = Mesh::new(
        bevy::render::render_resource::PrimitiveTopology::TriangleStrip,
        RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_indices(bevy::mesh::Indices::U32(indices));
    mesh
}

fn spawn_foe(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    shape: Shape,
    pos: Vec2,
    rot: Option<f32>,
    spawned_by: Option<Entity>,
    viewport_width: f32,
    shape_assets: &ShapeAssets,
) {
    let num_keys = SHAPE_NUM_KEYS[shape.id()];
    let mesh = shape_assets.meshes[shape.id()].clone();

    let z_index = match shape {
        Shape::Triangle | Shape::Rhombus => TRACKING_Z_INDEX,
        Shape::Pentagon => SUMMONER_Z_INDEX,
        Shape::Hexagon => HEXAGON_Z_INDEX,
    };

    let keys: [char; FOE_MAX_NUM_KEYS] = std::array::from_fn(|_| {
        if ONE_KEY {
            KEY_POOL[0]
        } else {
            KEY_POOL[rand::random_range(0..LEN_KEY_POOL)]
        }
    });

    let mut ent_cmds = commands.spawn((
        Mesh2d(mesh),
        Transform {
            translation: pos.extend(z_index),
            rotation: rot.map(Quat::from_rotation_z).unwrap_or_default(),
            ..default()
        },
        MeshMaterial2d(shape_assets.materials[shape.id()].clone()),
    ));
    match shape {
        Shape::Triangle | Shape::Rhombus => {
            ent_cmds.insert(Tracking);
        }
        Shape::Pentagon => {
            ent_cmds.insert(Summoner {
                since: 0.,
                delay: PENTAGON_SPAWN_DELAY,
                leading_vertex: 0,
                foe_dist: WeightedIndex::new(PENTAGON_SUMMON_WEIGHTS).unwrap(),
            });
            add_foe_launcher(
                &mut ent_cmds,
                meshes,
                materials,
                PENTAGON_LAUNCHER_LENGTH,
                PENTAGON_LAUNCHER_WIDTH,
                5,
            );
        }
        Shape::Hexagon => {
            let half_w = viewport_width / 2. - HEXAGON_VIEWPORT_PADDING;
            let half_h = VIEWPORT_HEIGHT / 2. - HEXAGON_VIEWPORT_PADDING;
            let target_pos = Vec2::new(pos.x.clamp(-half_w, half_w), pos.y.clamp(-half_h, half_h));
            ent_cmds.insert(Launcher {
                since: 0.,
                delay: HEXAGON_LAUNCH_DELAY,
                target_pos,
                stopped: false,
            });
            add_foe_launcher(
                &mut ent_cmds,
                meshes,
                materials,
                HEXAGON_LAUNCHER_LENGTH,
                HEXAGON_LAUNCHER_WIDTH,
                6,
            );
        }
    }
    add_text(&mut ent_cmds, &keys, num_keys, 0, 0);
    let e = Foe::new(shape, keys, num_keys, spawned_by);
    ent_cmds.insert(e);

    if SHOW_LOCAL_POINTS {
        let points = shape.local_points();
        let circle = Circle::new(10.);
        let circle_mesh = meshes.add(Mesh::from(circle));
        let circle_material = materials.add(Color::WHITE);

        ent_cmds.with_children(|cmd| {
            for p in points {
                cmd.spawn((
                    Mesh2d(circle_mesh.clone()),
                    MeshMaterial2d(circle_material.clone()),
                    Transform::from_xyz(p.x, p.y, PLAYER_Z_INDEX),
                ));
            }
        });
    }
}

fn replace_shape(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    entity: Entity,
    shape: &Shape,
    shape_assets: &ShapeAssets,
) {
    let color = shape_assets.materials[shape.id()].clone();
    let mesh = shape_assets.meshes[shape.id()].clone();
    let mut ent_cmds = commands.entity(entity);
    match shape {
        Shape::Rhombus => {
            ent_cmds.remove::<Summoner>();
            ent_cmds.insert(Tracking);
        }
        Shape::Pentagon => {
            ent_cmds.remove::<Launcher>();
            ent_cmds.insert(Summoner {
                since: 0.,
                delay: PENTAGON_SPAWN_DELAY,
                leading_vertex: 0,
                foe_dist: WeightedIndex::new(PENTAGON_SUMMON_WEIGHTS).unwrap(),
            });
            add_foe_launcher(
                &mut ent_cmds,
                meshes,
                materials,
                PENTAGON_LAUNCHER_LENGTH,
                PENTAGON_LAUNCHER_WIDTH,
                5,
            );
        }
        _ => {}
    }
    ent_cmds.insert(MeshMaterial2d(color)).insert(Mesh2d(mesh));
}

pub fn on_audio(
    mut commands: Commands,
    mut audio: MessageReader<AudioMsg>,
    sounds: Res<AudioAssets>,
) {
    for a in audio.read() {
        commands.spawn((
            AudioPlayer::<AudioSource>(a.sound(&sounds)),
            PlaybackSettings::DESPAWN,
        ));
    }
}
