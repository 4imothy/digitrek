// SPDX-License-Identifier: MIT

use crate::*;
use bevy::prelude::*;
use rand::{self, Rng, rngs::ThreadRng};

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
    enemies: Query<&Enemy, Without<ToDespawn>>,
    mut config: ResMut<Config>,
    stats: Single<&mut Stats>,
    mut pkv: ResMut<PkvStore>,
) {
    let mut rng = rand::rng();
    for msg in msg.read() {
        match msg {
            GameMsg::Explosion(position) => {
                spawn_explosion(
                    &mut rng,
                    &mut commands,
                    &mut meshes,
                    &mut materials,
                    position,
                );
            }
            GameMsg::Despawn(entity) => {
                commands.entity(*entity).insert(ToDespawn);
            }
            GameMsg::DespawnChildren(entity) => {
                commands.entity(*entity).despawn_related::<Children>();
            }
            GameMsg::ReplaceShape(entity, shape) => {
                replace_shape(&mut commands, &mut meshes, &mut materials, *entity, shape);
            }
            GameMsg::AddText(entity) => {
                let mut cmd = commands.entity(*entity);
                if let Ok(enemy) = enemies.get(*entity)
                    && !enemy.keys.is_empty()
                {
                    add_text(&mut cmd, &enemy.keys, enemy.next_index);
                }
            }
            GameMsg::SpawnEnemy(shape, pos, spawned_by, rot) => {
                spawn_enemy(
                    &mut rng,
                    &mut commands,
                    &mut meshes,
                    &mut materials,
                    *shape,
                    *pos,
                    *rot,
                    *spawned_by,
                    &config,
                );
            }
            GameMsg::SpawnObstacle(pos, direction) => {
                commands.spawn((
                    Obstacle {
                        direction: *direction,
                        entered_viewport: false,
                        time_to_enter_viewport: OBSTACLE_FIELD_TIME_TO_ENTER_VIEWPORT,
                        colliding: false,
                    },
                    Mesh2d(meshes.add(Circle::new(OBSTACLE_RADIUS))),
                    MeshMaterial2d(materials.add(colors::OBSTACLE)),
                    Transform::from_translation(pos.extend(OBSTACLE_Z_INDEX)),
                    FONT.clone(),
                ));
            }
            GameMsg::Invisible(entity) => {
                commands.entity(*entity).insert(Visibility::Hidden);
            }
            GameMsg::Visible(entity) => {
                commands.entity(*entity).insert(Visibility::Visible);
            }
            GameMsg::Select(entity) => {
                commands.entity(*entity).insert(Selected);
            }
            GameMsg::DeSelect(entity) => {
                commands.entity(*entity).remove::<Selected>();
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
        }
    }
}

fn spawn_explosion(
    rng: &mut ThreadRng,
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    loc: &Vec2,
) {
    for _ in 0..rng.random_range(15..30) {
        let angle = rng.random_range(0.0..std::f32::consts::TAU);
        let speed = rng.random_range(50.0..150.);
        let velocity = Vec2::new(angle.cos(), angle.sin()) * speed;

        let color = Color::srgba(
            1.,
            rng.random_range(0.5..1.),
            rng.random_range(0.0..5.),
            EXPLOSION_PARTICLE_INITIAL_ALPHA,
        );

        let material = materials.add(color);

        commands.spawn((
            Mesh2d(meshes.add(Circle::new(rng.random_range(
                EXPLOSION_PARTICLE_RADIUS_LOWER..EXPLOSION_PARTICLE_RADIUS_UPPER,
            )))),
            MeshMaterial2d(material.clone()),
            Transform::from_translation(loc.extend(rng.random_range(
                PARTICLE_Z_INDEX - EXPLOSION_Z_INDEX_RANGE
                    ..PARTICLE_Z_INDEX + EXPLOSION_Z_INDEX_RANGE,
            ))),
            ExplosionParticle {
                velocity,
                lifetime: rng.random_range(0.5..EXPLOSION_PARTICLE_MAX_LIFETIME),
                material,
            },
        ));
    }
}

fn add_text(commands: &mut EntityCommands, keys: &str, next_index: usize) {
    commands.with_children(|commands| {
        let mut chars = keys.chars();
        commands
            .spawn((
                Text2d::new(chars.next().unwrap()),
                TextLayout::default(),
                FONT.clone(),
                TextColor(if next_index == 0 {
                    colors::TEXT_NEXT
                } else {
                    colors::TEXT_DONE
                }),
                EnemyText,
            ))
            .with_children(|commands| {
                for (i, c) in chars.enumerate() {
                    commands.spawn((
                        TextSpan::new(c),
                        TextColor(if next_index == i + 1 {
                            colors::TEXT_NEXT
                        } else if next_index > i + 1 {
                            colors::TEXT_DONE
                        } else {
                            colors::TEXT_FUTURE
                        }),
                        FONT.clone(),
                    ));
                }
            });
    });
}

fn spawn_enemy(
    rng: &mut ThreadRng,
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<ColorMaterial>>,
    shape: Shape,
    pos: Vec2,
    rot: Option<f32>,
    spawned_by: Option<Entity>,
    config: &Config,
) {
    let (mesh, color, num_keys) = match shape {
        Shape::Triangle => (meshes.add(TRIANGLE), colors::TRIANGLE, TRIANGLE_NUM_KEYS),
        Shape::Rhombus => (meshes.add(RHOMBUS), colors::RHOMBUS, RHOMBUS_NUM_KEYS),
        Shape::Pentagon => (meshes.add(PENTAGON), colors::PENTAGON, PENTAGON_NUM_KEYS),
    };

    let keys = (0..num_keys)
        .map(|_| {
            if ONE_KEY {
                config.keypool()[0]
            } else {
                config.keypool()[rng.random_range(0..keys::QWERTY_POOL.len())]
            }
        })
        .collect::<String>();
    let mut ent_cmds = commands.spawn((
        Mesh2d(mesh),
        Transform {
            translation: pos.extend(if matches!(shape, Shape::Triangle | Shape::Rhombus) {
                TRACKING_Z_INDEX
            } else {
                SPAWNER_Z_INDEX
            }),
            rotation: rot.map(Quat::from_rotation_z).unwrap_or_default(),
            ..default()
        },
        MeshMaterial2d(materials.add(color)),
    ));
    match shape {
        Shape::Triangle | Shape::Rhombus => {
            ent_cmds.insert(Tracking);
        }
        Shape::Pentagon => {
            ent_cmds.insert(Spawning {
                time: 0.,
                entered_view: false,
            });
        }
    }
    add_text(&mut ent_cmds, &keys, 0);
    let e = Enemy::new(shape, keys, spawned_by);
    ent_cmds.insert(e);

    if SHOW_LOCAL_POINTS {
        let points: &[Vec3] = points!(shape, Transform::IDENTITY);
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
) {
    let (mesh, color) = match shape {
        Shape::Triangle => (meshes.add(TRIANGLE), materials.add(colors::TRIANGLE)),
        Shape::Rhombus => (meshes.add(RHOMBUS), materials.add(colors::RHOMBUS)),
        Shape::Pentagon => return,
    };
    let mut ent_cmds = commands.entity(entity);
    if let Shape::Rhombus = shape {
        ent_cmds.remove::<Spawning>();
        ent_cmds.insert(Tracking);
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
