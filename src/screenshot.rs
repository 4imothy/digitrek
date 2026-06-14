// SPDX-License-Identifier: MIT

use crate::{game::point_launcher, *};
use bevy::{
    prelude::*,
    render::view::screenshot::{Screenshot, ScreenshotCaptured},
};

#[derive(Resource, Default)]
struct ScreenshotFlow {
    elapsed: f32,
    stage: u32,
    rho_ent: Option<Entity>,
    rho_pos: Vec2,
}

pub fn plugin(app: &mut App) {
    app.insert_resource(ScreenshotFlow::default())
        .add_systems(Startup, start_game)
        .add_systems(
            Update,
            orchestrate.run_if(in_state(Screen::Game).and(in_state(GameScreen::Running))),
        );
}

fn start_game(mut next: ResMut<NextState<Screen>>) {
    next.set(Screen::Game);
}

fn facing_rot(foe_pos: Vec2, player_pos: Vec2, leading: Vec2) -> f32 {
    let dir = (player_pos - foe_pos).normalize_or_zero();
    dir.to_angle() - leading.to_angle()
}

fn rhombus_leading(foe_pos: Vec2, player_pos: Vec2) -> Vec2 {
    let dir = (player_pos - foe_pos).normalize_or_zero();
    if Vec2::NEG_Y.dot(dir) > Vec2::Y.dot(dir) {
        Vec2::NEG_Y
    } else {
        Vec2::Y
    }
}

fn orchestrate(
    mut flow: ResMut<ScreenshotFlow>,
    mut commands: Commands,
    mut msg: MessageWriter<GameMsg>,
    mut fuel: ResMut<Fuel>,
    mut spawner: ResMut<Spawner>,
    stats: Single<(&mut Stats, &mut Text)>,
    mut foes: Query<
        (
            Entity,
            &mut Foe,
            &Transform,
            Option<&mut Launcher>,
            Option<&mut Summoner>,
        ),
        Without<PlayerLauncher>,
    >,
    time: Res<Time<Real>>,
    player: Single<&Transform, (With<Player>, Without<Foe>, Without<PlayerLauncher>)>,
    mut launcher: Single<&mut Transform, With<PlayerLauncher>>,
) {
    flow.elapsed += time.delta_secs();
    spawner.foe_delay = 9999.;
    let (mut stats, mut score_text) = stats.into_inner();

    let player_pos = Vec2::new(0., 25.);
    let t = flow.elapsed;
    let stage = flow.stage;

    if stage == 0 && t >= 0.05 {
        flow.stage = 1;
        for (entity, ..) in foes.iter() {
            msg.write(GameMsg::Despawn(entity));
        }

        let tri1 = Vec2::new(-290., 290.);
        let rho1 = Vec2::new(270., 310.);
        let tri2 = Vec2::new(160., -310.);
        let rho2 = Vec2::new(-310., -390.);
        let pent = Vec2::new(-460., -130.);
        let hex = Vec2::new(310., -170.);

        msg.write(GameMsg::SpawnFoe(
            Shape::Triangle,
            tri1,
            None,
            Some(facing_rot(tri1, player_pos, Vec2::Y)),
        ));
        msg.write(GameMsg::SpawnFoe(
            Shape::Rhombus,
            rho1,
            None,
            Some(facing_rot(
                rho1,
                player_pos,
                rhombus_leading(rho1, player_pos),
            )),
        ));
        msg.write(GameMsg::SpawnFoe(
            Shape::Triangle,
            tri2,
            None,
            Some(facing_rot(tri2, player_pos, Vec2::Y)),
        ));
        msg.write(GameMsg::SpawnFoe(
            Shape::Rhombus,
            rho2,
            None,
            Some(facing_rot(
                rho2,
                player_pos,
                rhombus_leading(rho2, player_pos),
            )),
        ));
        msg.write(GameMsg::SpawnFoe(Shape::Pentagon, pent, None, None));
        msg.write(GameMsg::SpawnFoe(Shape::Hexagon, hex, None, None));

        msg.write(GameMsg::Explosion(Vec2::new(100., 310.)));
        stats.score = 4;
        stats.show(&mut score_text);
        msg.write(GameMsg::SpawnObstacle(
            Vec2::new(180., -90.),
            Vec2::new(-0.843, 0.539),
        ));

        fuel.0 = 0.65;
    } else if stage == 1 && t >= 0.25 {
        flow.stage = 2;
        let mut pent_ent_pos: Option<(Entity, Vec2)> = None;

        for (entity, mut foe, transform, launcher_comp, summoner) in foes.iter_mut() {
            foe.entered_viewport = true;
            if let Some(mut l) = launcher_comp {
                l.since = HEXAGON_LAUNCH_DELAY * 0.6;
            }
            if let Some(mut s) = summoner {
                s.since = PENTAGON_SPAWN_DELAY * 0.6;
                pent_ent_pos = Some((entity, transform.translation.xy()));
            }
            if matches!(foe.shape, Shape::Rhombus) && transform.translation.y < 0. {
                foe.cleared = 0;
                foe.next_index = 1;
                flow.rho_ent = Some(entity);
                flow.rho_pos = transform.translation.xy();
            }
        }

        if let Some(entity) = flow.rho_ent {
            msg.write(GameMsg::Select(entity));
            point_launcher(
                &player,
                &mut launcher,
                &Transform::from_translation(flow.rho_pos.extend(0.)),
            );
        }

        if let Some((pent_ent, pent_pos)) = pent_ent_pos {
            let to_player = (player_pos - pent_pos).normalize_or_zero();
            let spawn_pos = pent_pos + to_player * (PENTAGON_LAUNCHER_LENGTH * 2.8);
            let rot = facing_rot(spawn_pos, player_pos, Vec2::Y);
            msg.write(GameMsg::SpawnFoe(
                Shape::Triangle,
                spawn_pos,
                Some(pent_ent),
                Some(rot),
            ));
        }
    } else if stage == 2 && t >= 0.35 {
        flow.stage = 3;
        for (_, mut foe, ..) in foes.iter_mut() {
            foe.entered_viewport = true;
        }
    } else if stage == 3 {
        flow.stage = 4;
        if let Some(target) = flow.rho_ent {
            let dir = (flow.rho_pos - player_pos).normalize_or_zero();
            let dist = (flow.rho_pos - player_pos).length();
            let origin = (player_pos + dir * dist * 0.38).extend(0.);
            msg.write(GameMsg::Projectile(target, origin));
        }
    } else if stage == 4 && t >= 0.60 {
        flow.stage = 5;
        commands
            .spawn(Screenshot::primary_window())
            .observe(on_captured);
    }
}

fn on_captured(screenshot: On<ScreenshotCaptured>) {
    match screenshot.image.clone().try_into_dynamic() {
        Ok(dyn_img) => {
            if let Err(e) = dyn_img.to_rgb8().save(".github/assets/screenshot.png") {
                eprintln!("screenshot save failed: {e}");
            }
        }
        Err(e) => eprintln!("screenshot format error: {e}"),
    }
    std::process::exit(0);
}
