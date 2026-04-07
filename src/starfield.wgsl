#import bevy_sprite::mesh2d_vertex_output::VertexOutput

@group(2) @binding(0)
var<uniform> seed: vec4<u32>;
@group(2) @binding(1)
var<uniform> color: vec4<f32>;

const CELL_SIZE: f32 = 100.0;
const RADIUS_MIN: f32 = 2.0;
const RADIUS_MAX: f32 = 5.0;
const DUST_SCALE: f32 = 900.0;

@group(2) @binding(2)
var<uniform> bg: vec4<f32>;
@group(2) @binding(3)
var<uniform> dust_a: vec4<f32>;
@group(2) @binding(4)
var<uniform> dust_b: vec4<f32>;

fn cell_hash(cx: i32, cy: i32) -> u32 {
    var h: u32 = u32(cx) * 2246822519u
        + u32(cy) * 2654435761u
        + seed.x;
    h ^= h >> 16u;
    h = h * 0x45d9f3bu;
    h ^= h >> 16u;
    return h;
}

fn dust_hash(cx: i32, cy: i32) -> f32 {
    return f32(cell_hash(cx, cy)) / 4294967295.0;
}

fn dust_noise(p: vec2<f32>) -> f32 {
    let i = vec2<i32>(floor(p));
    let f = fract(p);
    let u = f * f * (3.0 - 2.0 * f);
    return mix(
        mix(dust_hash(i.x, i.y), dust_hash(i.x + 1, i.y), u.x),
        mix(dust_hash(i.x, i.y + 1), dust_hash(i.x + 1, i.y + 1), u.x),
        u.y
    );
}

fn fbm(p: vec2<f32>) -> f32 {
    return dust_noise(p) * 0.5
        + dust_noise(p * 2.0 + vec2<f32>(5.2, 1.3)) * 0.25
        + dust_noise(p * 4.0 + vec2<f32>(3.7, 9.2)) * 0.125
        + dust_noise(p * 8.0 + vec2<f32>(8.1, 2.8)) * 0.0625;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let pos = in.world_position.xy;

    let n = fbm(pos / DUST_SCALE);
    let n2 = fbm(pos / DUST_SCALE * 1.5 + vec2<f32>(7.3, 4.1));
    let dust_color = mix(bg.rgb, mix(dust_a.rgb, dust_b.rgb, n2 * 0.4), smoothstep(0.5, 0.75, n) * 0.06);

    let base_cx = i32(floor(pos.x / CELL_SIZE));
    let base_cy = i32(floor(pos.y / CELL_SIZE));
    for (var dx: i32 = -1; dx <= 1; dx++) {
        for (var dy: i32 = -1; dy <= 1; dy++) {
            let cx = base_cx + dx;
            let cy = base_cy + dy;
            let hash = cell_hash(cx, cy);
            if (hash & 1u) != 0u {
                let offset_x = f32((hash >> 2u) & 0x3FFu) / 1023.0;
                let offset_y = f32((hash >> 12u) & 0x3FFu) / 1023.0;
                let offset_r = f32((hash >> 22u) & 0xFFu) / 255.0;
                let star_x = f32(cx) * CELL_SIZE + offset_x * CELL_SIZE;
                let star_y = f32(cy) * CELL_SIZE + offset_y * CELL_SIZE;
                let radius = RADIUS_MIN + offset_r * (RADIUS_MAX - RADIUS_MIN);
                let d = distance(pos, vec2<f32>(star_x, star_y));
                let glow_radius = radius * 2.5;
                if d < glow_radius {
                    let sigma = radius * 0.6;
                    let intensity = exp(-0.5 * d * d / (sigma * sigma));
                    let star_add = color.rgb * intensity * color.a * 2.0;
                    return vec4<f32>(clamp(dust_color + star_add, vec3<f32>(0.0), vec3<f32>(1.0)), 1.0);
                }
            }
        }
    }
    return vec4<f32>(dust_color, 1.0);
}
