// Rockets shoot up and explode as bursts, rings or hearts

const ROCKET = "M0 -10 C4 -6 4 2 3 6 L5 10 L-5 10 L-3 6 C-4 2 -4 -6 0 -10 Z";
const COLORS = ["#ff4d6d", "#ffe066", "#5ce1e6", "#7b61ff", "#3ddc84", "#ff7ac8"];
const KINDS = ["burst", "ring", "heart"];

fn pick(list) {
    list[(rand() * list.len()).to_int()]
}

fn start(ctx) {
    ctx.gravity = ctx.height * 0.5;
    let rockets = 6;
    for i in 0..rockets {
        ctx.emit(#{
            x: ctx.width * (0.15 + 0.7 * i / (rockets - 1)),
            y: ctx.height,
            vx: (rand() - 0.5) * 80.0,
            vy: -ctx.height * (0.9 + rand() * 0.3),
            fuse: 0.8 + rand() * 0.7,
            shape: ROCKET,
            size: 28,
            color: "#dddddd",
            spin: false,
            tag: pick(KINDS),
        });
    }
}

fn expire(ctx, p) {
    let color = pick(COLORS);
    let sparks = ctx.count / 6;
    switch p.tag {
        "burst" => ctx.burst(p.x, p.y, sparks, color),
        "ring" => ring(ctx, p, sparks, color),
        "heart" => heart(ctx, p, sparks, color),
    }
}

fn ring(ctx, p, sparks, color) {
    let speed = ctx.height * 0.3;
    for i in 0..sparks {
        let angle = 2.0 * PI() * i / sparks;
        spark(ctx, p, angle.cos() * speed, angle.sin() * speed, color);
    }
}

// The classic parametric heart: x = 16 sin³t, y = 13 cos t - 5 cos 2t - 2 cos 3t - cos 4t.
fn heart(ctx, p, sparks, color) {
    let speed = ctx.height * 0.02;
    for i in 0..sparks {
        let t = 2.0 * PI() * i / sparks;
        let x = 16.0 * t.sin() ** 3;
        let y = 13.0 * t.cos() - 5.0 * (2.0 * t).cos() - 2.0 * (3.0 * t).cos() - (4.0 * t).cos();
        spark(ctx, p, x * speed, -y * speed, color);
    }
}

fn spark(ctx, p, vx, vy, color) {
    ctx.emit(#{ x: p.x, y: p.y, vx: vx, vy: vy, fuse: 1.4, shape: "circle", size: 6, color: color });
}
