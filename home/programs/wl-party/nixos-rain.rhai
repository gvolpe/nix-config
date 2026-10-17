// Blue NixOS snowflakes gently rain down from the top

// The six polygons of the NixOS logomark, combined into one filled SVG path.
const NIXOS = "M-624 249.415 L-496 27.713 L64 997.661 L-192 997.661 L-320 775.959 L-448 997.661 L-576 997.661 L-640 886.810 L-448 554.256 Z"
    + " M-528 -415.692 L-272 -415.692 L-832 554.256 L-960 332.554 L-832 110.851 L-1088 110.851 L-1152 0 L-1088 -110.851 L-704 -110.851 Z"
    + " M96 -665.108 L224 -443.405 L-896 -443.405 L-768 -665.108 L-512 -665.108 L-640 -886.810 L-576 -997.661 L-448 -997.661 L-256 -665.108 Z"
    + " M624 -249.415 L496 -27.713 L-64 -997.661 L192 -997.661 L320 -775.959 L448 -997.661 L576 -997.661 L640 -886.810 L448 -554.256 Z"
    + " M528 415.692 L272 415.692 L832 -554.256 L960 -332.554 L832 -110.851 L1088 -110.851 L1152 0 L1088 110.851 L704 110.851 Z"
    + " M-96 665.108 L-224 443.405 L896 443.405 L768 665.108 L512 665.108 L640 886.810 L576 997.661 L448 997.661 L256 665.108 Z";
const COLORS = ["#4d6fb7", "#77b6e1"];
const SIZES = [18, 24, 30, 36];

fn pick(list) {
    list[(rand() * list.len()).to_int()]
}

fn start(ctx) {
    // Match the native rain effect's physics and two-second release window.
    ctx.gravity = ctx.height * 0.6;
    ctx.drag = 1.8;
    let waves = if ctx.count < 40 { ctx.count } else { 40 };
    for i in 0..waves {
        let count = ctx.count / waves;
        if i < ctx.count % waves {
            count += 1;
        }
        // Offscreen timers release small batches without drawing placeholders.
        ctx.emit(#{
            y: -ctx.height * 2.0,
            size: 1,
            fuse: 2.0 * (i + 1) / waves,
            tag: count.to_string(),
        });
    }
}

fn expire(ctx, p) {
    let count = parse_int(p.tag);
    for i in 0..count {
        ctx.emit(#{
            x: rand() * ctx.width,
            y: -20.0,
            vx: (rand() - 0.5) * 120.0,
            vy: 60.0 + rand() * 120.0,
            shape: NIXOS,
            size: pick(SIZES),
            color: pick(COLORS),
            shadow: "#23324d",
        });
    }
}
