// Blue NixOS snowflake confetti bursts from the bottom corners

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
    ctx.gravity = ctx.height * 0.65;
    ctx.drag = 0.25;
    for i in 0..ctx.count {
        let left = i % 2 == 0;
        let direction = if left { 1.0 } else { -1.0 };
        ctx.emit(#{
            x: if left { ctx.width * 0.05 } else { ctx.width * 0.95 },
            y: ctx.height,
            vx: direction * ctx.width * (0.12 + rand() * 0.28),
            vy: -ctx.height * (0.9 + rand() * 0.45),
            fuse: 4.0 + rand() * 1.5,
            shape: NIXOS,
            size: pick(SIZES),
            color: pick(COLORS),
            shadow: "#23324d",
            wobble: 24.0,
        });
    }
}
