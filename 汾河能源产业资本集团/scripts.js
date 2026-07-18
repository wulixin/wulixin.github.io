/* ============================================================
   汾河能源产业资本集团 · 交互与动效
   ============================================================ */
(function () {
    'use strict';
    const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    /* ---------- 行情条无缝滚动：复制内容 ---------- */
    const ticker = document.getElementById('tickerTrack');
    if (ticker) {
        ticker.innerHTML += ticker.innerHTML;
    }

    /* ---------- 导航：滚动状态 + 移动端菜单 ---------- */
    const nav = document.getElementById('nav');
    const navToggle = document.getElementById('navToggle');
    const navLinks = document.getElementById('navLinks');

    window.addEventListener('scroll', function () {
        if (window.scrollY > 40) nav.classList.add('scrolled');
        else nav.classList.remove('scrolled');
    }, { passive: true });

    if (navToggle && navLinks) {
        navToggle.addEventListener('click', function () {
            navLinks.classList.toggle('open');
        });
        navLinks.querySelectorAll('a').forEach(function (a) {
            a.addEventListener('click', function () { navLinks.classList.remove('open'); });
        });
    }

    /* ---------- 滚动进度条 ---------- */
    const progress = document.getElementById('scrollProgress');
    window.addEventListener('scroll', function () {
        const h = document.documentElement.scrollHeight - document.documentElement.clientHeight;
        const sc = (window.scrollY / h) * 100;
        if (progress) progress.style.width = sc + '%';
    }, { passive: true });

    /* ---------- 滚动揭示动画 ---------- */
    const reveals = document.querySelectorAll('.reveal');
    if ('IntersectionObserver' in window && !reduceMotion) {
        const io = new IntersectionObserver(function (entries) {
            entries.forEach(function (e) {
                if (e.isIntersecting) { e.target.classList.add('visible'); io.unobserve(e.target); }
            });
        }, { threshold: 0.12 });
        reveals.forEach(function (el) { io.observe(el); });
    } else {
        reveals.forEach(function (el) { el.classList.add('visible'); });
    }

    /* ---------- 导航高亮当前区块 ---------- */
    const sections = document.querySelectorAll('main section[id]');
    const linkMap = {};
    document.querySelectorAll('.nav-links a').forEach(function (a) {
        const id = a.getAttribute('href').replace('#', '');
        linkMap[id] = a;
    });
    if ('IntersectionObserver' in window) {
        const navIO = new IntersectionObserver(function (entries) {
            entries.forEach(function (e) {
                if (e.isIntersecting) {
                    Object.values(linkMap).forEach(function (l) { l.classList.remove('active'); });
                    if (linkMap[e.target.id]) linkMap[e.target.id].classList.add('active');
                }
            });
        }, { rootMargin: '-45% 0px -50% 0px' });
        sections.forEach(function (s) { navIO.observe(s); });
    }

    /* ---------- 数字滚动计数 ---------- */
    function animateCount(el) {
        const target = parseFloat(el.getAttribute('data-target')) || 0;
        const suffix = el.getAttribute('data-suffix') || '';
        const dur = 1600;
        const start = performance.now();
        function step(now) {
            const p = Math.min((now - start) / dur, 1);
            const eased = 1 - Math.pow(1 - p, 3);
            const val = Math.round(target * eased);
            el.textContent = val.toLocaleString('en-US') + suffix;
            if (p < 1) requestAnimationFrame(step);
        }
        requestAnimationFrame(step);
    }
    const counters = document.querySelectorAll('.hm-num');
    if ('IntersectionObserver' in window && !reduceMotion) {
        const cIO = new IntersectionObserver(function (entries) {
            entries.forEach(function (e) {
                if (e.isIntersecting) { animateCount(e.target); cIO.unobserve(e.target); }
            });
        }, { threshold: 0.5 });
        counters.forEach(function (c) { cIO.observe(c); });
    } else {
        counters.forEach(function (c) {
            const t = c.getAttribute('data-target'); const s = c.getAttribute('data-suffix') || '';
            c.textContent = Number(t).toLocaleString('en-US') + s;
        });
    }

    /* ---------- 四象限交互：悬停更新核心标签 ---------- */
    const quad = document.getElementById('quad');
    if (quad) {
        const core = quad.querySelector('.quad-core');
        const orig = core.textContent;
        quad.querySelectorAll('.quad-cell').forEach(function (cell) {
            cell.addEventListener('mouseenter', function () { core.textContent = cell.getAttribute('data-label'); });
            cell.addEventListener('mouseleave', function () { core.textContent = orig; });
        });
    }

    /* ---------- 留言表单：前端提示 ---------- */
    const form = document.getElementById('contactForm');
    if (form) {
        form.addEventListener('submit', function (e) {
            e.preventDefault();
            const btn = form.querySelector('.submit-btn');
            const name = form.querySelector('#name').value.trim();
            btn.textContent = '已收到，' + (name || '朋友') + '！我们会尽快联系您 ✓';
            btn.style.background = 'linear-gradient(135deg,#34d399,#2dd4bf)';
            setTimeout(function () {
                btn.textContent = '发送留言';
                btn.style.background = '';
                form.reset();
            }, 3200);
        });
    }

    /* ---------- 英雄区 · 高级数据流粒子网络 ---------- */
    const canvas = document.getElementById('heroCanvas');
    if (canvas && !reduceMotion) {
        const ctx = canvas.getContext('2d');
        let w, h, dpr, raf = null, particles = [], pulses = [], edges = [];
        const mouse = { x: -9999, y: -9999, tx: 0, ty: 0 };
        let cpx = 0, cpy = 0;                 // 平滑视差
        const LINK = 132;                     // 连线距离阈值
        const palette = ['45,212,191', '56,189,248', '167,139,250']; // 青 / 蓝 / 紫

        function resize() {
            dpr = Math.min(window.devicePixelRatio || 1, 2);
            w = canvas.clientWidth; h = canvas.clientHeight;
            canvas.width = w * dpr; canvas.height = h * dpr;
            ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
            init();
        }
        function init() {
            const count = Math.min(Math.floor((w * h) / 11500), 130);
            particles = [];
            for (let i = 0; i < count; i++) {
                const depth = Math.random();               // 0 远 → 1 近，制造纵深
                particles.push({
                    x: Math.random() * w, y: Math.random() * h,
                    vx: (Math.random() - 0.5) * (0.16 + depth * 0.32),
                    vy: (Math.random() - 0.5) * (0.16 + depth * 0.32),
                    r: 0.7 + depth * 2.3,
                    depth: depth,
                    col: palette[(Math.random() * palette.length) | 0],
                    ph: Math.random() * Math.PI * 2          // 脉动相位
                });
            }
            pulses = [];
        }
        function buildEdges() {
            edges.length = 0;
            for (let i = 0; i < particles.length; i++) {
                for (let j = i + 1; j < particles.length; j++) {
                    const dx = particles[i].x - particles[j].x;
                    const dy = particles[i].y - particles[j].y;
                    const d = Math.sqrt(dx * dx + dy * dy);
                    if (d < LINK) edges.push({ a: i, b: j });
                }
            }
        }
        function spawnPulse() {
            if (!edges.length || pulses.length > 48) return;
            const e = edges[(Math.random() * edges.length) | 0];
            pulses.push({ a: e.a, b: e.b, t: 0, sp: 0.006 + Math.random() * 0.013, col: particles[e.a].col });
        }

        function draw() {
            ctx.clearRect(0, 0, w, h);
            ctx.globalCompositeOperation = 'lighter';      // 加色混合作辉光

            // 平滑视差
            cpx += (mouse.tx - cpx) * 0.05;
            cpy += (mouse.ty - cpy) * 0.05;

            // 更新节点位置
            for (let i = 0; i < particles.length; i++) {
                const p = particles[i];
                p.x += p.vx; p.y += p.vy;
                if (p.x < -24) p.x = w + 24; else if (p.x > w + 24) p.x = -24;
                if (p.y < -24) p.y = h + 24; else if (p.y > h + 24) p.y = -24;
                p.ph += 0.02;
            }

            // 构建连线并绘制
            buildEdges();
            for (let k = 0; k < edges.length; k++) {
                const e = edges[k];
                const A = particles[e.a], B = particles[e.b];
                const dx = A.x - B.x, dy = A.y - B.y;
                const dist = Math.sqrt(dx * dx + dy * dy);
                if (dist > LINK) continue;
                const alpha = (1 - dist / LINK) * 0.30;
                const grad = ctx.createLinearGradient(A.x + cpx, A.y + cpy, B.x + cpx, B.y + cpy);
                grad.addColorStop(0, 'rgba(' + A.col + ',' + alpha + ')');
                grad.addColorStop(1, 'rgba(' + B.col + ',' + alpha + ')');
                ctx.strokeStyle = grad;
                ctx.lineWidth = 0.5 + (A.depth + B.depth) * 0.28;
                ctx.beginPath();
                ctx.moveTo(A.x + cpx, A.y + cpy);
                ctx.lineTo(B.x + cpx, B.y + cpy);
                ctx.stroke();
            }

            // 节点（脉动 + 外晕）
            for (let i = 0; i < particles.length; i++) {
                const p = particles[i];
                const pr = p.r * (1 + Math.sin(p.ph) * 0.18);
                ctx.beginPath();
                ctx.arc(p.x + cpx, p.y + cpy, pr, 0, Math.PI * 2);
                ctx.fillStyle = 'rgba(' + p.col + ',0.92)';
                ctx.fill();
                ctx.beginPath();
                ctx.arc(p.x + cpx, p.y + cpy, pr * 3.4, 0, Math.PI * 2);
                ctx.fillStyle = 'rgba(' + p.col + ',0.05)';
                ctx.fill();
            }

            // 数据流光点：沿连线流动
            for (let k = pulses.length - 1; k >= 0; k--) {
                const pu = pulses[k];
                pu.t += pu.sp;
                if (pu.t >= 1) { pulses.splice(k, 1); continue; }
                const A = particles[pu.a], B = particles[pu.b];
                const x = A.x + (B.x - A.x) * pu.t + cpx;
                const y = A.y + (B.y - A.y) * pu.t + cpy;
                const fade = Math.sin(pu.t * Math.PI);       // 两端淡出
                ctx.beginPath();
                ctx.arc(x, y, 2.6, 0, Math.PI * 2);
                ctx.fillStyle = 'rgba(' + pu.col + ',' + (0.95 * fade) + ')';
                ctx.fill();
                ctx.beginPath();
                ctx.arc(x, y, 8, 0, Math.PI * 2);
                ctx.fillStyle = 'rgba(' + pu.col + ',' + (0.14 * fade) + ')';
                ctx.fill();
            }

            ctx.globalCompositeOperation = 'source-over';
            if (Math.random() < 0.55) spawnPulse();          // 持续生成数据流
            raf = requestAnimationFrame(draw);
        }

        window.addEventListener('mousemove', function (e) {
            const rect = canvas.getBoundingClientRect();
            const mx = e.clientX - rect.left, my = e.clientY - rect.top;
            mouse.x = mx; mouse.y = my;
            mouse.tx = (mx - w / 2) * 0.012;                  // 视差目标
            mouse.ty = (my - h / 2) * 0.012;
        });
        window.addEventListener('mouseleave', function () { mouse.x = -9999; mouse.y = -9999; mouse.tx = 0; mouse.ty = 0; });
        window.addEventListener('resize', resize);
        resize();
        draw();

        // 离开英雄区时暂停以省资源
        const heroSec = document.querySelector('.hero');
        if (heroSec && 'IntersectionObserver' in window) {
            const hIO = new IntersectionObserver(function (entries) {
                entries.forEach(function (e) {
                    if (e.isIntersecting) { if (!raf) draw(); }
                    else { cancelAnimationFrame(raf); raf = null; }
                });
            }, { threshold: 0 });
            hIO.observe(heroSec);
        }
    }
})();
