import * as THREE from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

// ---------------------------------------------------------------------------
// Config
// ---------------------------------------------------------------------------
const COL_ARG = new THREE.Color("#29b6ff"); // Argentina (attacks +x, right)
const COL_FRA = new THREE.Color("#ff2d55"); // France (attacks -x, left)

const PITCH_W = 68; // world units (long axis, x) — goal to goal
const PITCH_H = 44; // world units (short axis, z) — touchline to touchline

const MATCH_DURATION_S = 90; // seconds of real time to replay the whole match
const TRAIL_LEN = 40; // number of trail segments behind the ball

// ---------------------------------------------------------------------------
// Scene setup
// ---------------------------------------------------------------------------
const canvas = document.getElementById("scene");
const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: true });
renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));

const scene = new THREE.Scene();
scene.fog = new THREE.FogExp2(0x05010f, 0.006);

// Tilted broadcast-style camera: high and behind one corner, looking at center.
const camera = new THREE.PerspectiveCamera(42, 1, 0.1, 1000);
camera.position.set(0, 62, 58);

const controls = new OrbitControls(camera, canvas);
controls.enableDamping = true;
controls.dampingFactor = 0.08;
controls.minDistance = 45;
controls.maxDistance = 160;
controls.maxPolarAngle = Math.PI * 0.48;
controls.target.set(0, 0, 0);

scene.add(new THREE.AmbientLight(0xffffff, 0.7));
const key = new THREE.DirectionalLight(0xfff2e8, 0.65);
key.position.set(-20, 50, 20);
scene.add(key);

// ---------------------------------------------------------------------------
// Pitch: flat grass plane with baked mowing stripes + white markings.
// Argentina attacks toward +x (right goal), France toward -x (left goal).
// ---------------------------------------------------------------------------
function makePitchTexture() {
  const W = 1536;
  const H = 1024;
  const c = document.createElement("canvas");
  c.width = W;
  c.height = H;
  const g = c.getContext("2d");

  const stripes = 12;
  for (let i = 0; i < stripes; i++) {
    g.fillStyle = i % 2 === 0 ? "#0e6a2a" : "#0b5522";
    g.fillRect((i / stripes) * W, 0, W / stripes + 1, H);
  }

  g.strokeStyle = "rgba(255,255,255,0.95)";
  g.lineWidth = 5;
  g.lineJoin = "round";
  const m = 30;
  const iw = W - 2 * m;
  const ih = H - 2 * m;
  const Y = (ny) => m + ny * ih;

  g.strokeRect(m, m, iw, ih);
  g.beginPath();
  g.moveTo(W / 2, m);
  g.lineTo(W / 2, H - m);
  g.stroke();
  g.beginPath();
  g.arc(W / 2, H / 2, ih * 0.13, 0, Math.PI * 2);
  g.stroke();
  g.fillStyle = "rgba(255,255,255,0.95)";
  g.beginPath();
  g.arc(W / 2, H / 2, 6, 0, Math.PI * 2);
  g.fill();

  const drawEnd = (left) => {
    const dir = left ? 1 : -1;
    const edge = left ? m : W - m;
    const boxD = iw * 0.155;
    const sixD = iw * 0.055;
    g.beginPath();
    g.moveTo(edge, Y(0.19));
    g.lineTo(edge + dir * boxD, Y(0.19));
    g.lineTo(edge + dir * boxD, Y(0.81));
    g.lineTo(edge, Y(0.81));
    g.stroke();
    g.beginPath();
    g.moveTo(edge, Y(0.37));
    g.lineTo(edge + dir * sixD, Y(0.37));
    g.lineTo(edge + dir * sixD, Y(0.63));
    g.lineTo(edge, Y(0.63));
    g.stroke();
    const spotX = edge + dir * iw * 0.105;
    g.fillStyle = "rgba(255,255,255,0.95)";
    g.beginPath();
    g.arc(spotX, H / 2, 5, 0, Math.PI * 2);
    g.fill();
    g.beginPath();
    g.arc(spotX, H / 2, ih * 0.13, left ? -0.9 : Math.PI - 0.9,
      left ? 0.9 : Math.PI + 0.9);
    g.stroke();
  };
  drawEnd(true);
  drawEnd(false);

  const tex = new THREE.CanvasTexture(c);
  tex.anisotropy = 8;
  return tex;
}

const pitchGeo = new THREE.PlaneGeometry(PITCH_W, PITCH_H);
pitchGeo.rotateX(-Math.PI / 2);
const pitch = new THREE.Mesh(
  pitchGeo,
  new THREE.MeshBasicMaterial({ map: makePitchTexture() })
);
scene.add(pitch);

// Team half tints so it's obvious which side belongs to whom.
function makeHalf(color, sign) {
  const geo = new THREE.PlaneGeometry(PITCH_W / 2, PITCH_H);
  geo.rotateX(-Math.PI / 2);
  const mat = new THREE.MeshBasicMaterial({
    color,
    transparent: true,
    opacity: 0.16,
    blending: THREE.AdditiveBlending,
    depthWrite: false,
  });
  const m = new THREE.Mesh(geo, mat);
  m.position.set((sign * PITCH_W) / 4, 0.02, 0);
  return m;
}
scene.add(makeHalf(COL_FRA, -1)); // left half = France attacking end is right... tint = defending team
scene.add(makeHalf(COL_ARG, 1));

// Goal frames at each end (which team scores where).
function makeGoal(sign, color) {
  const grp = new THREE.Group();
  const mat = new THREE.MeshBasicMaterial({ color });
  const postH = 5;
  const goalW = 11;
  const gx = (sign * PITCH_W) / 2 - sign * 0.6; // pull just inside the end line
  const bar = new THREE.Mesh(new THREE.BoxGeometry(0.4, 0.4, goalW), mat);
  bar.position.set(gx, postH, 0);
  const p1 = new THREE.Mesh(new THREE.BoxGeometry(0.4, postH, 0.4), mat);
  p1.position.set(gx, postH / 2, goalW / 2);
  const p2 = p1.clone();
  p2.position.z = -goalW / 2;
  grp.add(bar, p1, p2);
  return grp;
}
scene.add(makeGoal(1, COL_ARG)); // right goal = where ARG attacks
scene.add(makeGoal(-1, COL_FRA)); // left goal = where FRA attacks

// world coord helpers (normalized pitch xy 0..1 -> world XZ)
function toWorld(nx, ny) {
  return { x: (nx - 0.5) * PITCH_W, z: (ny - 0.5) * PITCH_H };
}

// ---------------------------------------------------------------------------
// The ball + trail
// ---------------------------------------------------------------------------
const ball = new THREE.Mesh(
  new THREE.SphereGeometry(0.9, 24, 24),
  new THREE.MeshBasicMaterial({ color: 0xffffff })
);
ball.position.y = 1;
scene.add(ball);

// glow halo around the ball, colored by possession
const halo = new THREE.Mesh(
  new THREE.SphereGeometry(1.7, 24, 24),
  new THREE.MeshBasicMaterial({
    color: COL_ARG,
    transparent: true,
    opacity: 0.4,
    blending: THREE.AdditiveBlending,
    depthWrite: false,
  })
);
scene.add(halo);

// trail: a set of fading segments
const trailGroup = new THREE.Group();
scene.add(trailGroup);
const trail = [];
for (let i = 0; i < TRAIL_LEN; i++) {
  const m = new THREE.Mesh(
    new THREE.SphereGeometry(0.55, 12, 12),
    new THREE.MeshBasicMaterial({
      color: COL_ARG,
      transparent: true,
      opacity: 0,
      depthWrite: false,
    })
  );
  m.visible = false;
  trailGroup.add(m);
  trail.push(m);
}
let trailHead = 0;

// ball-carrier name label (single, follows the ball)
const carrierEl = document.getElementById("carrier");

// ---------------------------------------------------------------------------
// Goal bursts
// ---------------------------------------------------------------------------
const goalGroup = new THREE.Group();
scene.add(goalGroup);
const activeGoals = [];

function spawnGoalBurst(nx, ny, isHome) {
  const { x, z } = toWorld(nx, ny);
  const col = isHome ? COL_ARG : COL_FRA;
  const ring = new THREE.Mesh(
    new THREE.RingGeometry(0.4, 0.9, 48),
    new THREE.MeshBasicMaterial({
      color: col,
      transparent: true,
      opacity: 1,
      side: THREE.DoubleSide,
      depthWrite: false,
    })
  );
  ring.rotation.x = -Math.PI / 2;
  ring.position.set(x, 0.4, z);
  goalGroup.add(ring);
  const pillar = new THREE.Mesh(
    new THREE.CylinderGeometry(0.5, 0.5, 1, 16),
    new THREE.MeshBasicMaterial({
      color: col,
      transparent: true,
      opacity: 0.9,
      blending: THREE.AdditiveBlending,
      depthWrite: false,
    })
  );
  pillar.position.set(x, 0.5, z);
  goalGroup.add(pillar);
  activeGoals.push({ ring, pillar, life: 1 });
}

// ---------------------------------------------------------------------------
// Data + timeline
// ---------------------------------------------------------------------------
let MATCH = null;
let track = [];
let goals = [];
let matchEnd = 90 * 60;

const scoreHEl = document.getElementById("scoreH");
const scoreAEl = document.getElementById("scoreA");
const minuteEl = document.getElementById("minute");
const phaseEl = document.getElementById("phase");
const goalsListEl = document.getElementById("goalsList");
const compLabel = document.getElementById("compLabel");
const possFillArg = document.getElementById("possFillArg");
const possFillFra = document.getElementById("possFillFra");
const possPctArg = document.getElementById("possPctArg");
const possPctFra = document.getElementById("possPctFra");

let startTime = 0;
let ptr = 0;
let goalPtr = 0;
let scoreH = 0;
let scoreA = 0;
let possWindow = 0.5; // smoothed fraction of possession held by ARG

async function loadData() {
  const res = await fetch("./data/match.json");
  MATCH = await res.json();
  track = MATCH.track;
  goals = MATCH.goals;
  matchEnd = MATCH.meta.match_end_seconds;
  compLabel.textContent = `${MATCH.meta.competition} — ${titleCase(MATCH.meta.stage)}`;
  document.getElementById("loading").style.display = "none";
  reset();
}

function titleCase(s) {
  return s.charAt(0) + s.slice(1).toLowerCase();
}

function reset() {
  startTime = performance.now();
  ptr = 0;
  goalPtr = 0;
  scoreH = 0;
  scoreA = 0;
  possWindow = 0.5;
  trailHead = 0;
  goalsListEl.innerHTML = "";
  trail.forEach((t) => {
    t.visible = false;
    t.material.opacity = 0;
  });
  while (goalGroup.children.length) goalGroup.remove(goalGroup.children[0]);
  activeGoals.length = 0;
  scoreHEl.textContent = "0";
  scoreAEl.textContent = "0";
}

document.getElementById("replayBtn").addEventListener("click", reset);

// ---------------------------------------------------------------------------
// Ball position: find the active track segment for the current match clock and
// interpolate start->end so the ball physically travels across the pitch.
// ---------------------------------------------------------------------------
function ballStateAt(clock) {
  // advance ptr to the last segment whose start time <= clock
  while (ptr < track.length - 1 && track[ptr + 1].t <= clock) ptr++;
  const seg = track[ptr];
  const next = track[ptr + 1];
  // segment travel duration: prefer its own duration, else gap to next event
  let dur = seg.d;
  if (!dur || dur <= 0) {
    dur = next ? Math.max(0.15, next.t - seg.t) : 0.4;
  }
  dur = Math.min(dur, 3); // clamp long idle gaps
  const f = Math.max(0, Math.min(1, (clock - seg.t) / dur));
  const nx = seg.x + (seg.ex - seg.x) * f;
  const ny = seg.y + (seg.ey - seg.y) * f;
  return { nx, ny, isHome: seg.h === 1, player: seg.p, act: seg.a };
}

function pushTrail(x, z, isHome) {
  const t = trail[trailHead % TRAIL_LEN];
  t.position.set(x, 0.6, z);
  t.material.color.copy(isHome ? COL_ARG : COL_FRA);
  t.material.opacity = 0.9;
  t.visible = true;
  trailHead++;
}

function addGoalTicker(goal) {
  const el = document.createElement("div");
  el.className = "goal-item";
  const isHome = goal.team === MATCH.meta.home;
  el.innerHTML =
    `<span class="goal-dot ${isHome ? "arg" : "fra"}"></span>` +
    `${goal.player} ${goal.minute}&prime;`;
  goalsListEl.appendChild(el);
  requestAnimationFrame(() => el.classList.add("show"));
}

// ---------------------------------------------------------------------------
// Animation loop
// ---------------------------------------------------------------------------
let lastTrailAt = 0;
let lastNX = 0.5;
let lastNY = 0.5;

function tick(now) {
  requestAnimationFrame(tick);
  if (!MATCH) {
    renderer.render(scene, camera);
    return;
  }

  const elapsed = (now - startTime) / 1000;
  const progress = Math.min(1, elapsed / MATCH_DURATION_S);
  const matchClock = progress * matchEnd;

  const st = ballStateAt(matchClock);
  const { x, z } = toWorld(st.nx, st.ny);
  ball.position.set(x, 1, z);
  halo.position.set(x, 1, z);
  const col = st.isHome ? COL_ARG : COL_FRA;
  halo.material.color.copy(col);

  // possession bar: nudge smoothed window toward current holder
  possWindow += ((st.isHome ? 1 : 0) - possWindow) * 0.02;
  const argPct = Math.round(possWindow * 100);
  possFillArg.style.width = argPct + "%";
  possFillFra.style.width = 100 - argPct + "%";
  possPctArg.textContent = argPct + "%";
  possPctFra.textContent = 100 - argPct + "%";

  // ball carrier name floating label (HTML, projected to screen)
  if (st.player) {
    carrierEl.textContent = st.player;
    carrierEl.style.color = st.isHome ? "#8fd6ff" : "#ff8fa3";
    const v = new THREE.Vector3(x, 3.2, z).project(camera);
    const sx = (v.x * 0.5 + 0.5) * renderer.domElement.clientWidth;
    const sy = (-v.y * 0.5 + 0.5) * renderer.domElement.clientHeight;
    carrierEl.style.transform = `translate(-50%,-100%) translate(${sx}px,${sy}px)`;
    carrierEl.style.opacity = v.z < 1 ? "1" : "0";
  }

  // lay a trail crumb when the ball has moved enough
  const dx = st.nx - lastNX;
  const dy = st.ny - lastNY;
  if (dx * dx + dy * dy > 0.00012 || matchClock - lastTrailAt > 0.15) {
    pushTrail(x, z, st.isHome);
    lastTrailAt = matchClock;
    lastNX = st.nx;
    lastNY = st.ny;
  }

  // fade the whole trail
  for (const t of trail) {
    if (!t.visible) continue;
    t.material.opacity *= 0.965;
    t.scale.multiplyScalar(0.992);
    if (t.material.opacity < 0.02) {
      t.visible = false;
      t.scale.setScalar(1);
    }
  }

  // goals
  while (goalPtr < goals.length && goals[goalPtr].t <= matchClock) {
    const g = goals[goalPtr];
    const isHome = g.team === MATCH.meta.home;
    spawnGoalBurst(g.x, g.y, isHome);
    if (isHome) scoreH++;
    else scoreA++;
    scoreHEl.textContent = scoreH;
    scoreAEl.textContent = scoreA;
    addGoalTicker(g);
    goalPtr++;
  }

  for (let i = activeGoals.length - 1; i >= 0; i--) {
    const g = activeGoals[i];
    g.life -= 0.01;
    const s = 1 + (1 - g.life) * 16;
    g.ring.scale.setScalar(s);
    g.ring.material.opacity = Math.max(0, g.life);
    g.pillar.scale.y = 1 + (1 - g.life) * 22;
    g.pillar.position.y = g.pillar.scale.y * 0.5;
    g.pillar.material.opacity = Math.max(0, g.life * 0.9);
    if (g.life <= 0) {
      goalGroup.remove(g.ring);
      goalGroup.remove(g.pillar);
      activeGoals.splice(i, 1);
    }
  }

  const clockMin = Math.floor(matchClock / 60);
  minuteEl.textContent = clockMin;
  phaseEl.textContent =
    clockMin < 45 ? "1st Half" :
    clockMin < 90 ? "2nd Half" :
    clockMin < 105 ? "Extra Time 1" : "Extra Time 2";

  // gentle pulse on ball
  const pulse = 1 + Math.sin(now * 0.006) * 0.12;
  halo.scale.setScalar(pulse);

  controls.update();
  renderer.render(scene, camera);
}

function resize() {
  const stage = document.getElementById("stage");
  const w = stage.clientWidth;
  const h = stage.clientHeight;
  renderer.setSize(w, h, false);
  camera.aspect = w / h;
  camera.updateProjectionMatrix();
}
window.addEventListener("resize", resize);
resize();

loadData();
requestAnimationFrame(tick);
